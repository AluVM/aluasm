// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

//! Compiler converting AST constructed by analyzer into instructions and library data structure

use std::collections::BTreeMap;
use std::convert::{TryFrom, TryInto};
use std::fs::File;
use std::io::Write as IoWrite;
use std::str::FromStr;

use aluvm::data::{ByteStr, MaybeNumber, Step};
use aluvm::isa::{
    ArithmeticOp, BitwiseOp, Bytecode, CmpOp, ControlFlowOp, DigestOp, Flag, Instr, MoveOp,
    ParseFlagError, PutOp, ReservedOp, Secp256k1Op,
};
use aluvm::libs::{Cursor, LibId, LibSeg, LibSite, Read, Write};
use aluvm::reg::{NumericRegister, Reg32, RegAF, RegAFR, RegAR, RegAll, RegR, Register};
use aluvm::Isa;
use amplify::num::u1024;
use pest::Span;
use rustc_apfloat::ieee;

use crate::ast::{Const, FlagSet, Literal, Operand, Operator, Program, Routine, Statement};
use crate::issues::{self, CompileError, Issues};
use crate::module::{Module, Symbols};
use crate::CompilerError;

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display, Error, From)]
#[display(doc_comments)]
pub enum LibError {
    /// library `{0}` containing routine `{1}` is not found
    LibNotFound(LibId, String),

    /// number of external routine calls exceeds maximal number of jumps allowed by VM's `cy0`
    TooManyRoutines,
}

pub struct LibFrame<'a, 'b> {
    seg: &'a LibSeg,
    sym: &'b mut Vec<String>,
}

impl<'a, 'b> LibFrame<'a, 'b> {
    #[inline]
    pub fn with(seg: &'a LibSeg, sym: &'b mut Vec<String>) -> Self { LibFrame { seg, sym } }

    pub fn find_or_insert_ext(
        &mut self,
        id: LibId,
        routine: &String,
    ) -> Result<(u8, u16), LibError> {
        let LibFrame { seg: libs, sym: routines } = self;
        let pos1 = libs.index(id).ok_or(LibError::LibNotFound(id, routine.clone()))?;
        if routines.len() >= u16::MAX as usize {
            return Err(LibError::TooManyRoutines);
        }
        let pos2 = routines.iter().position(|name| name == routine).unwrap_or_else(|| {
            routines.push(routine.clone());
            routines.len() - 1
        });
        Ok((pos1, pos2 as u16))
    }
}

impl<'i> Program<'i> {
    pub fn compile(
        &'i self,
        dump: &mut Option<File>,
    ) -> Result<(Module, Issues<'i, issues::Compile>), CompilerError> {
        let mut issues = Issues::default();

        let isae = self.isae.iter().map(Isa::to_string).collect::<Vec<_>>().join(" ");
        let libs_segment = LibSeg::with(self.libs.map.values().copied())
            .map_err(|err| issues.push_error(err.into(), &self.libs.span))
            .unwrap_or_default();
        let mut code_segment = ByteStr::default();
        let mut cursor = Cursor::new(&mut code_segment.bytes[..], &libs_segment);

        let mut externals = vec![];
        let mut frame = LibFrame::with(&libs_segment, &mut externals);

        let routine_map: BTreeMap<String, Vec<u16>> =
            self.routines.iter().try_fold(bmap! {}, |mut map, (name, routine)| {
                map.insert(
                    name.clone(),
                    routine.compile(&mut cursor, self, &mut frame, dump, &mut issues)?,
                );
                Ok(map)
            })?;

        let pos = cursor.pos();
        let data = cursor.into_data_segment();
        code_segment.adjust_len(pos);

        let mut cursor = Cursor::with(&mut code_segment, data, &libs_segment);

        for routine in self.routines.values() {
            let map = routine_map
                .get(&routine.name)
                .ok_or_else(|| CompilerError::RoutineMissed(routine.name.clone()))?;
            let start =
                *map.first().ok_or_else(|| CompilerError::RoutineEmpty(routine.name.clone()))?;
            for (offset, statement) in routine.statements.iter().enumerate() {
                if statement.operator.0 == Operator::routine {
                    let (routine_name, span) = match statement.routine(0, &mut issues) {
                        Some(name) => name,
                        None => continue,
                    };
                    let posmap = match routine_map.get(&routine_name) {
                        Some(map) => map,
                        None => {
                            issues.push_error(CompileError::RoutineUnknown(routine_name), &span);
                            continue;
                        }
                    };
                    let pos =
                        *posmap.first().ok_or_else(|| CompilerError::RoutineEmpty(routine_name))?;
                    let seek = start + map[offset];
                    cursor.seek(Some(seek));
                    let mut instr = Instr::<ReservedOp>::read(&mut cursor)
                        .map_err(|_| CompilerError::InstrRead(seek))?;
                    let to = match instr {
                        Instr::ControlFlow(ControlFlowOp::Routine(ref mut to)) => to,
                        other => return Err(CompilerError::InstrChanged(seek, "routine", other)),
                    };
                    *to = pos;
                    cursor.seek(Some(seek));
                    instr.write(&mut cursor).map_err(|err| CompilerError::InstrWrite(seek, err))?;
                }
            }
        }

        // TODO: Collect inputs
        let input = vec![];
        /*
        let input = self.input.values().map(|v| {
            Input {
                name: v.name.clone(),
                details: v.info.clone(),
                data: match v {

                }
            }
        }).collect();
         */

        let routines = routine_map
            .iter()
            .filter_map(|(name, map)| Some((name.clone(), *map.first()?)))
            .collect();
        let symbols = Symbols { externals, routines };
        let data = cursor.into_data_segment();

        Ok((
            Module {
                isae,
                code: code_segment.to_vec(),
                data,
                libs: libs_segment,
                vars: input,
                symbols,
            },
            issues,
        ))
    }
}

impl<'i> Routine<'i> {
    pub fn compile(
        &'i self,
        cursor: &mut (impl Read + Write),
        program: &'i Program,
        frame: &mut LibFrame,
        dump: &mut Option<File>,
        issues: &mut Issues<'i, issues::Compile>,
    ) -> Result<Vec<u16>, CompilerError> {
        let mut instr_map = Vec::with_capacity(self.statements.len());
        let mut jump_map = bmap![];

        let mut do_dump = true;
        for (no, statement) in self.statements.iter().enumerate() {
            let pos = match cursor.pos() {
                Some(pos) => pos,
                None => {
                    issues.push_error(CompileError::CodeLengthOverflow, &statement.span);
                    break;
                }
            };
            instr_map.push(pos);
            let instr = statement.compile(program, frame, issues)?;
            if let Err(err) = instr.write(cursor) {
                issues.push_error(err.into(), &statement.span);
                break;
            }
            if matches!(statement.operator.0, Operator::jif | Operator::jmp) {
                let instr_no = statement
                    .goto(0, issues)
                    .and_then(|label| self.labels.get(&label).copied())
                    .unwrap_or(no as u16);
                jump_map.insert(pos, instr_no);
            }

            if do_dump {
                if let Some(df) = dump {
                    if let Err(err) = writeln!(
                        df,
                        "{:05}: {}\n    => {}",
                        instr_map.last().unwrap(),
                        statement.span.as_str().trim(),
                        instr
                    ) {
                        eprintln!("\x1B[1;31mError:\x1B can't write dump\ndetails:{}", err);
                        do_dump = false;
                    }
                }
            }
        }

        let end = cursor.pos();

        for (from, to) in jump_map {
            cursor.seek(Some(from));
            let mut instr =
                Instr::<ReservedOp>::read(cursor).map_err(|_| CompilerError::InstrRead(from))?;
            let pos = match instr {
                Instr::ControlFlow(ControlFlowOp::Jif(ref mut pos))
                | Instr::ControlFlow(ControlFlowOp::Jmp(ref mut pos)) => pos,
                other => return Err(CompilerError::InstrChanged(from, "jump", other)),
            };
            *pos = instr_map[to as usize];
            cursor.seek(Some(from));
            instr.write(cursor).map_err(|err| CompilerError::InstrWrite(from, err))?;
        }

        cursor.seek(end);

        Ok(instr_map)
    }
}

impl<'i> Statement<'i> {
    fn reg<T>(&'i self, no: u8, issues: &mut Issues<'i, issues::Compile>) -> T
    where
        T: TryFrom<RegAll> + Register,
    {
        let (operand, span) = self
            .operands
            .get(no as usize)
            .map(|op| match op {
                Operand::Reg { set, span, .. } => (*set, span),
                Operand::Goto(_, span)
                | Operand::Const(_, span)
                | Operand::Lit(_, span)
                | Operand::Call { span, .. } => {
                    issues.push_error(
                        CompileError::OperandWrongType {
                            operator: self.operator.0,
                            pos: no + 1,
                            expected: T::description(),
                        },
                        &span,
                    );
                    (RegAll::default(), span)
                }
            })
            .unwrap_or_else(|| {
                issues.push_error(
                    CompileError::OperandMissed {
                        operator: self.operator.0,
                        pos: no + 1,
                        expected: T::description(),
                    },
                    &self.span,
                );
                (RegAll::default(), &self.span)
            });
        operand.try_into().unwrap_or_else(|_| {
            issues.push_error(
                CompileError::OperandWrongReg {
                    operator: self.operator.0,
                    pos: no + 1,
                    expected: T::description(),
                },
                span,
            );
            T::default()
        })
    }

    fn idx<T>(&'i self, no: u8, issues: &mut Issues<'i, issues::Compile>) -> T
    where
        T: TryFrom<Reg32> + Register,
    {
        let (operand, span) = self
            .operands
            .get(no as usize)
            .map(|op| match op {
                Operand::Reg { index, span, .. } => (*index, span),
                Operand::Goto(_, span)
                | Operand::Const(_, span)
                | Operand::Lit(_, span)
                | Operand::Call { span, .. } => {
                    issues.push_error(
                        CompileError::OperandWrongType {
                            operator: self.operator.0,
                            pos: no + 1,
                            expected: T::description(),
                        },
                        span,
                    );
                    (Reg32::default(), span)
                }
            })
            .unwrap_or_else(|| {
                issues.push_error(
                    CompileError::OperandMissed {
                        operator: self.operator.0,
                        pos: no + 1,
                        expected: T::description(),
                    },
                    &self.operator.1,
                );
                (Reg32::default(), &self.operator.1)
            });
        operand.try_into().unwrap_or_else(|_| {
            issues.push_error(
                CompileError::OperandWrongReg {
                    operator: self.operator.0,
                    pos: no + 1,
                    expected: T::description(),
                },
                span,
            );
            T::default()
        })
    }

    fn flags<F>(&'i self, issues: &mut Issues<'i, issues::Compile>) -> F
    where
        WrappedFlag<F>: TryFrom<FlagSet<'i, char>, Error = FlagError<'i>>,
        F: Flag,
    {
        WrappedFlag::try_from(self.flags.clone())
            .map(|wrapper| wrapper.0)
            .map_err(|err| {
                issues.push_error(err.inner.into(), err.span.as_ref().unwrap_or(&self.operator.1));
            })
            .unwrap_or_default()
    }

    fn val(
        &'i self,
        no: u8,
        reg: impl NumericRegister,
        consts: &'i BTreeMap<String, Const<'i>>,
        issues: &mut Issues<'i, issues::Compile>,
    ) -> Result<MaybeNumber, CompilerError> {
        let operand = if let Some(operand) = self.operands.get(no as usize) {
            operand
        } else {
            issues.push_error(
                CompileError::OperandMissed {
                    operator: self.operator.0,
                    pos: no + 1,
                    expected: "constant or integer literal",
                },
                &self.operator.1,
            );
            return Ok(MaybeNumber::none());
        };

        let mut val = match operand {
            Operand::Lit(Literal::Int(val, _), _) => MaybeNumber::from(val),
            Operand::Lit(Literal::Float(i, r, e), _) => MaybeNumber::from(
                ieee::Quad::from_str(&format!("{}.{}e{}", i, r, e))
                    .map_err(|err| CompilerError::FloatConstruction(*i, *r, *e, err))?,
            ),
            Operand::Const(name, span) => {
                let val = match consts.get(name) {
                    Some(val) => val,
                    None => {
                        issues.push_error(CompileError::ConstUnknown(name.clone()), span);
                        return Ok(MaybeNumber::none());
                    }
                };
                match &val.value {
                    Literal::Int(val, _) => MaybeNumber::from(val),
                    Literal::Float(i, r, e) => MaybeNumber::from(
                        ieee::Quad::from_str(&format!("{}.{}e{}", i, r, e))
                            .map_err(|err| CompilerError::FloatConstruction(*i, *r, *e, err))?,
                    ),
                    lit => {
                        issues.push_error(
                            CompileError::ConstWrongType {
                                name: name.clone(),
                                expected: "integer or float",
                                found: lit.description(),
                            },
                            span,
                        );
                        return Ok(MaybeNumber::none());
                    }
                }
            }
            op => {
                issues.push_error(
                    CompileError::OperandWrongType {
                        operator: self.operator.0,
                        pos: no + 1,
                        expected: op.description(),
                    },
                    op.as_span(),
                );
                return Ok(MaybeNumber::none());
            }
        };
        val.reshape(reg.layout());
        Ok(val)
    }

    fn goto(&'i self, no: u8, issues: &mut Issues<'i, issues::Compile>) -> Option<String> {
        self.operands
            .get(no as usize)
            .and_then(|op| match op {
                Operand::Goto(goto, _) => Some(goto.clone()),
                Operand::Reg { span, .. }
                | Operand::Const(_, span)
                | Operand::Lit(_, span)
                | Operand::Call { span, .. } => {
                    issues.push_error(
                        CompileError::OperandWrongType {
                            operator: self.operator.0,
                            pos: no + 1,
                            expected: "goto statement",
                        },
                        span,
                    );
                    None
                }
            })
            .or_else(|| {
                issues.push_error(
                    CompileError::OperandMissed {
                        operator: self.operator.0,
                        pos: no + 1,
                        expected: "goto statement",
                    },
                    &self.span,
                );
                None
            })
    }

    fn routine(
        &'i self,
        no: u8,
        issues: &mut Issues<'i, issues::Compile>,
    ) -> Option<(String, Span)> {
        self.operands
            .get(no as usize)
            .and_then(|op| match op {
                Operand::Goto(goto, span) => Some((goto.clone(), span.clone())),
                Operand::Reg { span, .. }
                | Operand::Const(_, span)
                | Operand::Lit(_, span)
                | Operand::Call { span, .. } => {
                    issues.push_error(
                        CompileError::OperandWrongType {
                            operator: self.operator.0,
                            pos: no + 1,
                            expected: "routine call statement",
                        },
                        span,
                    );
                    None
                }
            })
            .or_else(|| {
                issues.push_error(
                    CompileError::OperandMissed {
                        operator: self.operator.0,
                        pos: no + 1,
                        expected: "routine call statement",
                    },
                    &self.span,
                );
                None
            })
    }

    fn lib(
        &'i self,
        no: u8,
        program: &'i Program,
        frame: &mut LibFrame,
        issues: &mut Issues<'i, issues::Compile>,
    ) -> LibSite {
        let operand = if let Some(operand) = self.operands.get(no as usize) {
            operand
        } else {
            issues.push_error(
                CompileError::OperandMissed {
                    operator: self.operator.0,
                    pos: no + 1,
                    expected: "external call statement",
                },
                &self.operator.1,
            );
            return LibSite::default();
        };

        match operand {
            Operand::Call { lib, routine, span } => {
                let id = program.libs.map.get(lib).copied().unwrap_or_else(|| {
                    issues.push_error(CompileError::LibUnknown(lib.clone()), span);
                    LibId::default()
                });
                match frame.find_or_insert_ext(id, routine) {
                    Ok((_, pos)) => return LibSite::with(pos, id),
                    Err(err) => {
                        issues.push_error(err.into(), span);
                        return LibSite::default();
                    }
                }
            }
            op => {
                issues.push_error(
                    CompileError::OperandWrongType {
                        operator: self.operator.0,
                        pos: no + 1,
                        expected: op.description(),
                    },
                    op.as_span(),
                );
                return LibSite::default();
            }
        }
    }

    pub fn compile(
        &'i self,
        program: &'i Program,
        frame: &mut LibFrame,
        issues: &mut Issues<'i, issues::Compile>,
    ) -> Result<Instr, CompilerError> {
        macro_rules! reg {
            ($no:expr) => {
                self.reg($no, issues)
            };
        }
        macro_rules! idx {
            ($no:expr) => {
                self.idx($no, issues)
            };
        }
        macro_rules! val {
            ($no:expr, $reg:ident) => {
                Box::new(self.val($no, $reg, &program.consts, issues)?)
            };
        }
        macro_rules! lib {
            ($no:expr) => {
                self.lib($no, program, frame, issues)
            };
        }
        macro_rules! flags {
            () => {
                self.flags(issues)
            };
        }
        Ok(match self.operator.0 {
            Operator::read => Instr::Nop,

            Operator::succ => Instr::ControlFlow(ControlFlowOp::Succ),
            Operator::fail => Instr::ControlFlow(ControlFlowOp::Fail),
            Operator::ret => Instr::ControlFlow(ControlFlowOp::Ret),
            Operator::jif => Instr::ControlFlow(ControlFlowOp::Jif(0)),
            Operator::jmp => Instr::ControlFlow(ControlFlowOp::Jmp(0)),
            Operator::routine => Instr::ControlFlow(ControlFlowOp::Routine(0)),
            Operator::exec => Instr::ControlFlow(ControlFlowOp::Exec(lib! {0})),
            Operator::call => Instr::ControlFlow(ControlFlowOp::Call(lib! {0})),

            // *** Put operations
            Operator::clr => match reg! {0} {
                RegAFR::A(a) => Instr::Put(PutOp::ClrA(a, idx! {0})),
                RegAFR::F(f) => Instr::Put(PutOp::ClrF(f, idx! {0})),
                RegAFR::R(r) => Instr::Put(PutOp::ClrR(r, idx! {0})),
            },
            Operator::put => match reg! {1} {
                RegAFR::A(a) => Instr::Put(PutOp::PutA(a, idx! {1}, val! {0, a})),
                RegAFR::F(f) => Instr::Put(PutOp::PutF(f, idx! {1}, val! {0, f})),
                RegAFR::R(r) => Instr::Put(PutOp::PutR(r, idx! {1}, val! {0, r})),
            },
            Operator::putif => match reg! {1} {
                RegAR::A(a) => Instr::Put(PutOp::PutIfA(a, idx! {1}, val! {0, a})),
                RegAR::R(r) => Instr::Put(PutOp::PutIfR(r, idx! {1}, val! {0, r})),
            },

            // *** Move operations
            Operator::dup => {
                let reg = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span(),
                    );
                }
                match reg {
                    RegAFR::A(a) => Instr::Move(MoveOp::DupA(a, idx! {0}, idx! {1})),
                    RegAFR::F(f) => Instr::Move(MoveOp::DupF(f, idx! {0}, idx! {1})),
                    RegAFR::R(r) => Instr::Move(MoveOp::DupR(r, idx! {0}, idx! {1})),
                }
            }
            Operator::mov => {
                let reg = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span(),
                    );
                }
                match reg {
                    RegAFR::A(a) => Instr::Move(MoveOp::MovA(a, idx! {0}, idx! {1})),
                    RegAFR::F(f) => Instr::Move(MoveOp::MovF(f, idx! {0}, idx! {1})),
                    RegAFR::R(r) => Instr::Move(MoveOp::MovR(r, idx! {0}, idx! {1})),
                }
            }
            Operator::cnv => match (reg! {0}, reg! {1}) {
                (RegAF::A(a1), RegAF::A(a2)) => {
                    Instr::Move(MoveOp::CnvA(a1, idx! {0}, a2, idx! {1}))
                }
                (RegAF::F(f1), RegAF::F(f2)) => {
                    Instr::Move(MoveOp::CnvF(f1, idx! {0}, f2, idx! {1}))
                }
                (RegAF::A(a), RegAF::F(f)) => Instr::Move(MoveOp::CnvAF(a, idx! {0}, f, idx! {1})),
                (RegAF::F(f), RegAF::A(a)) => Instr::Move(MoveOp::CnvFA(f, idx! {0}, a, idx! {1})),
            },
            Operator::cpy => match reg! {0} {
                RegAR::A(a) => Instr::Move(MoveOp::CpyA(a, idx! {0}, reg! {1}, idx! {1})),
                RegAR::R(r) => Instr::Move(MoveOp::CpyR(r, idx! {0}, reg! {1}, idx! {1})),
            },
            Operator::spy => Instr::Move(MoveOp::SpyAR(reg! {0}, idx! {0}, reg! {1}, idx! {1})),
            Operator::swp => {
                let reg = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span(),
                    );
                }
                match reg {
                    RegAF::A(a) => Instr::Move(MoveOp::SwpA(a, idx! {0}, idx! {1})),
                    RegAF::F(f) => Instr::Move(MoveOp::SwpF(f, idx! {0}, idx! {1})),
                }
            }

            // *** Comparison operations
            Operator::eq => {
                let reg = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span(),
                    );
                }
                match reg {
                    RegAFR::A(a) => Instr::Cmp(CmpOp::EqA(flags!(), a, idx! {0}, idx! {1})),
                    RegAFR::F(f) => Instr::Cmp(CmpOp::EqF(flags!(), f, idx! {0}, idx! {1})),
                    RegAFR::R(r) => Instr::Cmp(CmpOp::EqR(flags!(), r, idx! {0}, idx! {1})),
                }
            }
            Operator::gt => {
                let reg = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span(),
                    );
                }
                match reg {
                    RegAFR::A(a) => Instr::Cmp(CmpOp::GtA(flags!(), a, idx! {0}, idx! {1})),
                    RegAFR::F(f) => Instr::Cmp(CmpOp::GtF(flags!(), f, idx! {0}, idx! {1})),
                    RegAFR::R(r) => Instr::Cmp(CmpOp::GtR(r, idx! {0}, idx! {1})),
                }
            }
            Operator::lt => {
                let reg = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span(),
                    );
                }
                match reg {
                    RegAFR::A(a) => Instr::Cmp(CmpOp::LtA(flags!(), a, idx! {0}, idx! {1})),
                    RegAFR::F(f) => Instr::Cmp(CmpOp::LtF(flags!(), f, idx! {0}, idx! {1})),
                    RegAFR::R(r) => Instr::Cmp(CmpOp::LtR(r, idx! {0}, idx! {1})),
                }
            }
            Operator::ifn => match reg! {0} {
                RegAR::A(a) => Instr::Cmp(CmpOp::IfNA(a, idx! {0})),
                RegAR::R(r) => Instr::Cmp(CmpOp::IfNR(r, idx! {0})),
            },
            Operator::ifz => match reg! {0} {
                RegAR::A(a) => Instr::Cmp(CmpOp::IfZA(a, idx! {0})),
                RegAR::R(r) => Instr::Cmp(CmpOp::IfZR(r, idx! {0})),
            },
            Operator::stinv => Instr::Cmp(CmpOp::StInv),
            Operator::st => Instr::Cmp(CmpOp::St(flags!(), reg! {0}, idx! {0})),

            // *** Arithmetic
            Operator::neg => Instr::Arithmetic(ArithmeticOp::Neg(reg! {0}, idx! {0})),
            Operator::inc => {
                Instr::Arithmetic(ArithmeticOp::Stp(reg! {0}, idx! {0}, Step::with(1)))
            }
            Operator::dec => {
                Instr::Arithmetic(ArithmeticOp::Stp(reg! {0}, idx! {0}, Step::with(-1)))
            }
            Operator::add => {
                if let Some(Operand::Lit(Literal::Int(mut step, _), span)) = self.operands.get(0) {
                    if step > u1024::from(i16::MAX as u16) {
                        step = u1024::from(1u64);
                        issues.push_error(CompileError::StepTooLarge(self.operator.0), span);
                    }
                    Instr::Arithmetic(ArithmeticOp::Stp(
                        reg! {1},
                        idx! {1},
                        Step::with(step.low_u32() as i16),
                    ))
                } else {
                    let reg = reg! {0};
                    if reg != reg! {1} {
                        issues.push_error(
                            CompileError::OperandRegMutBeEqual(self.operator.0),
                            self.operands[2].as_span(),
                        );
                    }
                    match reg {
                        RegAF::A(a) => {
                            Instr::Arithmetic(ArithmeticOp::AddA(flags!(), a, idx! {0}, idx! {1}))
                        }
                        RegAF::F(f) => {
                            Instr::Arithmetic(ArithmeticOp::AddF(flags!(), f, idx! {0}, idx! {1}))
                        }
                    }
                }
            }
            Operator::sub => {
                if let Some(Operand::Lit(Literal::Int(mut step, _), span)) = self.operands.get(0) {
                    if step > u1024::from(i16::MAX as u16) {
                        step = u1024::from(1u64);
                        issues.push_error(CompileError::StepTooLarge(self.operator.0), span);
                    }
                    Instr::Arithmetic(ArithmeticOp::Stp(
                        reg! {1},
                        idx! {1},
                        Step::with(-(step.low_u32() as i16)),
                    ))
                } else {
                    let reg = reg! {0};
                    if reg != reg! {1} {
                        issues.push_error(
                            CompileError::OperandRegMutBeEqual(self.operator.0),
                            self.operands[2].as_span(),
                        );
                    }
                    match reg {
                        RegAF::A(a) => {
                            Instr::Arithmetic(ArithmeticOp::SubA(flags!(), a, idx! {0}, idx! {1}))
                        }
                        RegAF::F(f) => {
                            Instr::Arithmetic(ArithmeticOp::SubF(flags!(), f, idx! {0}, idx! {1}))
                        }
                    }
                }
            }
            Operator::mul => {
                let reg = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span(),
                    );
                }
                match reg {
                    RegAF::A(a) => {
                        Instr::Arithmetic(ArithmeticOp::MulA(flags!(), a, idx! {0}, idx! {1}))
                    }
                    RegAF::F(f) => {
                        Instr::Arithmetic(ArithmeticOp::MulF(flags!(), f, idx! {0}, idx! {1}))
                    }
                }
            }
            Operator::div => {
                let reg = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span(),
                    );
                }
                match reg {
                    RegAF::A(a) => {
                        Instr::Arithmetic(ArithmeticOp::DivA(flags!(), a, idx! {0}, idx! {1}))
                    }
                    RegAF::F(f) => {
                        Instr::Arithmetic(ArithmeticOp::DivF(flags!(), f, idx! {0}, idx! {1}))
                    }
                }
            }
            Operator::rem => {
                Instr::Arithmetic(ArithmeticOp::Rem(reg! {0}, idx! {0}, reg! {1}, idx! {1}))
            }
            Operator::abs => Instr::Arithmetic(ArithmeticOp::Abs(reg! {0}, idx! {0})),

            // *** Bitwise
            Operator::not => Instr::Bitwise(BitwiseOp::Not(reg! {0}, idx! {0})),
            Operator::and => {
                let reg: RegAR = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
                    );
                }
                if reg != reg! {2} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span(),
                    );
                }
                Instr::Bitwise(BitwiseOp::And(reg, idx! {0}, idx! {1}, idx! {2}))
            }
            Operator::or => {
                let reg: RegAR = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
                    );
                }
                if reg != reg! {2} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span(),
                    );
                }
                Instr::Bitwise(BitwiseOp::Or(reg, idx! {0}, idx! {1}, idx! {2}))
            }
            Operator::xor => {
                let reg: RegAR = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
                    );
                }
                if reg != reg! {2} {
                    issues.push_error(
                        CompileError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span(),
                    );
                }
                Instr::Bitwise(BitwiseOp::Xor(reg, idx! {0}, idx! {1}, idx! {2}))
            }
            Operator::shl => Instr::Bitwise(BitwiseOp::Shl(reg! {0}, idx! {0}, reg! {1}, idx! {1})),
            Operator::shr => match reg! {1} {
                RegAR::A(a) => {
                    Instr::Bitwise(BitwiseOp::ShrA(flags!(), reg! {0}, idx! {0}, a, idx! {1}))
                }
                RegAR::R(r) => Instr::Bitwise(BitwiseOp::ShrR(reg! {0}, idx! {0}, r, idx! {1})),
            },
            Operator::scl => Instr::Bitwise(BitwiseOp::Scl(reg! {0}, idx! {0}, reg! {1}, idx! {1})),
            Operator::scr => Instr::Bitwise(BitwiseOp::Scr(reg! {0}, idx! {0}, reg! {1}, idx! {1})),
            Operator::rev => match reg! {0} {
                RegAR::A(a) => Instr::Bitwise(BitwiseOp::RevA(a, idx! {0})),
                RegAR::R(r) => Instr::Bitwise(BitwiseOp::RevR(r, idx! {0})),
            },

            // *** Digest operations
            Operator::ripemd => {
                let reg: RegR = reg! {1};
                if reg != RegR::R160 {
                    issues.push_error(
                        CompileError::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 2,
                            expected: "r160 register",
                        },
                        self.operands[1].as_span(),
                    );
                    return Ok(Instr::Nop);
                }
                Instr::Digest(DigestOp::Ripemd(idx! {0}, idx! {1}))
            }
            Operator::sha2 => match reg! {1} {
                RegR::R256 => Instr::Digest(DigestOp::Sha256(idx! {0}, idx! {1})),
                RegR::R512 => Instr::Digest(DigestOp::Sha512(idx! {0}, idx! {1})),
                _ => {
                    issues.push_error(
                        CompileError::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 2,
                            expected: "r256 or r512 registers",
                        },
                        self.operands[1].as_span(),
                    );
                    return Ok(Instr::Nop);
                }
            },

            // *** Elliptic curve operations
            Operator::secpgen => {
                for r in 0..=1 {
                    let reg: RegR = reg! {r};
                    if reg != [RegR::R256, RegR::R512][r as usize] {
                        issues.push_error(
                            CompileError::OperandWrongReg {
                                operator: self.operator.0,
                                pos: r + 1,
                                expected: if r == 0 { "r256 register" } else { "r512 register" },
                            },
                            self.operands[r as usize].as_span(),
                        );
                        return Ok(Instr::Nop);
                    }
                }
                Instr::Secp256k1(Secp256k1Op::Gen(idx! {0}, idx! {1}))
            }
            Operator::secpneg => {
                for r in 0..=1 {
                    let reg: RegR = reg! {r};
                    if reg != RegR::R512 {
                        issues.push_error(
                            CompileError::OperandWrongReg {
                                operator: self.operator.0,
                                pos: r + 1,
                                expected: "r512 register",
                            },
                            self.operands[r as usize].as_span(),
                        );
                        return Ok(Instr::Nop);
                    }
                }
                Instr::Secp256k1(Secp256k1Op::Neg(idx! {0}, idx! {1}))
            }
            Operator::secpadd => {
                for r in 0..=1 {
                    let reg: RegR = reg! {r};
                    if reg != RegR::R512 {
                        issues.push_error(
                            CompileError::OperandWrongReg {
                                operator: self.operator.0,
                                pos: r + 1,
                                expected: "r512 register",
                            },
                            self.operands[r as usize].as_span(),
                        );
                        return Ok(Instr::Nop);
                    }
                }
                Instr::Secp256k1(Secp256k1Op::Add(idx! {0}, idx! {1}))
            }
            Operator::secpmul => {
                for r in 1..=2 {
                    let reg: RegR = reg! {r};
                    if reg != RegR::R512 {
                        issues.push_error(
                            CompileError::OperandWrongReg {
                                operator: self.operator.0,
                                pos: r + 1,
                                expected: "r512 register",
                            },
                            self.operands[r as usize].as_span(),
                        );
                        return Ok(Instr::Nop);
                    }
                }
                Instr::Secp256k1(Secp256k1Op::Mul(reg! {0}, idx! {0}, idx! {1}, idx! {2}))
            }

            Operator::nop => Instr::Nop,
        })
    }
}

#[derive(Clone, Hash, Debug, Display, Error)]
#[display("{inner}")]
pub struct FlagError<'i> {
    pub inner: ParseFlagError,
    pub span: Option<Span<'i>>,
}

pub struct WrappedFlag<F>(pub F);

impl<'i, F> TryFrom<FlagSet<'i, char>> for WrappedFlag<F>
where
    F: Flag,
{
    type Error = FlagError<'i>;

    fn try_from(flags: FlagSet<'i, char>) -> Result<Self, Self::Error> {
        match flags {
            FlagSet::None => F::from_str("").map_err(|err| FlagError { inner: err, span: None }),
            FlagSet::One(flag, span) => F::from_str(&flag.to_string())
                .map_err(|err| FlagError { inner: err, span: Some(span) }),
            FlagSet::Double(flag1, flag2, span) => F::from_str(&format!("{}{}", flag1, flag2))
                .map_err(|err| FlagError { inner: err, span: Some(span) }),
        }
        .map(|flag| WrappedFlag(flag))
    }
}
