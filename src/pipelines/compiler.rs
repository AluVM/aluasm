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
    ArithmeticOp, BitwiseOp, Bytecode, BytesOp, CmpOp, ControlFlowOp, DigestOp, Flag, Instr,
    MoveOp, ParseFlagError, PutOp, Secp256k1Op,
};
use aluvm::library::{Cursor, IsaSeg, Lib, LibId, LibSeg, LibSite, Read, Write};
use aluvm::reg::{
    NumericRegister, Reg32, RegA, RegAF, RegAFR, RegAR, RegAll, RegR, RegS, Register,
};
use aluvm::Isa;
use amplify::num::apfloat::ieee;
use amplify::num::u1024;
use pest::Span;

use crate::ast::{
    Const, FlagSet, Literal, Operand, Operator, Program, Routine, Statement, Var, VarType,
};
use crate::issues::{self, Issues, SemanticError};
use crate::module::{CallTable, DataType, Module, Variable};
use crate::{CompilerError, InstrError};

impl<'i> Program<'i> {
    pub fn compile(
        &'i self,
        dump: &mut Option<File>,
    ) -> Result<(Module, Issues<'i, issues::Compile>), CompilerError> {
        let mut issues = Issues::default();

        let isae = IsaSeg::from_iter(self.isae.iter().map(Isa::to_string))?;
        let libs_segment = LibSeg::from_iter(self.libs.map.values().copied())
            .map_err(|err| issues.push_error(err.into(), &self.libs.span))
            .unwrap_or_default();
        let mut code_segment = ByteStr::default();
        let mut call_table = CallTable::default();
        let mut cursor = Cursor::new(&mut code_segment.bytes[..], &libs_segment);

        let routine_map: BTreeMap<String, Vec<u16>> = self.routines.iter().try_fold(
            bmap! {},
            |mut map, (name, routine)| -> Result<_, CompilerError> {
                let code =
                    routine.compile(&mut cursor, self, &mut call_table, dump, &mut issues)?;
                if map.len() > u16::MAX as usize {
                    issues.push_error(SemanticError::RoutinesOverflow, &routine.span);
                } else {
                    map.insert(name.clone(), code);
                }
                Ok(map)
            },
        )?;

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
                            issues.push_error(SemanticError::RoutineUnknown(routine_name), &span);
                            continue;
                        }
                    };
                    let pos =
                        *posmap.first().ok_or_else(|| CompilerError::RoutineEmpty(routine_name))?;

                    let seek = start + map[offset];
                    cursor
                        .edit(seek, |instr| match instr {
                            Instr::ControlFlow(ControlFlowOp::Routine(ref mut to)) => {
                                *to = pos;
                                Ok(())
                            }
                            other => Err(InstrError::Changed("routine", other.clone())),
                        })
                        .map_err(|err| CompilerError::with(err, seek))?;
                }
            }
        }

        let mut vars = vec![];
        for v in self.input.values() {
            vars.push(v.compile(&mut issues)?);
        }

        let exports = routine_map
            .iter()
            .filter_map(|(name, map)| Some((name.clone(), *map.first()?)))
            .collect();

        let data = cursor.into_data_segment();

        let lib = Lib { isae, code: code_segment, data, libs: libs_segment };

        Ok((Module { inner: lib, vars, imports: call_table, exports }, issues))
    }
}

impl<'i> Var<'i> {
    pub fn compile(
        &'i self,
        issues: &mut Issues<'i, issues::Compile>,
    ) -> Result<Variable, CompilerError> {
        let mut info = self.info.clone();
        if info.bytes().len() > u16::MAX as usize {
            issues.push_error(
                SemanticError::VarNameLongInfo(self.name.clone(), info.bytes().len()),
                &self.span,
            );
            info = String::from_utf8(info.as_bytes()[0..u16::MAX as usize].to_vec())
                .map_err(|err| {
                    issues.push_error(
                        SemanticError::VarInfoNotUtf8(self.name.clone(), err),
                        &self.span,
                    );
                })
                .unwrap_or_default();
        }

        let data = match (self.ty, &self.default) {
            (VarType::Int(layout), None) => DataType::Int(layout, MaybeNumber::none()),
            (VarType::Float(layout), None) => DataType::Float(layout, MaybeNumber::none()),
            (VarType::Bytes | VarType::Str, None) => DataType::ByteStr(None),
            (VarType::Int(layout), Some(Literal::Int(val, _))) => {
                let mut default = MaybeNumber::from(val);
                default.reshape(layout.into());
                DataType::Int(layout, default)
            }
            (VarType::Float(layout), Some(Literal::Float(m, r, e))) => {
                let float = ieee::Quad::from_str(&format!("{}.{}e{}", m, r, e))
                    .map_err(|err| CompilerError::FloatConstruction(*m, *r, *e, err))?;
                let mut default = MaybeNumber::from(float);
                default.reshape(layout.into());
                DataType::Float(layout, default)
            }
            (VarType::Bytes, Some(Literal::String(s)))
            | (VarType::Str, Some(Literal::String(s))) => {
                DataType::ByteStr(Some(s.as_bytes().to_vec()))
            }
            (VarType::Bytes, Some(Literal::Int(val, _))) => {
                DataType::ByteStr(Some(val.to_be_bytes().to_vec()))
            }
            (VarType::Str, Some(Literal::Int(val, _))) => DataType::ByteStr(Some(
                String::from_utf8(val.to_be_bytes().to_vec())
                    .map_err(|err| {
                        issues.push_error(
                            SemanticError::VarValueNotUtf8(self.name.clone(), err),
                            &self.span,
                        );
                    })
                    .unwrap_or_default()
                    .as_bytes()
                    .to_vec(),
            )),
            (VarType::Int(layout), _) => {
                issues.push_error(SemanticError::VarWrongDefault(self.name.clone()), &self.span);
                DataType::Int(layout, MaybeNumber::none())
            }
            (VarType::Float(layout), _) => {
                issues.push_error(SemanticError::VarWrongDefault(self.name.clone()), &self.span);
                DataType::Float(layout, MaybeNumber::none())
            }
            (VarType::Str | VarType::Bytes, _) => {
                issues.push_error(SemanticError::VarWrongDefault(self.name.clone()), &self.span);
                DataType::ByteStr(None)
            }
        };

        Ok(Variable { info, data })
    }
}

impl<'i> Routine<'i> {
    pub fn compile(
        &'i self,
        cursor: &mut (impl Read + Write),
        program: &'i Program,
        call_table: &mut CallTable,
        dump: &mut Option<File>,
        issues: &mut Issues<'i, issues::Compile>,
    ) -> Result<Vec<u16>, CompilerError> {
        let mut instr_map = Vec::with_capacity(self.statements.len());
        let mut jump_map = bmap![];

        let mut do_dump = true;
        for (no, statement) in self.statements.iter().enumerate() {
            let pos = cursor.pos();
            instr_map.push(pos);
            let instr = statement.compile(program, call_table, issues)?;
            if let Err(err) = instr.encode(cursor) {
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
            if let Instr::ControlFlow(ControlFlowOp::Call(site) | ControlFlowOp::Exec(site)) = instr
            {
                call_table.get_mut(site)?.sites.insert(pos);
            };

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

        for (from, to) in jump_map {
            cursor
                .edit(from, |instr| {
                    let pos = match instr {
                        Instr::ControlFlow(ControlFlowOp::Jif(ref mut pos))
                        | Instr::ControlFlow(ControlFlowOp::Jmp(ref mut pos)) => pos,
                        other => return Err(InstrError::Changed("jump", other.clone())),
                    };
                    *pos = instr_map[to as usize];
                    Ok(())
                })
                .map_err(|err| CompilerError::with(err, from))?;
        }

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
                        SemanticError::OperandWrongType {
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
                    SemanticError::OperandMissed {
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
                SemanticError::OperandWrongReg {
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
                        SemanticError::OperandWrongType {
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
                    SemanticError::OperandMissed {
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
                SemanticError::OperandWrongReg {
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

    fn num(
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
                SemanticError::OperandMissed {
                    operator: self.operator.0,
                    pos: no + 1,
                    expected: "constant or number literal",
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
                        issues.push_error(SemanticError::ConstUnknown(name.clone()), span);
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
                            SemanticError::ConstWrongType {
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
                    SemanticError::OperandWrongType {
                        operator: self.operator.0,
                        pos: no + 1,
                        expected: "constant or number literal",
                    },
                    op.as_span(),
                );
                return Ok(MaybeNumber::none());
            }
        };
        val.reshape(reg.layout());
        Ok(val)
    }

    fn str(
        &'i self,
        no: u8,
        consts: &'i BTreeMap<String, Const<'i>>,
        issues: &mut Issues<'i, issues::Compile>,
    ) -> Result<ByteStr, CompilerError> {
        let operand = if let Some(operand) = self.operands.get(no as usize) {
            operand
        } else {
            issues.push_error(
                SemanticError::OperandMissed {
                    operator: self.operator.0,
                    pos: no + 1,
                    expected: "constant or number literal",
                },
                &self.operator.1,
            );
            return Ok(ByteStr::default());
        };

        let val = match operand {
            Operand::Lit(Literal::String(s), _) => ByteStr::with(s),
            Operand::Const(name, span) => {
                let val = match consts.get(name) {
                    Some(val) => val,
                    None => {
                        issues.push_error(SemanticError::ConstUnknown(name.clone()), span);
                        return Ok(ByteStr::default());
                    }
                };
                match &val.value {
                    Literal::String(s) => ByteStr::with(s),
                    lit => {
                        issues.push_error(
                            SemanticError::ConstWrongType {
                                name: name.clone(),
                                expected: "string literal",
                                found: lit.description(),
                            },
                            span,
                        );
                        return Ok(ByteStr::default());
                    }
                }
            }
            op => {
                issues.push_error(
                    SemanticError::OperandWrongType {
                        operator: self.operator.0,
                        pos: no + 1,
                        expected: "constant or string literal",
                    },
                    op.as_span(),
                );
                return Ok(ByteStr::default());
            }
        };
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
                        SemanticError::OperandWrongType {
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
                    SemanticError::OperandMissed {
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
                Operand::Goto(goto, span) => Some((goto.clone(), *span)),
                Operand::Reg { span, .. }
                | Operand::Const(_, span)
                | Operand::Lit(_, span)
                | Operand::Call { span, .. } => {
                    issues.push_error(
                        SemanticError::OperandWrongType {
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
                    SemanticError::OperandMissed {
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
        call_table: &mut CallTable,
        issues: &mut Issues<'i, issues::Compile>,
    ) -> LibSite {
        let operand = if let Some(operand) = self.operands.get(no as usize) {
            operand
        } else {
            issues.push_error(
                SemanticError::OperandMissed {
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
                    issues.push_error(SemanticError::LibUnknown(lib.clone()), span);
                    LibId::default()
                });
                match call_table.find_or_insert(id, routine) {
                    Ok(pos) => LibSite::with(pos, id),
                    Err(err) => {
                        issues.push_error(err.into(), span);
                        LibSite::default()
                    }
                }
            }
            op => {
                issues.push_error(
                    SemanticError::OperandWrongType {
                        operator: self.operator.0,
                        pos: no + 1,
                        expected: op.description(),
                    },
                    op.as_span(),
                );
                LibSite::default()
            }
        }
    }

    pub fn compile(
        &'i self,
        program: &'i Program,
        call_table: &mut CallTable,
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
        macro_rules! num {
            ($no:expr, $reg:ident) => {
                Box::new(self.num($no, $reg, &program.consts, issues)?)
            };
        }
        macro_rules! str {
            ($no:expr) => {
                Box::new(self.str($no, &program.consts, issues)?)
            };
        }
        macro_rules! lib {
            ($no:expr) => {
                self.lib($no, program, call_table, issues)
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
            Operator::put => match reg! {0} {
                RegAll::A(a) => Instr::Put(PutOp::PutA(a, idx! {0}, num! {1, a})),
                RegAll::F(f) => Instr::Put(PutOp::PutF(f, idx! {0}, num! {1, f})),
                RegAll::R(r) => Instr::Put(PutOp::PutR(r, idx! {0}, num! {1, r})),
                RegAll::S => Instr::Bytes(BytesOp::Put(idx! {0}, str! {1}, false)),
            },
            Operator::putif => match reg! {1} {
                RegAR::A(a) => Instr::Put(PutOp::PutIfA(a, idx! {1}, num! {0, a})),
                RegAR::R(r) => Instr::Put(PutOp::PutIfR(r, idx! {1}, num! {0, r})),
            },

            // *** Move operations
            Operator::dup => {
                let reg = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
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
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
                    );
                }
                match reg {
                    RegAll::A(a) => Instr::Move(MoveOp::MovA(a, idx! {0}, idx! {1})),
                    RegAll::F(f) => Instr::Move(MoveOp::MovF(f, idx! {0}, idx! {1})),
                    RegAll::R(r) => Instr::Move(MoveOp::MovR(r, idx! {0}, idx! {1})),
                    RegAll::S => Instr::Bytes(BytesOp::Mov(idx! {0}, idx! {1})),
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
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
                    );
                }
                match reg {
                    RegAll::A(a) => Instr::Move(MoveOp::SwpA(a, idx! {0}, idx! {1})),
                    RegAll::F(f) => Instr::Move(MoveOp::SwpF(f, idx! {0}, idx! {1})),
                    RegAll::S => Instr::Bytes(BytesOp::Swp(idx! {0}, idx! {1})),
                    _ => {
                        issues.push_error(
                            SemanticError::OperandWrongReg {
                                operator: Operator::mov,
                                pos: 0,
                                expected: "register S",
                            },
                            self.operands[1].as_span(),
                        );
                        Instr::Nop
                    }
                }
            }

            // *** Comparison operations
            Operator::eq => {
                let reg = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
                    );
                }
                match reg {
                    RegAll::A(a) => Instr::Cmp(CmpOp::EqA(flags!(), a, idx! {0}, idx! {1})),
                    RegAll::F(f) => Instr::Cmp(CmpOp::EqF(flags!(), f, idx! {0}, idx! {1})),
                    RegAll::R(r) => Instr::Cmp(CmpOp::EqR(flags!(), r, idx! {0}, idx! {1})),
                    RegAll::S => {
                        let _: RegS = reg! {1};
                        Instr::Bytes(BytesOp::Eq(idx! {0}, idx! {1}))
                    }
                }
            }
            Operator::gt => {
                let reg = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
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
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
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
                if let Some(Operand::Lit(Literal::Int(mut step, _), span)) = self.operands.get(1) {
                    if step > u1024::from(i8::MAX as u8) {
                        step = u1024::from(1u64);
                        issues.push_error(SemanticError::StepTooLarge(self.operator.0), span);
                    }
                    Instr::Arithmetic(ArithmeticOp::Stp(
                        reg! {0},
                        idx! {0},
                        Step::with(step.low_u32() as i8),
                    ))
                } else {
                    let reg = reg! {0};
                    if reg != reg! {1} {
                        issues.push_error(
                            SemanticError::OperandRegMutBeEqual(self.operator.0),
                            self.operands[1].as_span(),
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
                if let Some(Operand::Lit(Literal::Int(mut step, _), span)) = self.operands.get(1) {
                    if step > u1024::from(i8::MAX as u8) {
                        step = u1024::from(1u64);
                        issues.push_error(SemanticError::StepTooLarge(self.operator.0), span);
                    }
                    Instr::Arithmetic(ArithmeticOp::Stp(
                        reg! {0},
                        idx! {0},
                        Step::with(-(step.low_u32() as i8)),
                    ))
                } else {
                    let reg = reg! {0};
                    if reg != reg! {1} {
                        issues.push_error(
                            SemanticError::OperandRegMutBeEqual(self.operator.0),
                            self.operands[1].as_span(),
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
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
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
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
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
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
                    );
                }
                if reg != reg! {2} {
                    issues.push_error(
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span(),
                    );
                }
                Instr::Bitwise(BitwiseOp::And(reg, idx! {0}, idx! {1}, idx! {2}))
            }
            Operator::or => {
                let reg: RegAR = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
                    );
                }
                if reg != reg! {2} {
                    issues.push_error(
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span(),
                    );
                }
                Instr::Bitwise(BitwiseOp::Or(reg, idx! {0}, idx! {1}, idx! {2}))
            }
            Operator::xor => {
                let reg: RegAR = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span(),
                    );
                }
                if reg != reg! {2} {
                    issues.push_error(
                        SemanticError::OperandRegMutBeEqual(self.operator.0),
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
                RegAll::A(a) => Instr::Bitwise(BitwiseOp::RevA(a, idx! {0})),
                RegAll::R(r) => Instr::Bitwise(BitwiseOp::RevR(r, idx! {0})),
                RegAll::S => {
                    let _: RegS = reg! {1};
                    Instr::Bytes(BytesOp::Rev(idx! {0}, idx! {1}))
                }
                _ => {
                    issues.push_error(
                        SemanticError::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 0,
                            expected: "register S",
                        },
                        self.operands[0].as_span(),
                    );
                    Instr::Nop
                }
            },

            // *** String operations
            Operator::fill => {
                let _: RegS = reg! {0};
                let reg1: RegA = reg! {1};
                let reg2: RegA = reg! {2};
                let reg3: RegA = reg! {3};
                if reg1 != RegA::A16 {
                    issues.push_error(
                        SemanticError::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 1,
                            expected: "a16 register",
                        },
                        self.operands[1].as_span(),
                    );
                }
                if reg2 != RegA::A16 {
                    issues.push_error(
                        SemanticError::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 2,
                            expected: "a16 register",
                        },
                        self.operands[2].as_span(),
                    );
                }
                if reg3 != RegA::A8 {
                    issues.push_error(
                        SemanticError::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 3,
                            expected: "a8 register",
                        },
                        self.operands[3].as_span(),
                    );
                }
                Instr::Bytes(BytesOp::Fill(idx! {0}, idx! {1}, idx! {2}, idx! {3}, flags!()))
            }
            Operator::len => {
                let _: RegS = reg! {0};
                Instr::Bytes(BytesOp::Len(idx! {0}, reg! {1}, idx! {1}))
            }
            Operator::cnt => {
                let _: RegS = reg! {0};
                let byte_reg: RegA = reg! {1};
                let dst_reg: RegA = reg! {2};
                if byte_reg != RegA::A8 {
                    issues.push_error(
                        SemanticError::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 2,
                            expected: "a8 register",
                        },
                        self.operands[2].as_span(),
                    );
                }
                if dst_reg != RegA::A16 {
                    issues.push_error(
                        SemanticError::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 0,
                            expected: "a16 register",
                        },
                        self.operands[0].as_span(),
                    );
                }
                Instr::Bytes(BytesOp::Cnt(idx! {0}, idx! {1}, idx! {2}))
            }
            Operator::con => {
                let _: RegS = reg! {0};
                let _: RegS = reg! {1};
                for n in 2..5 {
                    let reg: RegA = reg! {n};
                    if reg != RegA::A16 {
                        issues.push_error(
                            SemanticError::OperandWrongReg {
                                operator: self.operator.0,
                                pos: n,
                                expected: "a16 register",
                            },
                            self.operands[n as usize].as_span(),
                        );
                    }
                }
                Instr::Bytes(BytesOp::Con(idx! {0}, idx! {1}, idx! {2}, idx! {3}, idx! {4}))
            }
            Operator::find => {
                let _: RegS = reg! {0};
                let _: RegS = reg! {1};
                let reg: RegA = reg! {2};
                let idx: Reg32 = idx! {2};
                if reg != RegA::A16 || idx != Reg32::Reg0 {
                    issues.push_error(
                        SemanticError::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 2,
                            expected: "a16[0] register",
                        },
                        self.operands[2].as_span(),
                    );
                }
                Instr::Bytes(BytesOp::Find(idx! {0}, idx! {1}))
            }
            Operator::extr => {
                let _: RegS = reg! {0};
                Instr::Bytes(BytesOp::Extr(idx! {0}, reg! {1}, idx! {1}, idx! {2}))
            }
            Operator::inj => {
                let _: RegS = reg! {0};
                let reg: RegA = reg! {2};
                if reg != RegA::A16 {
                    issues.push_error(
                        SemanticError::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 2,
                            expected: "a16 register",
                        },
                        self.operands[2].as_span(),
                    );
                }
                Instr::Bytes(BytesOp::Inj(idx! {0}, reg! {1}, idx! {1}, idx! {2}))
            }
            Operator::join => {
                let _: RegS = reg! {0};
                let _: RegS = reg! {1};
                let _: RegS = reg! {2};
                Instr::Bytes(BytesOp::Join(idx! {0}, idx! {1}, idx! {2}))
            }
            Operator::splt => {
                let _: RegS = reg! {0};
                let _: RegS = reg! {2};
                let _: RegS = reg! {3};
                let reg: RegA = reg! {1};
                if reg != RegA::A16 {
                    issues.push_error(
                        SemanticError::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 1,
                            expected: "a16[0] register",
                        },
                        self.operands[1].as_span(),
                    );
                }
                Instr::Bytes(BytesOp::Splt(flags!(), idx! {1}, idx! {0}, idx! {2}, idx! {3}))
            }
            Operator::ins => {
                let _: RegS = reg! {0};
                let _: RegS = reg! {1};
                let reg: RegA = reg! {2};
                if reg != RegA::A16 {
                    issues.push_error(
                        SemanticError::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 2,
                            expected: "a16[0] register",
                        },
                        self.operands[2].as_span(),
                    );
                }
                Instr::Bytes(BytesOp::Ins(flags!(), idx! {2}, idx! {0}, idx! {1}))
            }
            Operator::del => {
                todo!("Unimplemented operation `del`")
            }

            // *** Digest operations
            Operator::ripemd => {
                let reg: RegR = reg! {1};
                if reg != RegR::R160 {
                    issues.push_error(
                        SemanticError::OperandWrongReg {
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
            Operator::blake3 => {
                let reg: RegR = reg! {1};
                if reg != RegR::R256 {
                    issues.push_error(
                        SemanticError::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 2,
                            expected: "r256 register",
                        },
                        self.operands[1].as_span(),
                    );
                    return Ok(Instr::Nop);
                }
                Instr::Digest(DigestOp::Blake3(idx! {0}, idx! {1}))
            }
            Operator::sha2 => match reg! {1} {
                RegR::R256 => Instr::Digest(DigestOp::Sha256(idx! {0}, idx! {1})),
                RegR::R512 => Instr::Digest(DigestOp::Sha512(idx! {0}, idx! {1})),
                _ => {
                    issues.push_error(
                        SemanticError::OperandWrongReg {
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
                            SemanticError::OperandWrongReg {
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
                            SemanticError::OperandWrongReg {
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
                            SemanticError::OperandWrongReg {
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
                            SemanticError::OperandWrongReg {
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
