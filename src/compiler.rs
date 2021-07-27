// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

//! Compiler converting AST constructed by analyzer into instructions and library data structure

use std::collections::BTreeMap;
use std::convert::{TryFrom, TryInto};
use std::str::FromStr;

use aluvm::data::{ByteStr, MaybeNumber, Step};
use aluvm::isa::{
    ArithmeticOp, BitwiseOp, Bytecode, CmpOp, ControlFlowOp, DigestOp, Flag, Instr, MoveOp,
    ParseFlagError, PutOp, Secp256k1Op,
};
use aluvm::libs::{Cursor, LibId, LibSeg, LibSite, Read, Write};
use aluvm::reg::{NumericRegister, Reg32, RegAF, RegAFR, RegAR, RegAll, RegR, Register};
use amplify::num::u1024;
use pest::Span;
use rustc_apfloat::ieee;

use crate::ast::{self, FlagSet, Literal, Operand, Operator};
use crate::{Error, Issues};

pub mod obj {
    use std::collections::BTreeMap;

    use aluvm::data::{FloatLayout, IntLayout, MaybeNumber};
    use aluvm::libs::LibSeg;

    #[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Default)]
    pub struct Symbols {
        /// External routine names
        pub externals: Vec<String>,
        /// Map of local routine names to code offsets
        pub routines: BTreeMap<String, u16>,
    }

    #[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
    pub enum DataType {
        String(String),
        Int(IntLayout, MaybeNumber),
        Float(FloatLayout, MaybeNumber),
    }

    #[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
    pub struct Input {
        pub name: String,
        pub details: String,
        pub data: DataType,
    }

    #[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Default)]
    pub struct Module {
        pub code: Vec<u8>,
        pub data: Vec<u8>,
        pub libs: LibSeg,
        pub input: Vec<Input>,
        pub symbols: Symbols,
    }
}

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

impl<'i> ast::Program<'i> {
    pub fn compile(&'i self) -> (obj::Module, Issues<'i>) {
        let mut issues = Issues::default();

        let mut libs_segment = LibSeg::default();
        self.libs.map.values().any(|id| {
            libs_segment
                .add_lib(*id)
                .map_err(|err| issues.push_error(err.into(), self.libs.span.clone()))
                .is_err()
        });
        let mut code_segment = ByteStr::default();
        let mut writer = Cursor::new(&mut code_segment.bytes[..], &libs_segment);

        let mut externals = vec![];
        let mut frame = LibFrame::with(&libs_segment, &mut externals);
        let routines = self
            .code
            .iter()
            .filter_map(|(name, routine)| {
                Some((name.clone(), routine.compile(&mut writer, self, &mut frame, &mut issues)?))
            })
            .collect();

        let pos = writer.pos();
        let data = writer.into_data_segment();
        match pos {
            Some(pos) => code_segment.adjust_len(pos, false),
            None => code_segment.adjust_len(u16::MAX, true),
        }

        // TODO: Collect inputs
        let input = vec![];

        let symbols = obj::Symbols { externals, routines };

        (
            obj::Module { code: code_segment.to_vec(), data, libs: libs_segment, input, symbols },
            issues,
        )
    }
}

impl<'i> ast::Routine<'i> {
    pub fn compile(
        &'i self,
        writer: &mut (impl Read + Write),
        program: &'i ast::Program,
        frame: &mut LibFrame,
        issues: &mut Issues<'i>,
    ) -> Option<u16> {
        let mut map = Vec::with_capacity(self.code.len());

        if issues.has_errors() {
            return writer.pos();
        }

        for line in &self.code {
            map.push(writer.pos());
            let instr = line.compile(program, frame, issues);
            if let Err(err) = instr.write(writer) {
                issues.push_error(err.into(), line.span.clone());
                break;
            }
        }

        // TODO: Replace jumps

        map.get(0).copied().unwrap_or_else(|| writer.pos())
    }
}

impl<'i> ast::Instruction<'i> {
    fn reg<T>(&'i self, no: u8, issues: &mut Issues<'i>) -> T
    where
        T: TryFrom<RegAll> + Register,
    {
        let (operand, span) = self
            .operands
            .get(no as usize)
            .map(|op| match op {
                Operand::Reg { set, span, .. } => (*set, span.clone()),
                Operand::Goto(_, span)
                | Operand::Const(_, span)
                | Operand::Lit(_, span)
                | Operand::Call { span, .. } => {
                    issues.push_error(
                        Error::OperandWrongType {
                            operator: self.operator.0,
                            pos: no + 1,
                            expected: T::description(),
                        },
                        span.clone(),
                    );
                    (RegAll::default(), span.clone())
                }
            })
            .unwrap_or_else(|| {
                issues.push_error(
                    Error::OperandMissed {
                        operator: self.operator.0,
                        pos: no + 1,
                        expected: T::description(),
                    },
                    self.span.clone(),
                );
                (RegAll::default(), self.span.clone())
            });
        operand.try_into().unwrap_or_else(|_| {
            issues.push_error(
                Error::OperandWrongReg {
                    operator: self.operator.0,
                    pos: no + 1,
                    expected: T::description(),
                },
                span,
            );
            T::default()
        })
    }

    fn idx<T>(&'i self, no: u8, issues: &mut Issues<'i>) -> T
    where
        T: TryFrom<Reg32> + Register,
    {
        let (operand, span) = self
            .operands
            .get(no as usize)
            .map(|op| match op {
                Operand::Reg { index, span, .. } => (*index, span.clone()),
                Operand::Goto(_, span)
                | Operand::Const(_, span)
                | Operand::Lit(_, span)
                | Operand::Call { span, .. } => {
                    issues.push_error(
                        Error::OperandWrongType {
                            operator: self.operator.0,
                            pos: no + 1,
                            expected: T::description(),
                        },
                        span.clone(),
                    );
                    (Reg32::default(), span.clone())
                }
            })
            .unwrap_or_else(|| {
                issues.push_error(
                    Error::OperandMissed {
                        operator: self.operator.0,
                        pos: no + 1,
                        expected: T::description(),
                    },
                    self.operator.1.clone(),
                );
                (Reg32::default(), self.operator.1.clone())
            });
        operand.try_into().unwrap_or_else(|_| {
            issues.push_error(
                Error::OperandWrongReg {
                    operator: self.operator.0,
                    pos: no + 1,
                    expected: T::description(),
                },
                span,
            );
            T::default()
        })
    }

    fn flags<F>(&'i self, issues: &mut Issues<'i>) -> F
    where
        WrappedFlag<F>: TryFrom<FlagSet<'i, char>, Error = FlagError<'i>>,
        F: Flag,
    {
        WrappedFlag::try_from(self.flags.clone())
            .map(|wrapper| wrapper.0)
            .map_err(|err| {
                issues.push_error(err.inner.into(), err.span.unwrap_or(self.operator.1.clone()));
            })
            .unwrap_or_default()
    }

    fn val(
        &'i self,
        no: u8,
        reg: impl NumericRegister,
        consts: &'i BTreeMap<String, ast::Const<'i>>,
        issues: &mut Issues<'i>,
    ) -> MaybeNumber {
        let operand = if let Some(operand) = self.operands.get(no as usize) {
            operand
        } else {
            issues.push_error(
                Error::OperandMissed {
                    operator: self.operator.0,
                    pos: no + 1,
                    expected: "constant or integer literal",
                },
                self.operator.1.clone(),
            );
            return MaybeNumber::none();
        };

        let mut val = match operand {
            Operand::Lit(Literal::Int(val, _), _) => MaybeNumber::from(val),
            Operand::Lit(Literal::Float(i, r, e), _) => MaybeNumber::from(
                ieee::Quad::from_str(&format!("{}.{}e{}", i, r, e)).expect("internal error I0003"),
            ),
            Operand::Const(name, span) => {
                let val = match consts.get(name) {
                    Some(val) => val,
                    None => {
                        issues.push_error(Error::ConstUnknown(name.clone()), span.clone());
                        return MaybeNumber::none();
                    }
                };
                match &val.value {
                    Literal::Int(val, _) => MaybeNumber::from(val),
                    Literal::Float(i, r, e) => MaybeNumber::from(
                        ieee::Quad::from_str(&format!("{}.{}e{}", i, r, e))
                            .expect("internal error I0004"),
                    ),
                    lit => {
                        issues.push_error(
                            Error::ConstWrongType {
                                name: name.clone(),
                                expected: "integer or float",
                                found: lit.description(),
                            },
                            span.clone(),
                        );
                        return MaybeNumber::none();
                    }
                }
            }
            op => {
                issues.push_error(
                    Error::OperandWrongType {
                        operator: self.operator.0,
                        pos: no + 1,
                        expected: op.description(),
                    },
                    op.as_span().clone(),
                );
                return MaybeNumber::none();
            }
        };
        val.reshape(reg.layout());
        val
    }

    fn lib(
        &'i self,
        no: u8,
        program: &'i ast::Program,
        frame: &mut LibFrame,
        issues: &mut Issues<'i>,
    ) -> LibSite {
        let operand = if let Some(operand) = self.operands.get(no as usize) {
            operand
        } else {
            issues.push_error(
                Error::OperandMissed {
                    operator: self.operator.0,
                    pos: no + 1,
                    expected: "external call statement",
                },
                self.operator.1.clone(),
            );
            return LibSite::default();
        };

        match operand {
            Operand::Call { lib, routine, span } => {
                let id = program.libs.map.get(lib).copied().unwrap_or_else(|| {
                    issues.push_error(Error::LibUnknown(lib.clone()), span.clone());
                    LibId::default()
                });
                match frame.find_or_insert_ext(id, routine) {
                    Ok((_, pos)) => return LibSite::with(pos, id),
                    Err(err) => {
                        issues.push_error(err.into(), span.clone());
                        return LibSite::default();
                    }
                }
            }
            op => {
                issues.push_error(
                    Error::OperandWrongType {
                        operator: self.operator.0,
                        pos: no + 1,
                        expected: op.description(),
                    },
                    op.as_span().clone(),
                );
                return LibSite::default();
            }
        }
    }

    pub fn compile(
        &'i self,
        program: &'i ast::Program,
        frame: &mut LibFrame,
        issues: &mut Issues<'i>,
    ) -> Instr {
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
                Box::new(self.val($no, $reg, &program.r#const, issues))
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
        match self.operator.0 {
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
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span().clone(),
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
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span().clone(),
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
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span().clone(),
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
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span().clone(),
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
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span().clone(),
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
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span().clone(),
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
                        issues.push_error(Error::StepTooLarge(self.operator.0), span.clone());
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
                            Error::OperandRegMutBeEqual(self.operator.0),
                            self.operands[2].as_span().clone(),
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
                        issues.push_error(Error::StepTooLarge(self.operator.0), span.clone());
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
                            Error::OperandRegMutBeEqual(self.operator.0),
                            self.operands[2].as_span().clone(),
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
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span().clone(),
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
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span().clone(),
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
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span().clone(),
                    );
                }
                if reg != reg! {2} {
                    issues.push_error(
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span().clone(),
                    );
                }
                Instr::Bitwise(BitwiseOp::And(reg, idx! {0}, idx! {1}, idx! {2}))
            }
            Operator::or => {
                let reg: RegAR = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span().clone(),
                    );
                }
                if reg != reg! {2} {
                    issues.push_error(
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span().clone(),
                    );
                }
                Instr::Bitwise(BitwiseOp::Or(reg, idx! {0}, idx! {1}, idx! {2}))
            }
            Operator::xor => {
                let reg: RegAR = reg! {0};
                if reg != reg! {1} {
                    issues.push_error(
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[1].as_span().clone(),
                    );
                }
                if reg != reg! {2} {
                    issues.push_error(
                        Error::OperandRegMutBeEqual(self.operator.0),
                        self.operands[2].as_span().clone(),
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
                        Error::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 2,
                            expected: "r160 register",
                        },
                        self.operands[1].as_span().clone(),
                    );
                    return Instr::Nop;
                }
                Instr::Digest(DigestOp::Ripemd(idx! {0}, idx! {1}))
            }
            Operator::sha2 => match reg! {1} {
                RegR::R256 => Instr::Digest(DigestOp::Sha256(idx! {0}, idx! {1})),
                RegR::R512 => Instr::Digest(DigestOp::Sha512(idx! {0}, idx! {1})),
                _ => {
                    issues.push_error(
                        Error::OperandWrongReg {
                            operator: self.operator.0,
                            pos: 2,
                            expected: "r256 or r512 registers",
                        },
                        self.operands[1].as_span().clone(),
                    );
                    return Instr::Nop;
                }
            },

            // *** Elliptic curve operations
            Operator::secpgen => {
                for r in 0..=1 {
                    let reg: RegR = reg! {r};
                    if reg != [RegR::R256, RegR::R512][r as usize] {
                        issues.push_error(
                            Error::OperandWrongReg {
                                operator: self.operator.0,
                                pos: r + 1,
                                expected: if r == 0 { "r256 register" } else { "r512 register" },
                            },
                            self.operands[r as usize].as_span().clone(),
                        );
                        return Instr::Nop;
                    }
                }
                Instr::Secp256k1(Secp256k1Op::Gen(idx! {0}, idx! {1}))
            }
            Operator::secpneg => {
                for r in 0..=1 {
                    let reg: RegR = reg! {r};
                    if reg != RegR::R512 {
                        issues.push_error(
                            Error::OperandWrongReg {
                                operator: self.operator.0,
                                pos: r + 1,
                                expected: "r512 register",
                            },
                            self.operands[r as usize].as_span().clone(),
                        );
                        return Instr::Nop;
                    }
                }
                Instr::Secp256k1(Secp256k1Op::Neg(idx! {0}, idx! {1}))
            }
            Operator::secpadd => {
                for r in 0..=1 {
                    let reg: RegR = reg! {r};
                    if reg != RegR::R512 {
                        issues.push_error(
                            Error::OperandWrongReg {
                                operator: self.operator.0,
                                pos: r + 1,
                                expected: "r512 register",
                            },
                            self.operands[r as usize].as_span().clone(),
                        );
                        return Instr::Nop;
                    }
                }
                Instr::Secp256k1(Secp256k1Op::Add(idx! {0}, idx! {1}))
            }
            Operator::secpmul => {
                for r in 1..=2 {
                    let reg: RegR = reg! {r};
                    if reg != RegR::R512 {
                        issues.push_error(
                            Error::OperandWrongReg {
                                operator: self.operator.0,
                                pos: r + 1,
                                expected: "r512 register",
                            },
                            self.operands[r as usize].as_span().clone(),
                        );
                        return Instr::Nop;
                    }
                }
                Instr::Secp256k1(Secp256k1Op::Mul(reg! {0}, idx! {0}, idx! {1}, idx! {2}))
            }

            Operator::nop => Instr::Nop,
        }
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
