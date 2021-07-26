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
    ArithmeticOp, BitwiseOp, Bytecode, CmpOp, Flag, Instr, MoveOp, ParseFlagError, PutOp,
};
use aluvm::libs::{Cursor, LibSeg};
use aluvm::reg::{NumericRegister, Reg32, RegAF, RegAFR, RegAR, RegAll, Register};
use amplify::num::u1024;
use pest::Span;
use rustc_apfloat::ieee;

use crate::ast::{self, FlagSet, Literal, Operand, Operator};
use crate::{Error, Issues};

pub mod obj {
    use std::collections::BTreeMap;

    use crate::Issues;

    #[derive(Clone, Hash, Debug)]
    pub struct Program<'i> {
        pub routines: BTreeMap<String, Routine>,
        pub issues: Issues<'i>,
    }

    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    pub struct Routine {
        pub bytecode: Vec<u8>,
        /// Maps instruction positions withing the bytecode
        pub map: Vec<u16>,
    }
}

impl<'i> ast::Program<'i> {
    pub fn compile(&'i self) -> obj::Program {
        let mut issues = Issues::default();
        let routines = self
            .code
            .iter()
            .map(|(name, routine)| (name.clone(), routine.compile(self, &mut issues)))
            .collect();
        obj::Program { routines, issues }
    }
}

impl<'i> ast::Routine<'i> {
    pub fn compile(&'i self, program: &'i ast::Program, issues: &mut Issues<'i>) -> obj::Routine {
        let mut instructions = Vec::with_capacity(self.code.len());
        let mut map = Vec::with_capacity(self.code.len());
        for line in self.code.iter() {
            instructions.push(line.compile(program, issues));
        }
        if issues.has_errors() {
            return obj::Routine { bytecode: vec![], map };
        }

        let call_sites = instructions.iter().filter_map(|instr| instr.call_site());
        let libs_segment = LibSeg::from(call_sites)
            .map_err(|err| issues.push_error(err.into(), self.span.clone()))
            .unwrap_or_default();
        let mut code_segment = ByteStr::default();
        let mut writer = Cursor::new(&mut code_segment.bytes[..], &libs_segment);

        for (instr, line) in instructions.iter().zip(self.code.iter()) {
            map.push(writer.pos());
            if let Err(err) = instr.write(&mut writer) {
                issues.push_error(err.into(), line.span.clone());
                let pos = writer.pos();
                let data = writer.into_data_segment();
                code_segment.adjust_len(pos, false);
                return obj::Routine { bytecode: code_segment.to_vec(), map };
            }
        }
        let pos = writer.pos();
        let data = writer.into_data_segment();
        code_segment.adjust_len(pos, false);
        obj::Routine { bytecode: code_segment.to_vec(), map }
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
                | Operand::Call(_, span) => {
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
                | Operand::Call(_, span) => {
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

    pub fn compile(&'i self, program: &'i ast::Program, issues: &mut Issues<'i>) -> Instr {
        macro_rules! reg {
            ($no:literal) => {
                self.reg($no, issues)
            };
        }
        macro_rules! idx {
            ($no:literal) => {
                self.idx($no, issues)
            };
        }
        macro_rules! val {
            ($no:literal, $reg:ident) => {
                Box::new(self.val($no, $reg, &program.r#const, issues))
            };
        }
        macro_rules! flags {
            () => {
                self.flags(issues)
            };
        }
        match self.operator.0 {
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

            Operator::nop => Instr::Nop,

            _ => Instr::Nop,
            /*
            Operator::call => {}
            Operator::extr => {}
            Operator::fail => {}
            Operator::jif => {}
            Operator::jmp => {}
            Operator::read => {}
            Operator::ret => {}
            Operator::ripemd => {}
            Operator::secpadd => {}
            Operator::secpgen => {}
            Operator::secpmul => {}
            Operator::secpneg => {}
            Operator::sha2 => {}
            Operator::succ => {}
             */
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
