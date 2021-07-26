// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

//! Compiler converting AST constructed by analyzer into instructions and library data structure

use std::convert::{TryFrom, TryInto};

use aluvm::data::Step;
use aluvm::isa::{ArithmeticOp, Bytecode, CmpOp, Flag, Instr, IntFlags, ParseFlagError};
use aluvm::libs::{Cursor, LibSeg};
use aluvm::reg::{Reg32, RegAF, RegAll, Register};
use amplify::num::u1024;
use pest::Span;

use crate::ast::{self, FlagSet, Literal, Operand, Operator};
use crate::{Error, Issues};

pub mod obj {
    use std::collections::BTreeMap;

    use crate::Issues;

    pub struct Program<'i> {
        pub routines: BTreeMap<String, Routine>,
        pub issues: Issues<'i>,
    }

    pub struct Routine {
        pub bytecode: Vec<u8>,
        /// Maps instruction positions withing the bytecode
        pub map: Vec<u16>,
    }
}

impl<'i> ast::Program<'i> {
    pub fn compile(&self) -> obj::Program {
        let mut issues = self.issues.clone();
        let routines = self
            .code
            .iter()
            .map(|(name, routine)| (name.clone(), routine.compile(&mut issues)))
            .collect();
        obj::Program { routines, issues }
    }
}

impl<'i> ast::Routine<'i> {
    pub fn compile(&self, issues: &mut Issues<'i>) -> obj::Routine {
        let mut bytecode = Vec::with_capacity(self.code.len() * 3);
        let mut map = Vec::with_capacity(self.code.len());
        let libs = LibSeg::default();
        let mut writer = Cursor::new(&mut bytecode, &libs);
        for (index, line) in self.code.iter().enumerate() {
            map[index] = writer.pos();
            let instr = line.compile(issues);
            if let Err(err) = instr.write(&mut writer) {
                issues.push_error(err.into(), line.span.clone());
                return obj::Routine { bytecode, map };
            }
        }
        obj::Routine { bytecode, map }
    }
}

impl<'i> ast::Instruction<'i> {
    fn reg<T>(&self, no: u8, issues: &mut Issues<'i>) -> T
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
                            pos: no,
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
                        pos: no,
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
                    pos: no,
                    expected: T::description(),
                },
                span,
            );
            T::default()
        })
    }

    fn idx<T>(&self, no: u8, issues: &mut Issues<'i>) -> T
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
                            pos: no,
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
                        pos: no,
                        expected: T::description(),
                    },
                    self.span.clone(),
                );
                (Reg32::default(), self.span.clone())
            });
        operand.try_into().unwrap_or_else(|_| {
            issues.push_error(
                Error::OperandWrongReg {
                    operator: self.operator.0,
                    pos: no,
                    expected: T::description(),
                },
                span,
            );
            T::default()
        })
    }

    fn flags<F>(&self, issues: &mut Issues<'i>) -> F
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

    pub fn compile(&self, issues: &mut Issues<'i>) -> Instr {
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
        macro_rules! flags {
            () => {
                self.flags(issues)
            };
        }
        match self.operator.0 {
            Operator::neg => Instr::Arithmetic(ArithmeticOp::Neg(reg! {0}, idx! {1})),
            Operator::inc => {
                Instr::Arithmetic(ArithmeticOp::Stp(reg! {0}, idx! {1}, Step::with(1)))
            }
            Operator::dec => {
                Instr::Arithmetic(ArithmeticOp::Stp(reg! {0}, idx! {1}, Step::with(-1)))
            }
            Operator::add => {
                if let Some(Operand::Lit(Literal::Int(mut step, _), span)) = self.operands.get(0) {
                    if step > u1024::from(i16::MAX as u16) {
                        step = u1024::from(1u64);
                        issues.push_error(Error::StepTooLarge(self.operator.0), span.clone());
                    }
                    Instr::Arithmetic(ArithmeticOp::Stp(
                        reg! {0},
                        idx! {1},
                        Step::with(step.low_u32() as i16),
                    ))
                } else {
                    match reg! {0} {
                        RegAF::A(a) => {
                            Instr::Arithmetic(ArithmeticOp::AddA(flags!(), a, idx! {1}, idx! {2}))
                        }
                        RegAF::F(f) => {
                            Instr::Arithmetic(ArithmeticOp::AddF(flags!(), f, idx! {1}, idx! {2}))
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
                        reg! {0},
                        idx! {1},
                        Step::with(-(step.low_u32() as i16)),
                    ))
                } else {
                    match reg! {0} {
                        RegAF::A(a) => {
                            Instr::Arithmetic(ArithmeticOp::SubA(flags!(), a, idx! {1}, idx! {2}))
                        }
                        RegAF::F(f) => {
                            Instr::Arithmetic(ArithmeticOp::SubF(flags!(), f, idx! {1}, idx! {2}))
                        }
                    }
                }
            }
            Operator::mul => match reg! {0} {
                RegAF::A(a) => {
                    Instr::Arithmetic(ArithmeticOp::MulA(flags!(), a, idx! {1}, idx! {2}))
                }
                RegAF::F(f) => {
                    Instr::Arithmetic(ArithmeticOp::MulF(flags!(), f, idx! {1}, idx! {2}))
                }
            },
            Operator::div => match reg! {0} {
                RegAF::A(a) => {
                    Instr::Arithmetic(ArithmeticOp::DivA(flags!(), a, idx! {1}, idx! {2}))
                }
                RegAF::F(f) => {
                    Instr::Arithmetic(ArithmeticOp::DivF(flags!(), f, idx! {1}, idx! {2}))
                }
            },
            Operator::rem => {
                Instr::Arithmetic(ArithmeticOp::Rem(reg! {0}, idx! {1}, reg! {2}, idx! {3}))
            }
            Operator::abs => Instr::Arithmetic(ArithmeticOp::Abs(reg! {0}, idx! {1})),

            Operator::nop => Instr::Nop,

            _ => panic!(),
            /*
            Operator::and => {}
            Operator::call => {}
            Operator::clr => {}
            Operator::cnv => {}
            Operator::cpy => {}
            Operator::dup => {}
            Operator::eq => {}
            Operator::extr => {}
            Operator::fail => {}
            Operator::ge => {}
            Operator::gt => {}
            Operator::ifn => {}
            Operator::ifz => {}
            Operator::inv => {}
            Operator::jif => {}
            Operator::jmp => {}
            Operator::le => {}
            Operator::lt => {}
            Operator::mov => {}
            Operator::put => {}
            Operator::putif => {}
            Operator::read => {}
            Operator::ret => {}
            Operator::rev => {}
            Operator::ripemd => {}
            Operator::scl => {}
            Operator::scr => {}
            Operator::secpadd => {}
            Operator::secpgen => {}
            Operator::secpmul => {}
            Operator::secpneg => {}
            Operator::sha2 => {}
            Operator::shl => {}
            Operator::shr => {}
            Operator::spy => {}
            Operator::st => Instr::Cmp(CmpOp::St()),
            Operator::succ => {}
            Operator::swp => {}
            Operator::xor => {}
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
