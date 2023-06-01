// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

//! Analyzer code converting abstract parse tree, returned by parser, into an
//! abstract syntax tree (AST)

use std::convert::TryFrom;
use std::str::FromStr;

use aluvm::data::{FloatLayout, IntLayout};
use aluvm::libs::LibId;
use aluvm::reg::{RegA, RegAll, RegBlock, RegF, RegR};
use aluvm::Isa;
use amplify::hex::FromHex;
use amplify::num::error::OverflowError;
use amplify::num::{u1024, u5};
use pest::iterators::Pair;

use crate::ast::{
    Const, FlagSet, IntBase, Libs, Literal, Operand, Operator, Program, Routine, Statement, Var,
    VarType,
};
use crate::issues::{self, Issues, SyntaxError, SyntaxWarning, ToSrc};
use crate::parser::Rule;
use crate::LexerError;

impl<'i> Program<'i> {
    pub fn analyze(
        pair: Pair<'i, Rule>,
    ) -> Result<(Self, Issues<'i, issues::Analyze>), LexerError<'i>> {
        let mut issues = Default::default();
        let mut program = Program {
            isae: Default::default(),
            libs: Libs { map: bmap! {}, span: pair.as_span() },
            main: None,
            routines: Default::default(),
            consts: Default::default(),
            input: Default::default(),
        };
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::isae => program.analyze_isae(pair, &mut issues)?,
                Rule::routine => program.analyze_routine(pair, &mut issues)?,
                Rule::libs => program.analyze_libs(pair, &mut issues)?,
                Rule::data => program.analyze_const(pair, &mut issues)?,
                Rule::input => program.analyze_input(pair, &mut issues)?,
                Rule::EOI => {}
                _ => return Err(LexerError::UnknownSegment(pair.as_rule())),
            };
        }
        Ok((program, issues))
    }
}

impl<'i> Program<'i> {
    fn analyze_isae(
        &mut self,
        pair: Pair<'i, Rule>,
        issues: &mut Issues<'i, issues::Analyze>,
    ) -> Result<(), LexerError<'i>> {
        let mut set = bset![];
        for pair in pair.into_inner() {
            let mut found = false;
            for isa in Isa::all() {
                if isa.to_string() == pair.as_str() {
                    if set.contains(&isa) {
                        issues.push_warning(SyntaxWarning::DuplicatedIsa(isa), &pair);
                    }
                    set.insert(isa);
                    found = true;
                    break;
                }
            }
            if !found {
                issues.push_error(
                    SyntaxError::UnknownIsa(pair.as_str().to_string()),
                    &pair.as_span(),
                );
            }
        }
        self.isae = set;
        Ok(())
    }

    fn analyze_routine(
        &mut self,
        pair: Pair<'i, Rule>,
        issues: &mut Issues<'i, issues::Analyze>,
    ) -> Result<(), LexerError<'i>> {
        let span = pair.as_span();
        let routine = Routine::analyze(pair, issues)?;
        if self.routines.contains_key(&routine.name) {
            issues.push_error(SyntaxError::RoutineNameReuse(routine.name), &span);
        } else {
            self.routines.insert(routine.name.clone(), routine);
        }
        Ok(())
    }

    fn analyze_libs(
        &mut self,
        pair: Pair<'i, Rule>,
        issues: &mut Issues<'i, issues::Analyze>,
    ) -> Result<(), LexerError<'i>> {
        let span = pair.as_span();
        let mut map = bmap! {};
        for pair in pair.into_inner() {
            let mut iter = pair.into_inner();
            let name = iter.next().ok_or_else(|| LexerError::LibNoName(span.to_src()))?;
            let id = iter.next().ok_or_else(|| LexerError::LibNoId(span.to_src()))?;
            if map.contains_key(name.as_str()) {
                issues.push_error(SyntaxError::RepeatedLibName(name.as_str().to_owned()), &name);
            } else {
                match LibId::from_str(id.as_str()) {
                    Ok(libid) => {
                        map.insert(name.as_str().to_owned(), libid);
                    }
                    Err(err) => {
                        issues
                            .push_error(SyntaxError::WrongLibId(id.as_str().to_owned(), err), &id);
                    }
                }
            }
        }
        self.libs = Libs { map, span };
        Ok(())
    }

    fn analyze_const(
        &mut self,
        pair: Pair<'i, Rule>,
        issues: &mut Issues<'i, issues::Analyze>,
    ) -> Result<(), LexerError<'i>> {
        let mut consts = bmap! {};
        for pair in pair.into_inner() {
            let span = pair.as_span();
            let c = Const::analyze(pair, issues)?;
            if consts.contains_key(&c.name) {
                issues.push_error(SyntaxError::RepeatedConstName(c.name), &span);
            } else {
                consts.insert(c.name.clone(), c);
            }
        }
        self.consts = consts;
        Ok(())
    }

    fn analyze_input(
        &mut self,
        pair: Pair<'i, Rule>,
        issues: &mut Issues<'i, issues::Analyze>,
    ) -> Result<(), LexerError<'i>> {
        let mut input = bmap! {};
        for pair in pair.into_inner() {
            let span = pair.as_span();
            let v = Var::analyze(pair, issues)?;
            if input.contains_key(&v.name) {
                issues.push_error(SyntaxError::RepeatedVarName(v.name), &span);
            } else {
                input.insert(v.name.clone(), v);
            }
        }
        self.input = input;
        Ok(())
    }
}

trait Analyze<'i> {
    fn analyze(
        pair: Pair<'i, Rule>,
        issues: &mut Issues<'i, issues::Analyze>,
    ) -> Result<Self, LexerError<'i>>
    where
        Self: Sized;
}

impl<'i> Analyze<'i> for Routine<'i> {
    fn analyze(
        pair: Pair<'i, Rule>,
        issues: &mut Issues<'i, issues::Analyze>,
    ) -> Result<Self, LexerError<'i>> {
        let span = pair.as_span();
        let mut iter = pair.into_inner();

        let routine_name = iter.next().ok_or_else(|| LexerError::RoutineNoName(span.to_src()))?;
        let name = match routine_name.as_rule() {
            Rule::routine_main => ".MAIN",
            Rule::routine_name => routine_name.as_str(),
            _ => return Err(LexerError::RoutineUnrecognized(span.to_src())),
        }
        .to_owned();

        let mut labels: std::collections::BTreeMap<String, u16> = bmap![];
        let code =
            iter.enumerate().try_fold(Vec::<Statement>::new(), |mut vec, (index, pair)| {
                let span = pair.as_span();
                let op = Statement::analyze(pair, issues)?;
                if let Some(label) = op.label.clone() {
                    match labels.contains_key(&label.0) {
                        true => {
                            issues.push_error(
                                SyntaxError::RepeatedLabel {
                                    label: label.0,
                                    routine: name.clone(),
                                },
                                &span,
                            );
                        }
                        false => {
                            labels.insert(label.0, index as u16);
                        }
                    }
                }
                vec.push(op);
                Ok(vec)
            })?;

        Ok(Routine { name, labels, statements: code, span })
    }
}

impl<'i> Analyze<'i> for Statement<'i> {
    fn analyze(
        pair: Pair<'i, Rule>,
        issues: &mut Issues<'i, issues::Analyze>,
    ) -> Result<Self, LexerError<'i>> {
        let span = pair.as_span();
        let mut iter = pair.into_inner();

        let label = match iter.peek() {
            Some(pair) if pair.as_rule() == Rule::label => {
                let _ = iter.next();
                Some((pair.as_str().to_owned(), pair.as_span()))
            }
            _ => None,
        };

        let pair = iter.next().ok_or_else(|| LexerError::StatementNoInstruction(span.to_src()))?;
        let mut inner = pair.into_inner();
        let pair = inner.next().ok_or_else(|| LexerError::OperatorMiscomposition(span.to_src()))?;
        let mnemonic = pair.as_str();
        let operator = (
            Operator::from_str(mnemonic).unwrap_or_else(|_| {
                issues.push_error(SyntaxError::UnknownMnemonic(pair.as_str().to_owned()), &pair);
                Operator::nop
            }),
            pair.as_span(),
        );

        let mut flags = FlagSet::None;
        for pair in inner {
            if pair.as_rule() != Rule::flag {
                return Err(LexerError::StatementNoFlag(span.to_src()));
            }
            let chr = pair
                .as_str()
                .chars()
                .next()
                .ok_or_else(|| LexerError::FlagWithoutValue(span.to_src()))?;
            match flags {
                FlagSet::None => flags = FlagSet::One(chr, pair.as_span()),
                FlagSet::One(flag, span) => flags = FlagSet::Double(flag, chr, span),
                FlagSet::Double(_, _, _) => {
                    issues.push_error(SyntaxError::TooManyFlags(mnemonic.to_owned()), &pair)
                }
            }
        }

        let mut operands = vec![];
        for pair in iter {
            operands.push(Operand::analyze(pair, issues)?);
        }

        Ok(Statement { label, operator, flags, operands, span })
    }
}

impl<'i> Analyze<'i> for Operand<'i> {
    fn analyze(
        pair: Pair<'i, Rule>,
        issues: &mut Issues<'i, issues::Analyze>,
    ) -> Result<Self, LexerError<'i>> {
        let span = pair.as_span();
        Ok(match pair.as_rule() {
            Rule::reg => {
                let span = pair.as_span();
                let mut iter = pair.into_inner();
                let family =
                    iter.next().ok_or_else(|| LexerError::RegisterNoType(span.to_src()))?;
                let member = if family.as_rule() != Rule::reg_f16b {
                    iter.next()
                        .ok_or_else(|| LexerError::RegisterNoName(span.to_src()))?
                        .as_str()
                        .parse()
                        .map_err(|err| LexerError::RegisterNameNonDecimal(span.to_src(), err))?
                } else {
                    0u16
                };

                let index =
                    iter.next().ok_or_else(|| LexerError::RegisterNoIndex(span.to_src()))?.as_str();
                let index: u8 = index.parse().map_err(|err| {
                    LexerError::RegisterIndexNonDecimal(span.to_src(), err, index)
                })?;
                let index = index
                    .checked_sub(1)
                    .ok_or(OverflowError { max: 0, value: 0 })
                    .and_then(u5::try_from)
                    .unwrap_or_else(|_| {
                        issues.push_error(SyntaxError::RegisterIndexOutOfRange(index), &span);
                        u5::with(0)
                    });
                let set = match family.as_rule() {
                    Rule::reg_a => {
                        let a = RegA::with(member).unwrap_or_else(|| {
                            issues.push_error(
                                SyntaxError::WrongRegister { family: RegBlock::A, member },
                                &span,
                            );
                            RegA::A1024
                        });
                        RegAll::A(a)
                    }
                    Rule::reg_f16b => RegAll::F(RegF::F16B),
                    Rule::reg_f => {
                        let f = RegF::with(member, false).unwrap_or_else(|| {
                            issues.push_error(
                                SyntaxError::WrongRegister { family: RegBlock::F, member },
                                &span,
                            );
                            RegF::F128
                        });
                        RegAll::F(f)
                    }
                    Rule::reg_r => {
                        let r = RegR::with(member).unwrap_or_else(|| {
                            issues.push_error(
                                SyntaxError::WrongRegister { family: RegBlock::R, member },
                                &span,
                            );
                            RegR::R256
                        });
                        RegAll::R(r)
                    }
                    Rule::reg_s => {
                        if member != 16 {
                            issues.push_error(
                                SyntaxError::WrongRegister { family: RegBlock::S, member },
                                &span,
                            );
                        }
                        RegAll::S
                    }
                    _ => return Err(LexerError::RegisterUnknown(family.to_src())),
                };
                Operand::Reg { set, index: index.into(), span }
            }
            Rule::call => {
                let span = pair.as_span();
                let mut iter = pair.into_inner();
                let lib = iter
                    .next()
                    .ok_or_else(|| LexerError::CallWithoutLibName(span.to_src()))?
                    .as_str()
                    .to_owned();
                let routine = iter
                    .next()
                    .ok_or_else(|| LexerError::CallWithoutRoutineName(span.to_src()))?
                    .as_str()
                    .to_owned();

                Operand::Call { lib, routine, span }
            }
            Rule::lit => Operand::Lit(Literal::analyze(pair, issues)?, span),
            Rule::var => Operand::Const(pair.as_str().to_owned(), span),
            Rule::goto => Operand::Goto(pair.as_str().to_owned(), span),
            _ => return Err(LexerError::OperandUnknown(span.to_src(), pair.as_str())),
        })
    }
}

impl<'i> Analyze<'i> for Literal {
    fn analyze(
        pair: Pair<'i, Rule>,
        issues: &mut Issues<'i, issues::Analyze>,
    ) -> Result<Self, LexerError<'i>> {
        let span = pair.as_span();
        let pair =
            pair.into_inner().next().ok_or_else(|| LexerError::LiteralNoData(span.to_src()))?;
        Ok(match pair.as_rule() {
            Rule::lit_dec => {
                let val = pair.as_str();
                Literal::Int(
                    u128::from_str(val)
                        .map_err(|err| LexerError::LiteralWrongDec(span.to_src(), val, err))?
                        .into(),
                    IntBase::Dec,
                )
            }
            Rule::lit_bin => {
                let val = pair.as_str();
                Literal::Int(
                    u128::from_str_radix(&val[2..], 2)
                        .map_err(|err| LexerError::LiteralWrongBin(span.to_src(), val, err))?
                        .into(),
                    IntBase::Bin,
                )
            }
            Rule::lit_oct => {
                let val = pair.as_str();
                Literal::Int(
                    u128::from_str_radix(&val[2..], 8)
                        .map_err(|err| LexerError::LiteralWrongOct(span.to_src(), val, err))?
                        .into(),
                    IntBase::Oct,
                )
            }
            Rule::lit_hex => {
                let val = pair.as_str();
                let mut hex = Vec::from_hex(&val[2..])
                    .map_err(|err| LexerError::LiteralWrongHex(span.to_src(), val, err))?;
                if hex.len() < 128 {
                    let mut vec = vec![0; 128];
                    vec[128 - hex.len()..].copy_from_slice(&hex);
                    hex = vec;
                }
                let i = u1024::from_be_slice(&hex).unwrap_or_else(|_| {
                    issues.push_error(SyntaxError::TooBigInt(val.to_owned()), &pair);
                    u1024::MIN
                });
                Literal::Int(i, IntBase::Hex)
            }
            Rule::lit_float => {
                let mut iter = pair.into_inner();
                let significand_int = iter
                    .next()
                    .ok_or_else(|| LexerError::FloatNoWhole(span.to_src()))?
                    .as_str()
                    .parse()
                    .map_err(|err| LexerError::FloatWholeNotNumber(span.to_src(), err))?;
                let significand_rem = iter
                    .next()
                    .ok_or_else(|| LexerError::FloatNoFraction(span.to_src()))?
                    .as_str()
                    .parse()
                    .map_err(|err| LexerError::FloatFractionNotNumber(span.to_src(), err))?;
                let exponential = iter
                    .next()
                    .as_ref()
                    .map(Pair::as_str)
                    .unwrap_or("0")
                    .parse()
                    .map_err(|err| LexerError::FloatExponentialNotNumber(span.to_src(), err))?;
                Literal::Float(significand_int, significand_rem, exponential)
            }
            Rule::lit_str => {
                let s = pair.as_str();
                Literal::String(replace_special_chars(&s[1..s.len() - 1]))
            }
            Rule::lit_chr => {
                let s = pair.as_str();
                let s = replace_special_chars(&s[1..s.len() - 1]);
                if s.len() != 1 {
                    issues.push_error(SyntaxError::InvalidCharLiteral(s.clone()), &pair)
                }
                Literal::Char(s.as_bytes()[1])
            }
            x => return Err(LexerError::LiteralUnknown(span.to_src(), x)),
        })
    }
}

fn replace_special_chars(s: &str) -> String {
    s.replace("\\n", "\n")
        .replace("\\r", "\r")
        .replace("\\t", "\t")
        .replace("\\0", "\0")
        .replace("\\\"", "\"")
        .replace("\\'", "'")
        .replace("\\\\", "\\")
    // TODO: Remove hex and unicode escape sequences
}

impl<'i> Analyze<'i> for Const<'i> {
    fn analyze(
        pair: Pair<'i, Rule>,
        issues: &mut Issues<'i, issues::Analyze>,
    ) -> Result<Self, LexerError<'i>> {
        let span = pair.as_span();
        let mut iter = pair.into_inner();
        let name =
            iter.next().ok_or_else(|| LexerError::ConstNoName(span.to_src()))?.as_str().to_owned();
        let value = iter.next().ok_or_else(|| LexerError::ConstNoValue(span.to_src()))?;
        let value = Literal::analyze(value, issues)?;
        Ok(Const { name, value, span })
    }
}

impl<'i> Analyze<'i> for Var<'i> {
    fn analyze(
        pair: Pair<'i, Rule>,
        issues: &mut Issues<'i, issues::Analyze>,
    ) -> Result<Self, LexerError<'i>> {
        let span = pair.as_span();
        let mut iter = pair.into_inner();
        let name =
            iter.next().ok_or_else(|| LexerError::VarNoName(span.to_src()))?.as_str().to_owned();
        let ty = match iter
            .next()
            .ok_or_else(|| LexerError::VarNoType(span.to_src()))?
            .as_str()
            .to_lowercase()
            .as_str()
        {
            "bytes" => VarType::Bytes,
            "str" => VarType::Str,
            "u8" => VarType::Int(IntLayout::unsigned(1)),
            "u16" => VarType::Int(IntLayout::unsigned(2)),
            "u32" => VarType::Int(IntLayout::unsigned(4)),
            "u64" => VarType::Int(IntLayout::unsigned(8)),
            "u128" => VarType::Int(IntLayout::unsigned(16)),
            "u256" => VarType::Int(IntLayout::unsigned(32)),
            "u512" => VarType::Int(IntLayout::unsigned(64)),
            "u1024" => VarType::Int(IntLayout::unsigned(128)),
            "u2048" => VarType::Int(IntLayout::unsigned(256)),
            "u4096" => VarType::Int(IntLayout::unsigned(512)),
            "u8192" => VarType::Int(IntLayout::unsigned(1024)),
            "i8" => VarType::Int(IntLayout::signed(1)),
            "i16" => VarType::Int(IntLayout::signed(2)),
            "i32" => VarType::Int(IntLayout::signed(4)),
            "i64" => VarType::Int(IntLayout::signed(8)),
            "i128" => VarType::Int(IntLayout::signed(16)),
            "i256" => VarType::Int(IntLayout::signed(32)),
            "i512" => VarType::Int(IntLayout::signed(64)),
            "i1024" => VarType::Int(IntLayout::signed(128)),
            "f16b" => VarType::Float(FloatLayout::BFloat16),
            "f16" => VarType::Float(FloatLayout::IeeeHalf),
            "f32" => VarType::Float(FloatLayout::IeeeSingle),
            "f64" => VarType::Float(FloatLayout::IeeeDouble),
            "f80" => VarType::Float(FloatLayout::X87DoubleExt),
            "f128" => VarType::Float(FloatLayout::IeeeQuad),
            "f256" => VarType::Float(FloatLayout::IeeeOct),
            "apfloat" => VarType::Float(FloatLayout::FloatTapered),
            unknown => return Err(LexerError::VarTypeUnknown(unknown.to_owned(), span.to_src())),
        };
        let mut value = iter.next().ok_or_else(|| LexerError::VarNoDescription(span.to_src()))?;
        let default = match value.as_rule() {
            Rule::input_default => {
                let default = Literal::analyze(
                    value
                        .into_inner()
                        .next()
                        .ok_or_else(|| LexerError::LiteralNoData(span.to_src()))?,
                    issues,
                )?;
                value = iter.next().ok_or_else(|| LexerError::VarNoDescription(span.to_src()))?;
                Some(default)
            }
            _ => None,
        };
        let value = Literal::analyze(value, issues)?;
        match value {
            Literal::String(info) => Ok(Var { name, ty, info, default, span }),
            _ => Err(LexerError::VarWrongDescription(span.to_src())),
        }
    }
}
