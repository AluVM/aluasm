// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

//! Analyzer code coverting abstract parse tree, returned by parser, into an
//! abstract syntax tree (AST)

use std::collections::{BTreeMap, BTreeSet};
use std::str::FromStr;

use aluvm::libs::LibId;
use aluvm::reg::{RegA, RegAll, RegBlock, RegF, RegR};
use aluvm::Isa;
use amplify::hex::FromHex;
use amplify::num::{u1024, u5};
use pest::iterators::Pair;
use pest::Span;

use crate::ast::{
    Const, FlagSet, Goto, Instruction, IntBase, Literal, Offset, Operand,
    Operator, Routine, Var,
};
use crate::Rule;

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display)]
#[display(doc_comments)]
pub enum Error {
    /// re-definition of `{0}` routine
    RepeatedRoutineName(String),

    /// unknown operation: `{0}`
    UnknownMnemonic(String),

    /// repeated label `{label}` inside `{routine}` routine
    RepeatedLabel { label: String, routine: String },

    /// wrong register name `{family}{member}`
    WrongRegister { family: RegBlock, member: u16 },

    /// operator `{0}` has too many flags (more than 2)
    TooManyFlags(String),

    /// integer value `{0}` doesn't fit 1024 bits
    TooBigInt(String),

    /// not a character literal: `{0}`
    InvalidCharLiteral(String),
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display)]
#[display(doc_comments)]
pub enum Warning {}

#[derive(Clone, Hash, Default, Debug)]
pub struct Issues<'i> {
    pub errors: Vec<(Error, Span<'i>)>,
    pub warnings: Vec<(Warning, Span<'i>)>,
}

impl<'i> Issues<'i> {
    pub fn push_error(&mut self, error: Error, span: Span<'i>) {
        self.errors.push((error, span));
    }
}

#[derive(Clone, Hash, Default, Debug)]
pub struct Program<'i> {
    pub isae: BTreeSet<Isa>,
    pub libs: BTreeMap<String, LibId>,
    pub main: Option<Routine>,
    pub code: BTreeMap<String, Routine>,
    pub r#const: BTreeMap<String, Const>,
    pub input: BTreeMap<String, Var>,
    pub issues: Issues<'i>,
}

impl<'i> Program<'i> {
    pub fn analyze(pair: Pair<'i, Rule>) -> Self {
        let mut program = Program::default();
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::isae => program.analyze_isae(pair),
                Rule::routine => program.analyze_routine(pair),
                Rule::libs => program.analyze_libs(pair),
                Rule::data => program.analyze_const(pair),
                Rule::input => program.analyze_input(pair),
                Rule::EOI => {}
                _ => unreachable!(
                    "lexer error: unknown program segment `{:?}`",
                    pair.as_rule()
                ),
            };
        }
        program
    }
}

impl<'i> Program<'i> {
    fn analyze_isae(&mut self, pair: Pair<'i, Rule>) {
        for pair in pair.into_inner() {}
    }

    fn analyze_routine(&mut self, pair: Pair<'i, Rule>) {
        let span = pair.as_span();
        let routine = Routine::analyze(pair, &mut self.issues);
        if self.code.contains_key(&routine.name) {
            self.issues
                .push_error(Error::RepeatedRoutineName(routine.name), span);
        } else {
            self.code.insert(routine.name.clone(), routine);
        }
    }

    fn analyze_libs(&mut self, pair: Pair<'i, Rule>) {
        for pair in pair.into_inner() {}
    }

    fn analyze_const(&mut self, pair: Pair<'i, Rule>) {
        for pair in pair.into_inner() {}
    }

    fn analyze_input(&mut self, pair: Pair<'i, Rule>) {
        for pair in pair.into_inner() {}
    }
}

trait Analyze {
    fn analyze<'i>(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self;
}

impl Analyze for Routine {
    fn analyze<'i>(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        let mut iter = pair.into_inner();

        let routine_name =
            iter.next().expect("lexer error: routine must always has a name");
        let name = match routine_name.as_rule() {
            Rule::routine_main => ".MAIN",
            Rule::routine_name => routine_name.as_str(),
            _ => unreachable!(
                "lexer error: routine must always start with a name"
            ),
        }
        .to_owned();

        let mut labels = bmap![];
        let code: Vec<Instruction> = iter
            .enumerate()
            .map(|(index, pair)| {
                let span = pair.as_span();
                let op = Instruction::analyze(pair, issues);
                if let Some(label) = op.label.clone() {
                    if labels.contains_key(&label) {
                        issues.push_error(
                            Error::RepeatedLabel {
                                label,
                                routine: name.clone(),
                            },
                            span,
                        );
                    } else {
                        labels.insert(label, index as u16);
                    }
                }
                op
            })
            .collect();

        Routine { name, labels, code }
    }
}

impl Analyze for Instruction {
    fn analyze<'i>(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        let mut iter = pair.into_inner();
        let label = match iter.peek() {
            Some(pair) if pair.as_rule() == Rule::label => {
                let _ = iter.next();
                Some(pair.as_str().to_owned())
            }
            _ => None,
        };
        let pair = iter
            .next()
            .expect("lexer error: label not followed by an instruction");
        let mnemonic = pair.as_str();
        let operator = Operator::from_str(mnemonic).unwrap_or_else(|_| {
            issues.push_error(
                Error::UnknownMnemonic(pair.as_str().to_owned()),
                pair.as_span(),
            );
            Operator::nop
        });

        let mut flags = FlagSet::None;
        let mut operands = vec![];
        for pair in iter {
            match pair.as_rule() {
                Rule::flag => {
                    let chr = pair
                        .as_str()
                        .chars()
                        .nth(0)
                        .expect("lexer error: flag without flag value");
                    match flags {
                        FlagSet::None => flags = FlagSet::One(chr),
                        FlagSet::One(flag) => {
                            flags = FlagSet::Double(flag, chr)
                        }
                        FlagSet::Double(f1, f2) => issues.push_error(
                            Error::TooManyFlags(mnemonic.to_owned()),
                            pair.as_span(),
                        ),
                    }
                }
                _ => operands.push(Operand::analyze(pair, issues)),
            }
        }

        Instruction { label, operator, flags, operands }
    }
}

impl Analyze for Operand {
    fn analyze<'i>(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        match pair.as_rule() {
            Rule::reg => {
                let span = pair.as_span();
                let mut iter = pair.into_inner();
                let family = iter.next().expect(
                    "lexer error: register operand not specifying family",
                );
                let member = if family.as_rule() != Rule::reg_f16b {
                    iter.next()
                        .expect(
                            "lexer error: register operand not specifying \
                             member",
                        )
                        .as_str()
                        .parse()
                        .expect(
                            "lexer error: register family member must be \
                             encoded as decimal number",
                        )
                } else {
                    0u16
                };
                let index: u5 = iter
                    .next()
                    .expect(
                        "lexer error: register operand not specifying index",
                    )
                    .as_str()
                    .parse()
                    .expect("lexer error: register index is not an integer");
                let set = match family.as_rule() {
                    Rule::reg_a => {
                        let a = RegA::with(member).unwrap_or_else(|| {
                            issues.push_error(
                                Error::WrongRegister {
                                    family: RegBlock::A,
                                    member,
                                },
                                span,
                            );
                            RegA::A1024
                        });
                        RegAll::A(a)
                    }
                    Rule::reg_f16b => RegAll::F(RegF::F16B),
                    Rule::reg_f => {
                        let f =
                            RegF::with(member, false).unwrap_or_else(|| {
                                issues.push_error(
                                    Error::WrongRegister {
                                        family: RegBlock::F,
                                        member,
                                    },
                                    span,
                                );
                                RegF::F128
                            });
                        RegAll::F(f)
                    }
                    Rule::reg_r => {
                        let r = RegR::with(member).unwrap_or_else(|| {
                            issues.push_error(
                                Error::WrongRegister {
                                    family: RegBlock::R,
                                    member,
                                },
                                span,
                            );
                            RegR::R256
                        });
                        RegAll::R(r)
                    }
                    Rule::reg_s => {
                        if member != 16 {
                            issues.push_error(
                                Error::WrongRegister {
                                    family: RegBlock::S,
                                    member,
                                },
                                span,
                            );
                        }
                        RegAll::S
                    }
                    _ => panic!(
                        "lexer error: unexpected register family {}",
                        family.as_str()
                    ),
                };
                Operand::Reg { set, index: index.into() }
            }
            Rule::call => Operand::Call(pair.as_str().to_owned()),
            Rule::lit => Operand::Lit(Literal::analyze(pair, issues)),
            Rule::var => Operand::Const(pair.as_str().to_owned()),
            Rule::goto => Operand::Goto(Goto::analyze(pair, issues)),
            _ => panic!("lexer error: unexpected operand {}", pair.as_str()),
        }
    }
}

impl Analyze for Literal {
    fn analyze<'i>(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        let pair = pair
            .into_inner()
            .next()
            .expect("lexer error: literal without inner data");
        match pair.as_rule() {
            Rule::lit_dec => {
                let val = pair.as_str();
                Literal::Int(
                    u128::from_str(val)
                        .expect(&format!(
                            "lexer error: wrong decimal integer {}",
                            val
                        ))
                        .into(),
                    IntBase::Dec,
                )
            }
            Rule::lit_bin => {
                let val = pair.as_str();
                Literal::Int(
                    u128::from_str_radix(&val[2..], 2)
                        .expect(&format!(
                            "lexer error: wrong binary integer {}",
                            val
                        ))
                        .into(),
                    IntBase::Bin,
                )
            }
            Rule::lit_oct => {
                let val = pair.as_str();
                Literal::Int(
                    u128::from_str_radix(&val[2..], 8)
                        .expect(&format!(
                            "lexer error: wrong octal integer {}",
                            val
                        ))
                        .into(),
                    IntBase::Oct,
                )
            }
            Rule::lit_hex => {
                let val = pair.as_str();
                let hex = Vec::from_hex(&val[2..])
                    .expect(&format!("lexer error: wrong hex integer {}", val));
                let i = u1024::from_be_slice(&hex).unwrap_or_else(|_| {
                    issues.push_error(
                        Error::TooBigInt(val.to_owned()),
                        pair.as_span(),
                    );
                    u1024::MIN
                });
                Literal::Int(i, IntBase::Hex)
            }
            Rule::lit_float => {
                let mut iter = pair.into_inner();
                let significand_int = iter
                    .next()
                    .expect(
                        "lexer error: float without integer significand part",
                    )
                    .as_str()
                    .parse()
                    .expect(
                        "lexer error: float integer significand contains \
                         wrong digits",
                    );
                let significand_rem = iter
                    .next()
                    .expect(
                        "lexer error: float without significand reminder part",
                    )
                    .as_str()
                    .parse()
                    .expect(
                        "lexer error: float significand reminder contains \
                         wrong digits",
                    );
                let exponential = iter
                    .next()
                    .as_ref()
                    .map(Pair::as_str)
                    .unwrap_or("0")
                    .parse()
                    .expect("lexer error: float exponential is not an integer");
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
                    issues.push_error(
                        Error::InvalidCharLiteral(s.clone()),
                        pair.as_span(),
                    )
                }
                Literal::Char(s.as_bytes()[1])
            }
            x => panic!(
                "lexer error: unexpected type `{:?}` for literal {}",
                x,
                pair.as_str()
            ),
        }
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

impl Analyze for Goto {
    fn analyze<'i>(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        let mut iter = pair.into_inner();
        let inner = iter.next().expect(
            "lexer error: goto statement must contain either label name or an \
             offset",
        );
        match inner.as_rule() {
            Rule::goto_name => Goto::Label(inner.as_str().to_owned()),
            _ => {
                let offset = iter
                    .next()
                    .expect(
                        "lexer error: offset-based goto statement must \
                         contain offset value",
                    )
                    .as_str()
                    .parse()
                    .expect(
                        "lexer error: offset in goto statement must be a \
                         valid 16-bit integer",
                    );
                match inner.as_rule() {
                    Rule::goto_exact => Goto::Offset(Offset::Exact(offset)),
                    Rule::goto_fwd => Goto::Offset(Offset::Forward(offset)),
                    Rule::goto_rev => Goto::Offset(Offset::Backward(offset)),
                    x => panic!("lexer error: unknown goto rule `{:?}`", x),
                }
            }
        }
    }
}
