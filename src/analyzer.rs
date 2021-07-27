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

use aluvm::libs::LibId;
use aluvm::reg::{RegA, RegAll, RegBlock, RegF, RegR};
use aluvm::Isa;
use amplify::hex::FromHex;
use amplify::num::error::OverflowError;
use amplify::num::{u1024, u5};
use pest::iterators::Pair;

use crate::ast::{
    Const, FlagSet, IntBase, Libs, Literal, Operand, Operator, Program, Routine, Statement, Var,
};
use crate::{Error, Issues, Rule, Warning};

impl<'i> Program<'i> {
    pub fn analyze(pair: Pair<'i, Rule>) -> Self {
        let mut program = Program {
            isae: Default::default(),
            libs: Libs { map: bmap! {}, span: pair.as_span() },
            main: None,
            routines: Default::default(),
            r#const: Default::default(),
            input: Default::default(),
            issues: Default::default(),
        };
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::isae => program.analyze_isae(pair),
                Rule::routine => program.analyze_routine(pair),
                Rule::libs => program.analyze_libs(pair),
                Rule::data => program.analyze_const(pair),
                Rule::input => program.analyze_input(pair),
                Rule::EOI => {}
                _ => unreachable!("lexer error: unknown program segment `{:?}`", pair.as_rule()),
            };
        }
        program
    }
}

impl<'i> Program<'i> {
    fn analyze_isae(&mut self, pair: Pair<'i, Rule>) {
        let mut set = bset![];
        for pair in pair.into_inner() {
            let mut found = false;
            for isa in Isa::all() {
                if isa.to_string() == pair.as_str() {
                    if set.contains(&isa) {
                        self.issues.push_warning(Warning::DuplicatedIsa(isa), pair.as_span());
                    }
                    set.insert(isa);
                    found = true;
                    break;
                }
            }
            if !found {
                self.issues
                    .push_error(Error::UnknownIsa(pair.as_str().to_string()), pair.as_span());
            }
        }
        self.isae = set;
    }

    fn analyze_routine(&mut self, pair: Pair<'i, Rule>) {
        let span = pair.as_span();
        let routine = Routine::analyze(pair, &mut self.issues);
        if self.routines.contains_key(&routine.name) {
            self.issues.push_error(Error::RoutineNameReuse(routine.name), span);
        } else {
            self.routines.insert(routine.name.clone(), routine);
        }
    }

    fn analyze_libs(&mut self, pair: Pair<'i, Rule>) {
        let span = pair.as_span();
        let mut map = bmap! {};
        for pair in pair.into_inner() {
            let mut iter = pair.into_inner();
            let name = iter.next().expect("lexer error: library statement must start with name");
            let id = iter.next().expect("lexer error: library statement must end with lib id");
            if map.contains_key(name.as_str()) {
                self.issues
                    .push_error(Error::RepeatedLibName(name.as_str().to_owned()), name.as_span());
            } else {
                match LibId::from_str(id.as_str()) {
                    Ok(libid) => {
                        map.insert(name.as_str().to_owned(), libid);
                    }
                    Err(err) => {
                        self.issues.push_error(
                            Error::WrongLibId(id.as_str().to_owned(), err),
                            id.as_span(),
                        );
                    }
                }
            }
        }
        self.libs = Libs { map, span };
    }

    fn analyze_const(&mut self, pair: Pair<'i, Rule>) {
        let issues = &mut self.issues;
        let mut r#const = bmap! {};
        for pair in pair.into_inner() {
            let span = pair.as_span();
            let c = Const::analyze(pair, issues);
            if r#const.contains_key(&c.name) {
                issues.push_error(Error::RepeatedConstName(c.name), span);
            } else {
                r#const.insert(c.name.clone(), c);
            }
        }
        self.r#const = r#const;
    }

    fn analyze_input(&mut self, pair: Pair<'i, Rule>) {
        let issues = &mut self.issues;
        let mut input = bmap! {};
        for pair in pair.into_inner() {
            let span = pair.as_span();
            let v = Var::analyze(pair, issues);
            if r#input.contains_key(&v.name) {
                issues.push_error(Error::RepeatedVarName(v.name), span);
            } else {
                input.insert(v.name.clone(), v);
            }
        }
        self.input = input;
    }
}

trait Analyze<'i> {
    fn analyze(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self;
}

impl<'i> Analyze<'i> for Routine<'i> {
    fn analyze(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        let span = pair.as_span();
        let mut iter = pair.into_inner();

        let routine_name = iter.next().expect("lexer error: routine must always has a name");
        let name = match routine_name.as_rule() {
            Rule::routine_main => ".MAIN",
            Rule::routine_name => routine_name.as_str(),
            _ => unreachable!(
                "lexer error: routine must always start with a either .MAIN or .ROUTINE"
            ),
        }
        .to_owned();

        let mut labels = bmap![];
        let code: Vec<Statement> = iter
            .enumerate()
            .map(|(index, pair)| {
                let span = pair.as_span();
                let op = Statement::analyze(pair, issues);
                if let Some(label) = op.label.clone() {
                    if labels.contains_key(&label.0) {
                        issues.push_error(
                            Error::RepeatedLabel { label: label.0, routine: name.clone() },
                            span,
                        );
                    } else {
                        labels.insert(label.0, index as u16);
                    }
                }
                op
            })
            .collect();

        Routine { name, labels, statements: code, span }
    }
}

impl<'i> Analyze<'i> for Statement<'i> {
    fn analyze(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        let span = pair.as_span();
        let mut iter = pair.into_inner();

        let label = match iter.peek() {
            Some(pair) if pair.as_rule() == Rule::label => {
                let _ = iter.next();
                Some((pair.as_str().to_owned(), pair.as_span()))
            }
            _ => None,
        };

        let pair = iter.next().expect("lexer error: label not followed by an instruction");
        let mut inner = pair.into_inner();
        let pair =
            inner.next().expect("lexer error: operator must be composed of mnemonic and flags");
        let mnemonic = pair.as_str();
        let operator = (
            Operator::from_str(mnemonic).unwrap_or_else(|_| {
                issues.push_error(Error::UnknownMnemonic(pair.as_str().to_owned()), pair.as_span());
                Operator::nop
            }),
            pair.as_span(),
        );

        let mut flags = FlagSet::None;
        for pair in inner {
            debug_assert!(
                pair.as_rule() == Rule::flag,
                "lexer error: mnemonic is followed by non-flag rules"
            );
            let chr = pair.as_str().chars().nth(0).expect("lexer error: flag without flag value");
            match flags {
                FlagSet::None => flags = FlagSet::One(chr, pair.as_span()),
                FlagSet::One(flag, span) => flags = FlagSet::Double(flag, chr, span),
                FlagSet::Double(_, _, _) => {
                    issues.push_error(Error::TooManyFlags(mnemonic.to_owned()), pair.as_span())
                }
            }
        }

        let mut operands = vec![];
        for pair in iter {
            operands.push(Operand::analyze(pair, issues))
        }

        Statement { label, operator, flags, operands, span }
    }
}

impl<'i> Analyze<'i> for Operand<'i> {
    fn analyze(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        let span = pair.as_span();
        match pair.as_rule() {
            Rule::reg => {
                let span = pair.as_span();
                let mut iter = pair.into_inner();
                let family =
                    iter.next().expect("lexer error: register operand not specifying family");
                let member = if family.as_rule() != Rule::reg_f16b {
                    iter.next()
                        .expect("lexer error: register operand not specifying member")
                        .as_str()
                        .parse()
                        .expect(
                            "lexer error: register family member must be encoded as decimal number",
                        )
                } else {
                    0u16
                };

                let index = iter
                    .next()
                    .expect("lexer error: register operand not specifying index")
                    .as_str();
                let index: u8 = index
                    .parse()
                    .expect(&format!("lexer error: register index `{}` is not an integer", index));
                let index = index
                    .checked_sub(1)
                    .ok_or(OverflowError { max: 0, value: 0 })
                    .and_then(u5::try_from)
                    .unwrap_or_else(|_| {
                        issues.push_error(Error::RegisterIndexOutOfRange(index), span.clone());
                        u5::with(0)
                    });
                let set = match family.as_rule() {
                    Rule::reg_a => {
                        let a = RegA::with(member).unwrap_or_else(|| {
                            issues.push_error(
                                Error::WrongRegister { family: RegBlock::A, member },
                                span.clone(),
                            );
                            RegA::A1024
                        });
                        RegAll::A(a)
                    }
                    Rule::reg_f16b => RegAll::F(RegF::F16B),
                    Rule::reg_f => {
                        let f = RegF::with(member, false).unwrap_or_else(|| {
                            issues.push_error(
                                Error::WrongRegister { family: RegBlock::F, member },
                                span.clone(),
                            );
                            RegF::F128
                        });
                        RegAll::F(f)
                    }
                    Rule::reg_r => {
                        let r = RegR::with(member).unwrap_or_else(|| {
                            issues.push_error(
                                Error::WrongRegister { family: RegBlock::R, member },
                                span.clone(),
                            );
                            RegR::R256
                        });
                        RegAll::R(r)
                    }
                    Rule::reg_s => {
                        if member != 16 {
                            issues.push_error(
                                Error::WrongRegister { family: RegBlock::S, member },
                                span.clone(),
                            );
                        }
                        RegAll::S
                    }
                    _ => {
                        unreachable!("lexer error: unexpected register family {}", family.as_str())
                    }
                };
                Operand::Reg { set, index: index.into(), span }
            }
            Rule::call => {
                let span = pair.as_span();
                let mut iter = pair.into_inner();
                let lib = iter
                    .next()
                    .expect("lexer error: call statement without library name")
                    .as_str()
                    .to_owned();
                let routine = iter
                    .next()
                    .expect("lexer error: call statement without routine name")
                    .as_str()
                    .to_owned();

                Operand::Call { lib, routine, span }
            }
            Rule::lit => Operand::Lit(Literal::analyze(pair, issues), span),
            Rule::var => Operand::Const(pair.as_str().to_owned(), span),
            Rule::goto => Operand::Goto(pair.as_str().to_owned(), span),
            _ => unreachable!("lexer error: unexpected operand {} at {:?}", pair.as_str(), span),
        }
    }
}

impl<'i> Analyze<'i> for Literal {
    fn analyze(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        let pair = pair.into_inner().next().expect("lexer error: literal without inner data");
        match pair.as_rule() {
            Rule::lit_dec => {
                let val = pair.as_str();
                Literal::Int(
                    u128::from_str(val)
                        .expect(&format!("lexer error: wrong decimal integer {}", val))
                        .into(),
                    IntBase::Dec,
                )
            }
            Rule::lit_bin => {
                let val = pair.as_str();
                Literal::Int(
                    u128::from_str_radix(&val[2..], 2)
                        .expect(&format!("lexer error: wrong binary integer {}", val))
                        .into(),
                    IntBase::Bin,
                )
            }
            Rule::lit_oct => {
                let val = pair.as_str();
                Literal::Int(
                    u128::from_str_radix(&val[2..], 8)
                        .expect(&format!("lexer error: wrong octal integer {}", val))
                        .into(),
                    IntBase::Oct,
                )
            }
            Rule::lit_hex => {
                let val = pair.as_str();
                let mut hex = Vec::from_hex(&val[2..])
                    .expect(&format!("lexer error: wrong hex integer {}", val));
                if hex.len() < 128 {
                    let mut vec = vec![0; 128];
                    vec[128 - hex.len()..].copy_from_slice(&hex);
                    hex = vec;
                }
                let i = u1024::from_be_slice(&hex).unwrap_or_else(|_| {
                    issues.push_error(Error::TooBigInt(val.to_owned()), pair.as_span());
                    u1024::MIN
                });
                Literal::Int(i, IntBase::Hex)
            }
            Rule::lit_float => {
                let mut iter = pair.into_inner();
                let significand_int = iter
                    .next()
                    .expect("lexer error: float without integer significand part")
                    .as_str()
                    .parse()
                    .expect("lexer error: float integer significand contains wrong digits");
                let significand_rem = iter
                    .next()
                    .expect("lexer error: float without significand reminder part")
                    .as_str()
                    .parse()
                    .expect("lexer error: float significand reminder contains wrong digits");
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
                    issues.push_error(Error::InvalidCharLiteral(s.clone()), pair.as_span())
                }
                Literal::Char(s.as_bytes()[1])
            }
            x => {
                unreachable!("lexer error: unexpected type `{:?}` for literal {}", x, pair.as_str())
            }
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

impl<'i> Analyze<'i> for Const<'i> {
    fn analyze(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        let span = pair.as_span();
        let mut iter = pair.into_inner();
        let name = iter
            .next()
            .expect("lexer error: const statement must start with name")
            .as_str()
            .to_owned();
        let value = iter.next().expect("lexer error: const statement must end with value");
        let value = Literal::analyze(value, issues);
        Const { name, value, span }
    }
}

impl<'i> Analyze<'i> for Var<'i> {
    fn analyze(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        let span = pair.as_span();
        let mut iter = pair.into_inner();
        let name = iter
            .next()
            .expect("lexer error: input variable statement must start with name")
            .as_str()
            .to_owned();
        let value = iter.next().expect(
            "lexer error: input variable statement must contain description as string literal",
        );
        let value = Literal::analyze(value, issues);
        match value {
            Literal::String(info) => Var {
                name,
                info,
                span,
                default: None, // TODO: support defaults in the grammar
            },
            _ => unreachable!("lexer error: input variable description must be a string literal"),
        }
    }
}
