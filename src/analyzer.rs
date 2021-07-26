// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

//! Analyzer code coverting abstract parse tree, returned by parser, into an
//! abstract syntax tree (AST)

use std::collections::{BTreeMap, BTreeSet};
use std::convert::TryFrom;
use std::fmt::{self, Debug, Display, Formatter, Write};
use std::str::FromStr;

use aluvm::libs::{LibId, LibIdError};
use aluvm::reg::{RegA, RegAll, RegBlock, RegF, RegR};
use aluvm::Isa;
use amplify::hex::FromHex;
use amplify::num::error::OverflowError;
use amplify::num::{u1024, u5};
use pest::iterators::Pair;
use pest::Span;

use crate::ast::{
    Const, FlagSet, Goto, Instruction, IntBase, Literal, Offset, Operand, Operator, Routine, Var,
};
use crate::Rule;

pub trait Issue: Debug + Display {
    fn errno(&self) -> u16;
    fn is_error(&self) -> bool;
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display)]
#[display(doc_comments)]
pub enum Error {
    /// re-definition of `{0}` routine
    RepeatedRoutineName(String),

    /// unknown operation mnemonic `{0}`
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

    /// unknown ISA extension ID `{0}`
    UnknownIsa(String),

    /// register index `{0}` is out of range 1..=32
    RegisterIndexOutOfRange(u8),

    /// re-definition of `{0}` library name
    RepeatedLibName(String),

    /// incorrect library id Bech32 string `{0}` ({1})
    WrongLibId(String, LibIdError),
}

impl Issue for Error {
    fn errno(&self) -> u16 {
        match self {
            Error::RepeatedRoutineName(_) => 1,
            Error::UnknownMnemonic(_) => 2,
            Error::RepeatedLabel { .. } => 3,
            Error::WrongRegister { .. } => 4,
            Error::TooManyFlags(_) => 5,
            Error::TooBigInt(_) => 6,
            Error::InvalidCharLiteral(_) => 7,
            Error::UnknownIsa(_) => 8,
            Error::RegisterIndexOutOfRange(_) => 9,
            Error::RepeatedLibName(_) => 11,
            Error::WrongLibId(_, _) => 12,
        }
    }

    #[inline]
    fn is_error(&self) -> bool { true }
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display)]
#[display(doc_comments)]
pub enum Warning {
    /// duplicated ISA extension declaration for `{0}`
    DuplicatedIsa(Isa),
}

impl Issue for Warning {
    fn errno(&self) -> u16 {
        match self {
            Warning::DuplicatedIsa(_) => 10,
        }
    }

    #[inline]
    fn is_error(&self) -> bool { false }
}

#[derive(Clone, Hash, Default, Debug)]
pub struct Issues<'i> {
    errors: Vec<(Error, Span<'i>)>,
    warnings: Vec<(Warning, Span<'i>)>,
}

impl<'i> Display for Issues<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fn _f(f: &mut Formatter<'_>, issue: &impl Issue, span: &Span) -> fmt::Result {
            let (line, col) = span.start_pos().line_col();
            writeln!(
                f,
                "{} E{:04}:\x1B[0m \x1B[1;15m{}\x1B[0m",
                if issue.is_error() { "\x1B[1;31mError" } else { "\x1B[1;33mWarning" },
                issue.errno(),
                issue
            )?;
            writeln!(f, "\x1B[1;34m   --> line {}, column {}", line, col)?;
            writeln!(f, "\x1B[1;34m     |\x1B[0m")?;
            for (index, s) in span.lines().enumerate() {
                write!(f, "\x1B[1;34m{:>4} |\x1B[0m {}", line + index, s)?;
                write!(f, "\x1B[1;34m     |\x1B[1;31m{:1$}", "", col)?;
                for _ in 0..(span.end() - span.start()) {
                    f.write_char('^')?;
                }
                f.write_str("\x1B[0m\n")?;
            }
            writeln!(f, "")
        };

        for (error, span) in &self.errors {
            _f(f, error, span)?;
        }

        for (warning, span) in &self.warnings {
            _f(f, warning, span)?;
        }

        Ok(())
    }
}

impl<'i> Issues<'i> {
    pub fn push_error(&mut self, error: Error, span: Span<'i>) { self.errors.push((error, span)); }
    pub fn push_warning(&mut self, warning: Warning, span: Span<'i>) {
        self.warnings.push((warning, span));
    }
    pub fn has_errors(&self) -> bool { !self.errors.is_empty() }
    pub fn has_warnings(&self) -> bool { !self.warnings.is_empty() }
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
        if self.code.contains_key(&routine.name) {
            self.issues.push_error(Error::RepeatedRoutineName(routine.name), span);
        } else {
            self.code.insert(routine.name.clone(), routine);
        }
    }

    fn analyze_libs(&mut self, pair: Pair<'i, Rule>) {
        let issues = &mut self.issues;
        let mut libs = bmap! {};
        for pair in pair.into_inner() {
            let mut iter = pair.into_inner();
            let name = iter.next().expect("lexer error: library statement must start with name");
            let id = iter.next().expect("lexer error: library statement must end with lib id");
            if libs.contains_key(name.as_str()) {
                self.issues
                    .push_error(Error::RepeatedLibName(name.as_str().to_owned()), name.as_span());
            } else {
                match LibId::from_str(id.as_str()) {
                    Ok(libid) => {
                        libs.insert(name.as_str().to_owned(), libid);
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
        self.libs = libs;
    }

    fn analyze_const(&mut self, pair: Pair<'i, Rule>) { for pair in pair.into_inner() {} }

    fn analyze_input(&mut self, pair: Pair<'i, Rule>) { for pair in pair.into_inner() {} }
}

trait Analyze {
    fn analyze<'i>(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self;
}

impl Analyze for Routine {
    fn analyze<'i>(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
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
        let code: Vec<Instruction> = iter
            .enumerate()
            .map(|(index, pair)| {
                let span = pair.as_span();
                let op = Instruction::analyze(pair, issues);
                if let Some(label) = op.label.clone() {
                    if labels.contains_key(&label) {
                        issues.push_error(
                            Error::RepeatedLabel { label, routine: name.clone() },
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

        let pair = iter.next().expect("lexer error: label not followed by an instruction");
        let mut inner = pair.into_inner();
        let pair =
            inner.next().expect("lexer error: operator must be composed of mnemonic and flags");
        let mnemonic = pair.as_str();
        let operator = Operator::from_str(mnemonic).unwrap_or_else(|_| {
            issues.push_error(Error::UnknownMnemonic(pair.as_str().to_owned()), pair.as_span());
            Operator::nop
        });

        let mut flags = FlagSet::None;
        for pair in inner {
            debug_assert!(
                pair.as_rule() == Rule::flag,
                "lexer error: mnemonic is followed by non-flag rules"
            );
            let chr = pair.as_str().chars().nth(0).expect("lexer error: flag without flag value");
            match flags {
                FlagSet::None => flags = FlagSet::One(chr),
                FlagSet::One(flag) => flags = FlagSet::Double(flag, chr),
                FlagSet::Double(f1, f2) => {
                    issues.push_error(Error::TooManyFlags(mnemonic.to_owned()), pair.as_span())
                }
            }
        }

        let mut operands = vec![];
        for pair in iter {
            operands.push(Operand::analyze(pair, issues))
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
                                span,
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
                                span,
                            );
                            RegF::F128
                        });
                        RegAll::F(f)
                    }
                    Rule::reg_r => {
                        let r = RegR::with(member).unwrap_or_else(|| {
                            issues.push_error(
                                Error::WrongRegister { family: RegBlock::R, member },
                                span,
                            );
                            RegR::R256
                        });
                        RegAll::R(r)
                    }
                    Rule::reg_s => {
                        if member != 16 {
                            issues.push_error(
                                Error::WrongRegister { family: RegBlock::S, member },
                                span,
                            );
                        }
                        RegAll::S
                    }
                    _ => {
                        unreachable!("lexer error: unexpected register family {}", family.as_str())
                    }
                };
                Operand::Reg { set, index: index.into() }
            }
            Rule::call => Operand::Call(pair.as_str().to_owned()),
            Rule::lit => Operand::Lit(Literal::analyze(pair, issues)),
            Rule::var => Operand::Const(pair.as_str().to_owned()),
            Rule::goto => Operand::Goto(Goto::analyze(pair, issues)),
            _ => unreachable!("lexer error: unexpected operand {}", pair.as_str()),
        }
    }
}

impl Analyze for Literal {
    fn analyze<'i>(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
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

impl Analyze for Goto {
    fn analyze<'i>(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        let mut iter = pair.into_inner();
        let inner = iter
            .next()
            .expect("lexer error: goto statement must contain either label name or an offset");
        match inner.as_rule() {
            Rule::goto_name => Goto::Label(inner.as_str().to_owned()),
            _ => {
                let offset = iter
                    .next()
                    .expect("lexer error: offset-based goto statement must contain offset value")
                    .as_str()
                    .parse()
                    .expect("lexer error: offset in goto statement must be a valid 16-bit integer");
                match inner.as_rule() {
                    Rule::goto_exact => Goto::Offset(Offset::Exact(offset)),
                    Rule::goto_fwd => Goto::Offset(Offset::Forward(offset)),
                    Rule::goto_rev => Goto::Offset(Offset::Backward(offset)),
                    x => {
                        unreachable!("lexer error: unknown goto rule `{:?}`", x)
                    }
                }
            }
        }
    }
}
