// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::fmt::{self, Debug, Display, Formatter, Write};

use aluvm::isa::{BytecodeError, ParseFlagError};
use aluvm::libs::{LibIdError, LibSegOverflow, WriteError};
use aluvm::reg::RegBlock;
use aluvm::Isa;
use pest::iterators::Pair;
use pest::Span;

use crate::ast::Operator;
use crate::compiler::LibError;
use crate::parser::Rule;

pub trait Issue: Debug + Display {
    fn errno(&self) -> u16;
    fn is_error(&self) -> bool;
}

pub trait Stage {
    type Error: std::error::Error + Issue;
    type Warning: Issue;
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Default)]
pub struct Syntax;
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Default)]
pub struct Compile;
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Default)]
pub struct Linking;

impl Stage for Syntax {
    type Error = SyntaxError;
    type Warning = SyntaxWarning;
}

impl Stage for Compile {
    type Error = CompileError;
    type Warning = CompileWarning;
}

impl Stage for Linking {
    type Error = LinkingError;
    type Warning = LinkingWarning;
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display, Error, From)]
#[display(doc_comments)]
pub enum SyntaxError {
    /// re-definition of `{0}` routine
    RoutineNameReuse(String),

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

    /// re-definition of `{0}` constant
    RepeatedConstName(String),

    /// re-definition of `{0}` input variable
    RepeatedVarName(String),
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display, Error, From)]
#[display(doc_comments)]
pub enum CompileError {
    /// operator `{operator}` requires {expected} as {pos} operand
    OperandWrongType { operator: Operator, pos: u8, expected: &'static str },

    /// operator `{operator}` requires different register type ({expected}) as its {pos} operand
    OperandWrongReg { operator: Operator, pos: u8, expected: &'static str },

    /// operator `{0}` can be applied to registers of the same type only
    OperandRegMutBeEqual(Operator),

    /// operator `{operator}` requires {expected} operand at position {pos}
    OperandMissed { operator: Operator, pos: u8, expected: &'static str },

    /// unrecognized flag `{1}` for operator `{0}`
    OperatorWrongFlag(Operator, char),

    /// operator `{0}` requires flag
    OperatorRequiresFlag(Operator),

    #[from]
    #[display(inner)]
    FlagError(ParseFlagError),

    /// program code exceeds maximum length
    CodeLengthOverflow,

    /// program data exceeds maximum length
    DataLengthOverflow,

    /// list of external libraries does not fit into the maximum library segment length
    #[from(LibSegOverflow)]
    LibsLengthOverflow,

    /// integer literal for `{0}` must has value smaller than 32768
    StepTooLarge(Operator),

    /// unknown constant name `{0}`; consider adding constant to .CONST segment
    ConstUnknown(String),

    /// constant `{name}` is {found}, while {expected} is required in this position
    ConstWrongType { name: String, expected: &'static str, found: &'static str },

    /// undeclared library name `{0}`
    LibUnknown(String),

    #[from]
    #[display(inner)]
    LibError(LibError),

    /// call to an unknown routine `{0}`
    RoutineUnknown(String),
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display, Error, From)]
#[display(doc_comments)]
pub enum LinkingError {}

impl Issue for SyntaxError {
    fn errno(&self) -> u16 {
        match self {
            SyntaxError::RoutineNameReuse(_) => 1,
            SyntaxError::UnknownMnemonic(_) => 2,
            SyntaxError::RepeatedLabel { .. } => 3,
            SyntaxError::WrongRegister { .. } => 4,
            SyntaxError::TooManyFlags(_) => 5,
            SyntaxError::TooBigInt(_) => 6,
            SyntaxError::InvalidCharLiteral(_) => 7,
            SyntaxError::UnknownIsa(_) => 8,
            SyntaxError::RegisterIndexOutOfRange(_) => 9,
            SyntaxError::RepeatedLibName(_) => 11,
            SyntaxError::WrongLibId(_, _) => 12,
            SyntaxError::RepeatedConstName(_) => 13,
            SyntaxError::RepeatedVarName(_) => 14,
        }
    }

    #[inline]
    fn is_error(&self) -> bool { true }
}

impl Issue for CompileError {
    fn errno(&self) -> u16 {
        match self {
            CompileError::OperandWrongType { .. } => 15,
            CompileError::OperandWrongReg { .. } => 16,
            CompileError::OperandMissed { .. } => 17,
            CompileError::OperatorWrongFlag(..) => 18,
            CompileError::OperatorRequiresFlag(..) => 19,
            CompileError::FlagError(err) => match err {
                ParseFlagError::UnknownFlag(_, _) => 20,
                ParseFlagError::UnknownFlags(_, _) => 21,
                ParseFlagError::MutuallyExclusiveFlags(_, _, _) => 22,
                ParseFlagError::RequiredFlagAbsent(_) => 23,
                ParseFlagError::DuplicatedFlags(_, _) => 24,
            },
            CompileError::OperandRegMutBeEqual(_) => 29,
            CompileError::CodeLengthOverflow => 25,
            CompileError::DataLengthOverflow => 26,
            CompileError::LibsLengthOverflow => 27,
            CompileError::StepTooLarge(_) => 28,
            CompileError::ConstUnknown(_) => 30,
            CompileError::ConstWrongType { .. } => 31,
            CompileError::LibUnknown(_) => 32,
            CompileError::LibError(err) => match err {
                LibError::LibNotFound(_, _) => 33,
                LibError::TooManyRoutines => 34,
            },
            CompileError::RoutineUnknown(_) => 35,
        }
    }

    #[inline]
    fn is_error(&self) -> bool { true }
}

impl Issue for LinkingError {
    fn errno(&self) -> u16 { 0 }

    #[inline]
    fn is_error(&self) -> bool { true }
}

impl From<BytecodeError> for CompileError {
    fn from(err: BytecodeError) -> Self {
        match err {
            BytecodeError::Write(err) => match err {
                WriteError::CodeNotFittingSegment => CompileError::CodeLengthOverflow,
                WriteError::DataExceedsLimit(_) => CompileError::DataLengthOverflow,
                WriteError::DataNotFittingSegment => CompileError::DataLengthOverflow,
                WriteError::LibAbsent(_) => unreachable!("internal compiler error I0002"),
            },
            BytecodeError::PutNoNumber => unreachable!("internal compiler error I0001"),
        }
    }
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display)]
#[display(doc_comments)]
pub enum SyntaxWarning {
    /// duplicated ISA extension declaration for `{0}`
    DuplicatedIsa(Isa),
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display)]
#[display(doc_comments)]
pub enum CompileWarning {}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display)]
#[display(doc_comments)]
pub enum LinkingWarning {}

impl Issue for SyntaxWarning {
    fn errno(&self) -> u16 {
        match self {
            SyntaxWarning::DuplicatedIsa(_) => 10,
        }
    }

    #[inline]
    fn is_error(&self) -> bool { false }
}

impl Issue for CompileWarning {
    fn errno(&self) -> u16 { 0 }

    #[inline]
    fn is_error(&self) -> bool { false }
}

impl Issue for LinkingWarning {
    fn errno(&self) -> u16 { 0 }

    #[inline]
    fn is_error(&self) -> bool { false }
}

#[derive(Clone, Debug)]
pub struct Issues<'i, S>
where
    S: Stage,
{
    errors: Vec<(S::Error, Src<'i>)>,
    warnings: Vec<(S::Warning, Src<'i>)>,
}

impl<'i, S> Default for Issues<'i, S>
where
    S: Stage,
{
    #[inline]
    fn default() -> Self { Issues { errors: vec![], warnings: vec![] } }
}

impl<'i, S> Display for Issues<'i, S>
where
    S: Stage,
{
    fn fmt<'f>(&self, f: &mut Formatter<'f>) -> fmt::Result {
        fn _f(f: &mut Formatter<'_>, issue: &impl Issue, src: &Src) -> fmt::Result {
            write!(
                f,
                "{} E{:04}:\x1B[0m \x1B[1;15m{}\x1B[0m",
                if issue.is_error() { "\x1B[1;31mError" } else { "\x1B[1;33mWarning" },
                issue.errno(),
                issue
            )?;
            Display::fmt(src, f)
        }

        for (error, src) in &self.errors {
            _f(f, error, src)?;
        }

        for (warning, src) in &self.warnings {
            _f(f, warning, src)?;
        }

        Ok(())
    }
}

impl<'i, S> Issues<'i, S>
where
    S: Stage,
{
    pub fn push_error(&mut self, error: S::Error, span: &impl ToSrc<'i>) {
        self.errors.push((error, span.to_src()));
    }
    pub fn push_warning(&mut self, warning: S::Warning, span: &impl ToSrc<'i>) {
        self.warnings.push((warning, span.to_src()));
    }
    pub fn has_errors(&self) -> bool { !self.errors.is_empty() }
    pub fn count_errors(&self) -> usize { self.errors.len() }
    pub fn count_warnings(&self) -> usize { self.warnings.len() }
}

pub trait ToSrc<'i> {
    fn to_src(&self) -> Src<'i>;
}

impl<'i> ToSrc<'i> for Span<'i> {
    #[inline]
    fn to_src(&self) -> Src<'i> { Src(self.clone()) }
}

impl<'i> ToSrc<'i> for &Span<'i> {
    #[inline]
    fn to_src(&self) -> Src<'i> { Src((*self).clone()) }
}

impl<'i> ToSrc<'i> for Pair<'i, Rule> {
    #[inline]
    fn to_src(&self) -> Src<'i> {
        let span = self.as_span();
        span.to_src()
    }
}

#[derive(Clone)]
pub struct Src<'i>(Span<'i>);

impl<'i> Src<'i> {
    #[inline]
    pub fn as_span(&self) -> &Span<'i> { &self.0 }
}

impl<'i> Display for Src<'i> {
    fn fmt<'f>(&self, f: &mut Formatter<'f>) -> fmt::Result {
        if f.alternate() {
            f.write_str(self.0.as_str())?;
            return Ok(());
        }

        let (line, col) = self.0.start_pos().line_col();
        writeln!(f, "\n\x1B[1;34m   --> line {}, column {}", line, col)?;
        writeln!(f, "\x1B[1;34m     |\x1B[0m")?;
        for (index, s) in self.0.lines().enumerate() {
            write!(f, "\x1B[1;34m{:>4} |\x1B[0m {}", line + index, s)?;
            write!(f, "\x1B[1;34m     |\x1B[1;31m{:1$}", "", col)?;
            for _ in 0..(self.0.end() - self.0.start()) {
                f.write_char('^')?;
            }
            f.write_str("\x1B[0m\n")?;
        }
        writeln!(f, "")
    }
}

impl<'i> Debug for Src<'i> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result { Display::fmt(self, f) }
}
