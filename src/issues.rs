// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::fmt::{self, Debug, Display, Formatter, Write};

use aluvm::isa::{BytecodeError, ParseFlagError};
use aluvm::libs::{LibIdError, WriteError};
use aluvm::reg::RegBlock;
use aluvm::Isa;
use pest::Span;

use crate::ast::Operator;

pub trait Issue: Debug + Display {
    fn errno(&self) -> u16;
    fn is_error(&self) -> bool;
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display, Error, From)]
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

    /// re-definition of `{0}` constant
    RepeatedConstName(String),

    /// re-definition of `{0}` input variable
    RepeatedVarName(String),

    /// operator `{operator}` requires {expected} as {pos} operand
    OperandWrongType { operator: Operator, pos: u8, expected: &'static str },

    /// operator `{operator}` requires different register type ({expected}) as its {pos} operand
    OperandWrongReg { operator: Operator, pos: u8, expected: &'static str },

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
            Error::RepeatedConstName(_) => 13,
            Error::RepeatedVarName(_) => 14,
            Error::OperandWrongType { .. } => 15,
            Error::OperandWrongReg { .. } => 16,
            Error::OperandMissed { .. } => 17,
            Error::OperatorWrongFlag(..) => 18,
            Error::OperatorRequiresFlag(..) => 19,
            Error::FlagError(err) => match err {
                ParseFlagError::UnknownFlag(_, _) => 20,
                ParseFlagError::UnknownFlags(_, _) => 21,
                ParseFlagError::MutuallyExclusiveFlags(_, _, _) => 22,
                ParseFlagError::RequiredFlagAbsent(_) => 23,
                ParseFlagError::DuplicatedFlags(_, _) => 24,
            },
            Error::CodeLengthOverflow => 25,
            Error::DataLengthOverflow => 26,
        }
    }

    #[inline]
    fn is_error(&self) -> bool { true }
}

impl From<BytecodeError> for Error {
    fn from(err: BytecodeError) -> Self {
        match err {
            BytecodeError::Write(err) => match err {
                WriteError::CodeNotFittingSegment => Error::CodeLengthOverflow,
                WriteError::DataExceedsLimit(_) => Error::DataLengthOverflow,
                WriteError::DataNotFittingSegment => Error::DataLengthOverflow,
                WriteError::LibAbsent(_) => unreachable!("internal compiler error I0002"),
            },
            BytecodeError::PutNoNumber => unreachable!("internal compiler error I0001"),
        }
    }
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
        }

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
}
