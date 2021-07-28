// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate amplify;

mod model;
mod pipelines;

use std::error::Error;
use std::num::ParseIntError;

use aluvm::isa::{BytecodeError, Instr};
use aluvm::libs::SegmentError;
use amplify::{hex, IoError};
pub use model::{ast, issues, module};
pub use pipelines::{analyzer, compiler, linker, parser};
use rustc_apfloat::ParseError;

use crate::issues::Src;
use crate::parser::Rule;

#[derive(Debug, Display, Error, From)]
pub enum MainError {
    #[display("\x1B[1;31mInternal lexer error L{1:04}:\x1B[0m {0}")]
    Lexer(String, u16),

    #[display("\x1B[1;31mInternal compiler error C{1:04}:\x1B[0m {0}")]
    Internal(InternalError, u16),

    #[display("\x1B[1;31mError:\x1B[0m {0}")]
    #[from]
    Access(BuildError),

    #[display(
        "{1}\n\x1B[1;31mError:\x1B[0m could not compile `{0}` due to a previous parsing error"
    )]
    Parser(String, pest::error::Error<Rule>),

    #[display(
        "{3}\n\x1B[1;31mError:\x1B[0m could not compile `{0}` due to {1} previous syntax \
         error(s); {2} warning(s) emitted"
    )]
    Syntax(String, usize, usize, String),

    #[display(
        "{3}\n\x1B[1;31mError:\x1B[0m could not complete compillation of `{0}` due to {1} \
         previous error(s); {2} warning(s) emitted"
    )]
    Compile(String, usize, usize, String),

    #[display(
        "{3}\n\x1B[1;31mError:\x1B[0m could not link `{0}` due to {1} previous error(s); {2} \
         warning(s) emitted"
    )]
    Linking(String, usize, usize, String),
}

#[derive(Clone, Debug, Display, Error)]
#[display(doc_comments)]
pub enum LexerError<'i> {
    /// unknown program segment `{0:?}`
    UnknownSegment(Rule),

    /// library statement `{0:#}` has no detectable name
    /// {0}
    LibNoName(Src<'i>),

    /// library statement `{0:#}` has no detectable lib id
    /// {0}
    LibNoId(Src<'i>),

    /// routine `{0:#}` has no detectable name
    /// {0}
    RoutineNoName(Src<'i>),

    /// routine `{0:#}` does not start with neither .MAIN or .ROUTINE
    /// {0}
    RoutineUnrecognized(Src<'i>),

    /// label in `{0:#}` is not followed by an instruction
    /// {0}
    StatementNoInstruction(Src<'i>),

    /// mnemonic is followed by non-flag rules
    /// {0}
    StatementNoFlag(Src<'i>),

    /// operator `{0:#}` is not composed of mnemonic and flags
    /// {0}
    OperatorMiscomposition(Src<'i>),

    /// flag without flag value
    /// {0}
    FlagWithoutValue(Src<'i>),

    /// register operand contains no register type
    /// {0}
    RegisterNoType(Src<'i>),

    /// register operand does not specify register name
    /// {0}
    RegisterNoName(Src<'i>),

    /// register name must be encoded as a decimal number
    /// {0}
    /// details: {1}
    RegisterNameNonDecimal(Src<'i>, ParseIntError),

    /// register operand does not specify register index
    /// {0}
    RegisterNoIndex(Src<'i>),

    /// register index {2} is not a decimal number
    /// {0}
    /// details: {1}
    RegisterIndexNonDecimal(Src<'i>, ParseIntError, &'i str),

    /// unknown register type
    /// {0}
    RegisterUnknown(Src<'i>),

    /// call statement without library name
    /// {0}
    CallWithoutLibName(Src<'i>),

    /// call statement without routine name
    /// {0}
    CallWithoutRoutineName(Src<'i>),

    /// unknown operand format `{1}` inside the statement `{0:#}`
    /// {0}
    OperandUnknown(Src<'i>, &'i str),

    /// literal contains no data
    /// {0}
    LiteralNoData(Src<'i>),

    /// incorrect decimal literal `{1}`
    /// {0}
    /// details: {2}
    LiteralWrongDec(Src<'i>, &'i str, ParseIntError),

    /// incorrect hex literal `{1}`
    /// {0}
    /// details: {2}
    LiteralWrongHex(Src<'i>, &'i str, hex::Error),

    /// incorrect oct literal `{1}`
    /// {0}
    /// details: {2}
    LiteralWrongOct(Src<'i>, &'i str, ParseIntError),

    /// incorrect bin literal `{1}`
    /// {0}
    /// details: {2}
    LiteralWrongBin(Src<'i>, &'i str, ParseIntError),

    /// float literal contains no whole mantissa part
    /// {0}
    FloatNoWhole(Src<'i>),

    /// float literal contains no fractional mantissa part
    /// {0}
    FloatNoFraction(Src<'i>),

    /// float literal mantissa whole part is not an integer
    /// {0}
    /// details: {1}
    FloatWholeNotNumber(Src<'i>, ParseIntError),

    /// float literal mantissa fraction part is not an integer
    /// {0}
    /// details: {1}
    FloatFractionNotNumber(Src<'i>, ParseIntError),

    /// float literal exponential part is not an integer
    /// {0}
    /// details: {1}
    FloatExponentialNotNumber(Src<'i>, ParseIntError),

    /// unknown type of literal `{1:?}`
    /// {0}
    LiteralUnknown(Src<'i>, Rule),

    /// constant statement has no name
    /// {0}
    ConstNoName(Src<'i>),

    /// constant statement has no value
    /// {0}
    ConstNoValue(Src<'i>),

    /// input variable has no name
    /// {0}
    VarNoName(Src<'i>),

    /// input variable has no description
    /// {0}
    VarNoDescription(Src<'i>),

    /// input variable description is not a string literal
    /// {0}
    VarWrongDescription(Src<'i>),

    /// unable to detect program code
    ProgramAbsent,
}

impl<'i> From<LexerError<'i>> for MainError {
    #[inline]
    fn from(err: LexerError<'i>) -> Self {
        let no = err.errno();
        MainError::Lexer(err.to_string(), no)
    }
}

impl<'i> LexerError<'i> {
    pub fn errno(&self) -> u16 {
        match self {
            LexerError::UnknownSegment(_) => 1,
            LexerError::LibNoName(_) => 2,
            LexerError::LibNoId(_) => 3,
            LexerError::RoutineNoName(_) => 4,
            LexerError::RoutineUnrecognized(_) => 5,
            LexerError::StatementNoInstruction(_) => 6,
            LexerError::StatementNoFlag(_) => 7,
            LexerError::OperatorMiscomposition(_) => 8,
            LexerError::FlagWithoutValue(_) => 9,
            LexerError::RegisterNoType(_) => 10,
            LexerError::RegisterNoName(_) => 11,
            LexerError::RegisterNameNonDecimal(_, _) => 12,
            LexerError::RegisterNoIndex(_) => 13,
            LexerError::RegisterIndexNonDecimal(_, _, _) => 14,
            LexerError::RegisterUnknown(_) => 15,
            LexerError::CallWithoutLibName(_) => 16,
            LexerError::CallWithoutRoutineName(_) => 17,
            LexerError::OperandUnknown(_, _) => 18,
            LexerError::LiteralNoData(_) => 19,
            LexerError::LiteralWrongDec(_, _, _) => 20,
            LexerError::LiteralWrongHex(_, _, _) => 21,
            LexerError::LiteralWrongOct(_, _, _) => 22,
            LexerError::LiteralWrongBin(_, _, _) => 23,
            LexerError::FloatNoWhole(_) => 24,
            LexerError::FloatNoFraction(_) => 25,
            LexerError::FloatWholeNotNumber(_, _) => 26,
            LexerError::FloatFractionNotNumber(_, _) => 27,
            LexerError::FloatExponentialNotNumber(_, _) => 28,
            LexerError::LiteralUnknown(_, _) => 29,
            LexerError::ConstNoName(_) => 30,
            LexerError::ConstNoValue(_) => 31,
            LexerError::VarNoName(_) => 32,
            LexerError::VarNoDescription(_) => 33,
            LexerError::VarWrongDescription(_) => 34,
            LexerError::ProgramAbsent => 35,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Display, Error)]
#[display(doc_comments)]
pub enum InternalError {
    /// routine `{0}` is absent in the list of routines
    RoutineMissed(String),

    /// routine `{0}` contains no instructions
    RoutineEmpty(String),

    /// unable to read instruction at position {0}
    InstrRead(u16),

    /// instruction at position {0} has changed from `{1}` into `{2}`
    InstrChanged(u16, &'static str, Instr),

    /// unable to write instruction at position {0}
    /// \n
    /// details: {1}
    InstrWrite(u16, BytecodeError),

    /// unable to construct float representation of literal `{0}.{1}e{2}`
    /// \n
    /// details: {3:?}
    FloatConstruction(u128, u128, u16, ParseError),
}

impl InternalError {
    pub fn errno(&self) -> u16 {
        match self {
            InternalError::RoutineMissed(_) => 1,
            InternalError::RoutineEmpty(_) => 2,
            InternalError::InstrRead(_) => 3,
            InternalError::InstrChanged(_, _, _) => 4,
            InternalError::InstrWrite(_, _) => 5,
            InternalError::FloatConstruction(_, _, _, _) => 6,
        }
    }
}

impl From<InternalError> for MainError {
    #[inline]
    fn from(err: InternalError) -> Self {
        let no = err.errno();
        MainError::Internal(err, no)
    }
}

#[derive(Debug, Display, Error)]
#[display(doc_comments)]
pub enum BuildError {
    /// unable to create output directory `{dir}`
    /// \n
    /// details: {details}
    OutputDir { dir: String, details: Box<dyn Error> },

    /// no file named `{file}`
    /// \n
    /// details: {details}
    FileNotFound { file: String, details: Box<dyn Error> },

    /// file `{file}` exists, but can't be opened; please check file attributes
    /// \n
    /// details: {details}
    FileNoAccess { file: String, details: Box<dyn Error> },

    /// unable to create dump file `{file}`
    /// \n
    /// details: {details}
    DumpFileCreation { file: String, details: Box<dyn Error> },

    /// unable to create object file `{file}`
    /// \n
    /// details: {details}
    ObjFileCreation { file: String, details: Box<dyn Error> },

    /// unable to write into object file `{file}`
    /// \n
    /// details: {details}
    ObjFileWrite { file: String, details: Box<dyn Error> },

    /// unable to create library out of object data for `{file}`
    /// \n
    /// details: {details}
    LibraryCreation { file: String, details: SegmentError },

    /// error disassembling file `{file}` since last instruction is incomplete
    Disassembling { file: String },

    /// path `{0}` specified for objects directory (-O | --obj-dir) is not a directory. Try use
    /// lowercase -o argument if you'd like to specify a single object file
    ObjDirIsFile(String),

    /// can't access objects directory `{0}`
    /// \n
    /// details: {1}
    ObjDirFail(String, IoError),
}
