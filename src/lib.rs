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

#[macro_use]
mod macros;
mod model;
mod pipelines;

use std::error::Error;
use std::num::ParseIntError;

use aluvm::data::encoding::DecodeError;
use aluvm::isa::Instr;
use aluvm::library::{CodeEofError, IsaSegError};
use amplify::{hex, IoError};
pub use model::{ast, issues, module, product};
#[doc(hidden)]
pub use paste::paste;
pub use pipelines::{analyzer, compiler, linker, parser};

use crate::issues::Src;
use crate::module::CallTableError;
use crate::parser::Rule;
use crate::product::DyError;

#[derive(Debug, Display, Error, From)]
pub enum MainError {
    #[display("\x1B[1;31mInternal lexer error X{1:04}:\x1B[0m {0}")]
    Lexer(String, u16),

    #[display("\x1B[1;31mInternal compiler error C{1:04}:\x1B[0m {0}")]
    Compiler(CompilerError, u16),

    #[display("\x1B[1;31mInternal linker error L{1:04}:\x1B[0m {0}")]
    Linker(LinkerError, u16),

    #[display("\x1B[1;31mError:\x1B[0m {0}")]
    #[from]
    Build(BuildError),

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
        "\x1B[1;31mError:\x1B[0m {0}\n\n\x1B[1;31mError:\x1B[0m failing due to broken binary data \
         in module `{1}` "
    )]
    Module(DecodeError, String),

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

    /// input variable has no type
    /// {0}
    VarNoType(Src<'i>),

    /// input variable has no description
    /// {0}
    VarNoDescription(Src<'i>),

    /// input variable description is not a string literal
    /// {0}
    VarWrongDescription(Src<'i>),

    /// unknown variable type `{0}`
    /// {1}
    VarTypeUnknown(String, Src<'i>),

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
            LexerError::VarNoType(_) => 36,
            LexerError::VarTypeUnknown(_, _) => 37,
        }
    }
}

/// Errors happening when editing existing instruction (should never happen and indicate internal
/// compiler bug)
#[derive(Clone, Debug, Display, Error, From)]
#[display(doc_comments)]
pub enum InstrError {
    /// referenced position is outside of the code
    #[from(CodeEofError)]
    Read,

    /// instruction has changed from `{0}` to `{1}`
    Changed(&'static str, Instr),
}

#[derive(Clone, Eq, PartialEq, Debug, Display, Error, From)]
#[display(doc_comments)]
pub enum CompilerError {
    /// Errors related to ISA segment
    #[from]
    #[display(inner)]
    Isa(IsaSegError),

    /// routine `{0}` is absent in the list of routines
    RoutineMissed(String),

    /// routine `{0}` contains no instructions
    RoutineEmpty(String),

    /// unable to read instruction at position {0}
    InstrRead(u16),

    /// instruction at position {0} has changed from `{1}` into `{2}`
    InstrChanged(u16, &'static str, Instr),

    /// Call table error
    #[from]
    #[display(inner)]
    CallTable(CallTableError),

    /// unable to construct float representation of literal `{0}.{1}e{2}`
    ///
    /// details: {3:?}
    FloatConstruction(u128, u128, u16, amplify::num::apfloat::ParseError),
}

impl CompilerError {
    pub fn errno(&self) -> u16 {
        match self {
            CompilerError::Isa(err) => match err {
                IsaSegError::SegmentTooLarge(_) => 1,
                IsaSegError::SegmentTooManyExt(_) => 2,
                IsaSegError::IsaIdWrongLength(_) => 3,
                IsaSegError::IsaIdWrongSymbols(_) => 4,
            },
            CompilerError::RoutineMissed(_) => 5,
            CompilerError::RoutineEmpty(_) => 6,
            CompilerError::InstrRead(_) => 7,
            CompilerError::InstrChanged(_, _, _) => 8,
            CompilerError::FloatConstruction(_, _, _, _) => 9,
            CompilerError::CallTable(_) => 10,
        }
    }

    pub fn with(err: InstrError, pos: u16) -> Self {
        match err {
            InstrError::Read => CompilerError::InstrRead(pos),
            InstrError::Changed(from, to) => CompilerError::InstrChanged(pos, from, to),
        }
    }
}

impl From<CompilerError> for MainError {
    #[inline]
    fn from(err: CompilerError) -> Self {
        let no = err.errno();
        MainError::Compiler(err, no)
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, Display, Error)]
#[display(doc_comments)]
pub enum LinkerError {
    /// unable to read instruction at position {0}
    InstrRead(u16),

    /// instruction at position {0} has changed from `{1}` into `{2}`
    InstrChanged(u16, &'static str, Instr),
}

impl LinkerError {
    pub fn errno(&self) -> u16 {
        match self {
            LinkerError::InstrRead(_) => 3,
            LinkerError::InstrChanged(_, _, _) => 4,
        }
    }

    pub fn with(err: InstrError, pos: u16) -> Self {
        match err {
            InstrError::Read => LinkerError::InstrRead(pos),
            InstrError::Changed(from, to) => LinkerError::InstrChanged(pos, from, to),
        }
    }
}

impl From<LinkerError> for MainError {
    #[inline]
    fn from(err: LinkerError) -> Self {
        let no = err.errno();
        MainError::Linker(err, no)
    }
}

#[derive(Debug, Display, Error)]
#[display(doc_comments)]
pub enum BuildError {
    /// unable to create output directory `{dir}`
    ///
    /// details: {details}
    OutputDir { dir: String, details: Box<dyn Error> },

    /// `{0}` provided as one o the arguments is not a file
    NotFile(String),

    /// no file named `{file}`
    ///
    /// details: {details}
    FileNotFound { file: String, details: Box<dyn Error> },

    /// file `{file}` exists, but can't be opened; please check file attributes
    ///
    /// details: {details}
    FileNoAccess { file: String, details: Box<dyn Error> },

    /// unable to create dump file `{file}`
    ///
    /// details: {details}
    DumpFileCreation { file: String, details: Box<dyn Error> },

    /// unable to create object file `{file}`
    ///
    /// details: {details}
    ObjFileCreation { file: String, details: Box<dyn Error> },

    /// unable to write into object file `{file}`
    ///
    /// details: {details}
    ObjFileWrite { file: String, details: Box<dyn Error> },

    /// unable to create product file `{file}`
    ///
    /// details: {details}
    ProductFileCreation { file: String, details: Box<dyn Error> },

    /// unable to write into product file `{file}`
    ///
    /// details: {details}
    ProductFileWrite { file: String, details: Box<dyn Error> },

    /// error disassembling file `{file}` since last instruction is incomplete
    Disassembling { file: String },

    /// path `{0}` specified for objects directory (-O | --obj-dir) is not a directory. Try use
    /// lowercase -o argument if you'd like to specify a single object file
    ObjDirIsFile(String),

    /// can't access objects directory `{0}`
    ///
    /// details: {1}
    ObjDirFail(String, IoError),

    /// path `{0}` specified for library directory (-L | --lib-dir) is not a directory. Try use
    /// lowercase -l argument if you'd like to specify a single library file
    LibDirIsFile(String),

    /// library `{0}` is a directory, not a file. Try use uppercase -L argument if you'd like to
    /// specify a library directory and not a single file
    LibIsDir(String),

    /// can't access library directory `{0}`
    ///
    /// details: {1}
    LibDirFail(String, IoError),

    /// library at `{0}` has incorrect binary data
    ///
    /// details: {1}
    LibIncorrectData(String, DyError),

    /// library file at `{0}` is not accessible
    ///
    /// details: {1}
    LibNotAccessible(String, Box<dyn Error>),
}
