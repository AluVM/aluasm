// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error]
    Error,

    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Whitespace,

    #[regex("[A-Za-z0-9]{2;}")]
    Ident,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("\\[\\d+\\]")]
    Index,

    #[regex(";.*\n")]
    LineComment,

    #[regex("\\(*(.|\n)+*\\)")]
    BlockComment,

    #[token(".isae")]
    IsaeSegment,

    #[token(".libs")]
    LibsSegment,

    #[token(".code")]
    CodeSegment,

    #[token(".data")]
    DataSegment,

    #[token(".input")]
    InputSegment,

    #[token(".output")]
    OutputSegment,
}
