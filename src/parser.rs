// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use super::lexer::Token;

peg::parser! {
    grammar tokenparser() for [Token] {
        pub rule instruction() -> Instr = [mnemonic @ Token::Ident] [arg1 @ Token::Arg] [Token::Comma] [arg2 @ Token::Arg] { }
    }
}
