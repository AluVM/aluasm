// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

mod lexer;
mod parser;

use std::path::PathBuf;

use clap::{AppSettings, Clap};

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Clap)]
#[clap(
    name = "aluasm",
    bin_name = "aluasm",
    author,
    version,
    setting = AppSettings::ColoredHelp
)]
pub struct Args {
    /// Source files to compile
    pub files: Vec<PathBuf>,
}

fn main() { let args = Args::parse(); }
