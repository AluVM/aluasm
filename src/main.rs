// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

#[macro_use]
extern crate pest_derive;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use clap::{AppSettings, Clap};
use pest::Parser;

#[derive(Parser)]
#[grammar = "../grammar/aluasm.pest"]
pub struct AsmParser;

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

fn main() {
    let args = Args::parse();

    for file in args.files {
        let mut fd = File::open(file).expect("wrong file");

        let mut s = String::new();
        fd.read_to_string(&mut s).expect("reading file");
        let pairs =
            AsmParser::parse(Rule::program, &s).expect("compilation error: ");

        for pair in pairs {
            println!("Rule:    {:?}", pair.as_rule());
            println!("Span:    {:?}", pair.as_span());
            println!("Text:    {}", pair.as_str());
        }
    }
}
