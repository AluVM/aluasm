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

mod analyzer;
pub mod ast;
mod compiler;
mod issues;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use aluvm::isa::ReservedOp;
use aluvm::libs::Lib;
use clap::{AppSettings, Clap};
use pest::Parser;

use crate::ast::Program;
pub use crate::issues::{Error, Issue, Issues, Warning};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct AsmParser;

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Clap)]
#[clap(
    name = "aluasm",
    bin_name = "aluasm",
    author,
    version,
    about,
    setting = AppSettings::ColoredHelp
)]
pub struct Args {
    /// Source files to compile
    pub files: Vec<PathBuf>,
}

fn main() {
    let args = Args::parse();

    for file in args.files {
        let mut fd = File::open(&file).expect("wrong file");

        let mut s = String::new();
        fd.read_to_string(&mut s).expect("reading file");
        let pairs = AsmParser::parse(Rule::program, &s).expect("compilation error: ");
        let program = Program::analyze(pairs.into_iter().next().expect("program not found"));

        eprintln!("{}", program.issues);
        if !program.issues.has_errors() {
            let (module, issues) = program.compile();
            eprintln!("{}", issues);
            if !issues.has_errors() {
                let mut dest = file.clone();
                dest.set_extension("ao");
                let mut fd = File::create(dest).expect("creating module file");
                module.write(&mut fd).expect("writing module output");

                println!("{:?}", module);
                let lib: Lib<ReservedOp> =
                    Lib::with(&module.isae, module.code, module.data, module.libs)
                        .expect("module can't be transformed into a library");
                println!("{}", lib);
                let code = lib.disassemble().expect("library disassembly failed");
                for instr in code {
                    println!("\t\t{}", instr);
                }
            }
        }
    }
}
