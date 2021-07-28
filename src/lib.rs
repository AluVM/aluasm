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

pub use model::{ast, issues, obj};
pub use pipelines::{analyzer, compiler, linker, parser};
