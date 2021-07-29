// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::collections::BTreeMap;

use aluvm::data::ByteStr;
use aluvm::libs::LibSeg;

use crate::module::Variable;

// TODO: Move shared part into AluVM structure (`Lib`)

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum EntryPoint {
    LibTable(BTreeMap<String, u16>),
    BinMain(u16),
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct Product {
    pub isae: String,
    pub code: ByteStr,
    pub data: ByteStr,
    pub libs: LibSeg,
    pub vars: Vec<Variable>,
    pub entry_point: EntryPoint,
}
