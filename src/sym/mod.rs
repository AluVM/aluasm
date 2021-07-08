// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

mod mnemonic;

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Debug;
use std::hash::Hash;

use aluvm::libs::LibId;
use aluvm::reg::{Reg32, RegAll};
use amplify::num::u1024;

use self::mnemonic::Mnemonic;

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default, Debug)]
pub struct Program {
    pub libs: BTreeMap<String, LibId>,
    pub main: Option<Routine>,
    pub code: BTreeMap<String, Routine>,
    pub r#const: BTreeMap<String, Const>,
    pub vars: BTreeMap<String, Var>,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct Const {
    pub name: String,
    pub value: Literal,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct Var {
    pub name: String,
    pub info: String,
    pub default: Option<Literal>,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct Routine {
    pub name: String,
    pub labels: BTreeMap<String, u16>,
    pub code: Vec<Operator>,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct Operator {
    pub mnemonic: Mnemonic,
    pub operands: Vec<Operand>,
    pub postfix_flags: FlagSet<char>,
    pub keyed_flags: FlagSet<KeyedFlag>,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum FlagSet<T>
where
    T: Clone + Ord + Eq + Hash + Debug,
{
    None,
    One(T),
    Double([T; 2]),
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct KeyedFlag {
    pub key: String,
    pub val: String,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum Operand {
    Reg { set: RegAll, index: Reg32 },
    Goto(Goto),
    Call(Routine),
    Lit(Literal),
    Data(String),
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum Goto {
    Label(String),
    Offset(Offset),
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum Offset {
    Backward(u16),
    Forward(u16),
    Exact(u16),
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum Call {
    Routine(String),
    Lib { routine: String, lib: String },
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum Literal {
    Int(u1024, IntBase),
    Float(u1024, u8),
    String(String),
    Char(u8),
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum IntBase {
    Dec,
    Hex,
    Oct,
    Bin,
}
