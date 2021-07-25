// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

//! Abstract syntax tree data types

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Debug;
use std::hash::Hash;

use aluvm::isa::Isa;
use aluvm::libs::LibId;
use aluvm::reg::{Reg32, RegAll};
use amplify::num::u1024;

use crate::Mnemonic;

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
    pub label: Option<String>,
    pub mnemonic: Mnemonic,
    pub postfix_flags: FlagSet<char>,
    pub keyed_flags: FlagSet<KeyedFlag>,
    pub operands: Vec<Operand>,
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
