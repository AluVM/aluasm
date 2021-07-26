// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

//! Abstract syntax tree data types

use std::collections::BTreeMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::str::FromStr;

use aluvm::reg::{Reg32, RegAll};
use amplify::num::u1024;

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
    pub code: Vec<Instruction>,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct Instruction {
    pub label: Option<String>,
    pub operator: Operator,
    pub flags: FlagSet<char>,
    pub operands: Vec<Operand>,
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum FlagSet<T>
where
    T: Clone + Ord + Eq + Hash + Debug,
{
    None,
    One(T),
    Double(T, T),
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
    Call(String),
    Lit(Literal),
    Const(String),
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
    Float(u128, u128, u16),
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

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display)]
#[display(Debug)]
#[allow(non_camel_case_types)]
pub enum Operator {
    abs,
    add,
    and,
    call,
    clr,
    cnv,
    cpy,
    dec,
    div,
    dup,
    eq,
    extr,
    fail,
    ge,
    gt,
    ifn,
    ifz,
    inc,
    inv,
    jif,
    jmp,
    le,
    lt,
    mov,
    mul,
    neg,
    put,
    putif,
    read,
    rem,
    ret,
    rev,
    ripemd,
    scl,
    scr,
    secpadd,
    secpgen,
    secpmul,
    secpneg,
    sha2,
    shl,
    shr,
    spy,
    st,
    sub,
    succ,
    swp,
    xor,
    nop,
}

impl Operator {
    pub const fn all() -> [Operator; 49] {
        use Operator::*;
        [
            abs, add, and, call, clr, cnv, cpy, dec, div, dup, eq, extr, fail, ge, gt, ifn, ifz,
            inc, inv, jif, jmp, le, lt, mov, mul, neg, put, putif, read, rem, ret, rev, ripemd,
            scl, scr, secpadd, secpgen, secpmul, secpneg, sha2, shl, shr, spy, st, sub, succ, swp,
            xor, nop,
        ]
    }
}

impl FromStr for Operator {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        for m in Operator::all() {
            if m.to_string().as_str() == s {
                return Ok(m);
            }
        }
        Err(())
    }
}
