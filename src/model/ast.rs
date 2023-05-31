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
use std::str::FromStr;

use aluvm::data::{FloatLayout, IntLayout};
use aluvm::library::LibId;
use aluvm::reg::{Reg32, RegAll};
use aluvm::Isa;
use amplify::num::u1024;
use pest::Span;

#[derive(Clone, Hash, Debug)]
pub struct Program<'i> {
    pub isae: BTreeSet<Isa>,
    pub libs: Libs<'i>,
    pub main: Option<Routine<'i>>,
    pub routines: BTreeMap<String, Routine<'i>>,
    pub consts: BTreeMap<String, Const<'i>>,
    pub input: BTreeMap<String, Var<'i>>,
}

#[derive(Clone, Hash, Debug)]
pub struct Libs<'i> {
    pub map: BTreeMap<String, LibId>,
    pub span: Span<'i>,
}

#[derive(Clone, Hash, Debug)]
pub struct Const<'i> {
    pub name: String,
    pub value: Literal,
    pub span: Span<'i>,
}

#[derive(Copy, Clone, Hash, Debug)]
pub enum VarType {
    Bytes,
    Str,
    Int(IntLayout),
    Float(FloatLayout),
}

#[derive(Clone, Hash, Debug)]
pub struct Var<'i> {
    pub name: String,
    pub ty: VarType,
    pub default: Option<Literal>,
    pub info: String,
    pub span: Span<'i>,
}

#[derive(Clone, Hash, Debug)]
pub struct Routine<'i> {
    pub name: String,
    pub labels: BTreeMap<String, u16>,
    pub statements: Vec<Statement<'i>>,
    pub span: Span<'i>,
}

#[derive(Clone, Hash, Debug)]
pub struct Statement<'i> {
    pub label: Option<(String, Span<'i>)>,
    pub operator: (Operator, Span<'i>),
    pub flags: FlagSet<'i, char>,
    pub operands: Vec<Operand<'i>>,
    pub span: Span<'i>,
}

#[derive(Clone, Hash, Debug)]
pub enum FlagSet<'i, T>
where
    T: Clone + Ord + Eq + Hash + Debug,
{
    None,
    One(T, Span<'i>),
    Double(T, T, Span<'i>),
}

#[derive(Clone, Hash, Debug)]
pub struct KeyedFlag<'i> {
    pub key: String,
    pub val: String,
    pub span: Span<'i>,
}

#[derive(Clone, Hash, Debug)]
pub enum Operand<'i> {
    Reg { set: RegAll, index: Reg32, span: Span<'i> },
    Goto(String, Span<'i>),
    Call { lib: String, routine: String, span: Span<'i> },
    Lit(Literal, Span<'i>),
    Const(String, Span<'i>),
}

impl<'i> Operand<'i> {
    pub fn as_span(&self) -> &Span {
        match self {
            Operand::Reg { span, .. }
            | Operand::Goto(_, span)
            | Operand::Call { span, .. }
            | Operand::Lit(_, span)
            | Operand::Const(_, span) => span,
        }
    }

    pub fn description(&self) -> &'static str {
        match self {
            Operand::Reg { .. } => "register",
            Operand::Goto(_, _) => "goto statement",
            Operand::Call { .. } => "call statement",
            Operand::Lit(lit, _) => lit.description(),
            Operand::Const(_, _) => "constant value",
        }
    }
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
    Bytes(Vec<u8>),
    Char(u8),
}

impl Literal {
    pub fn description(&self) -> &'static str {
        match self {
            Literal::Int(_, _) => "integer literal",
            Literal::Float(_, _, _) => "float literal",
            Literal::String(_) => "string literal",
            Literal::Char(_) => "char literal",
            Literal::Bytes(_) => "bytes literal",
        }
    }
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
    cnt,
    cnv,
    con,
    cpy,
    dec,
    del,
    div,
    dup,
    eq,
    exec,
    extr,
    fail,
    fill,
    find,
    gt,
    ifn,
    ifz,
    inc,
    inj,
    ins,
    jif,
    jmp,
    join,
    len,
    lt,
    mov,
    mul,
    neg,
    not,
    or,
    put,
    putif,
    read,
    rem,
    ret,
    rev,
    ripemd,
    routine,
    scl,
    scr,
    secpadd,
    secpgen,
    secpmul,
    secpneg,
    sha2,
    shl,
    shr,
    splt,
    spy,
    st,
    stinv,
    sub,
    succ,
    swp,
    xor,
    nop,
}

impl Operator {
    pub const fn all() -> [Operator; 61] {
        use Operator::*;
        [
            abs, add, and, call, clr, cnt, cnv, con, cpy, dec, del, div, dup, eq, exec, extr, fail,
            fill, find, gt, ifn, ifz, inc, inj, ins, jif, jmp, join, len, lt, mov, mul, neg, not,
            or, put, putif, read, rem, ret, rev, ripemd, routine, scl, scr, secpadd, secpgen,
            secpmul, secpneg, sha2, shl, shr, splt, spy, st, stinv, sub, succ, swp, xor, nop,
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
