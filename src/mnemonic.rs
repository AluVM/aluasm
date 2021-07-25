// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

use std::str::FromStr;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display)]
#[display(Debug)]
#[allow(non_camel_case_types)]
pub enum Mnemonic {
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

impl Mnemonic {
    pub const fn all() -> [Mnemonic; 49] {
        use Mnemonic::*;
        [
            abs, add, and, call, clr, cnv, cpy, dec, div, dup, eq, extr, fail,
            ge, gt, ifn, ifz, inc, inv, jif, jmp, le, lt, mov, mul, neg, put,
            putif, read, rem, ret, rev, ripemd, scl, scr, secpadd, secpgen,
            secpmul, secpneg, sha2, shl, shr, spy, st, sub, succ, swp, xor,
            nop,
        ]
    }
}

impl FromStr for Mnemonic {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        for m in Mnemonic::all() {
            if m.to_string().as_str() == s {
                return Ok(m);
            }
        }
        Err(())
    }
}
