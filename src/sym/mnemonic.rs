// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

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
}
