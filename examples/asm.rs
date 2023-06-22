// Reference rust implementation of AluVM (arithmetic logic unit virtual machine).
// To find more on AluVM please check <https://github.com/internet2-org/aluvm-spec>
//
// Designed & written in 2021-2022 by
//     Dr. Maxim Orlovsky <orlovsky@lnp-bp.org>
// This work is donated to LNP/BP Standards Association by Pandora Core AG
//
// This software is licensed under the terms of MIT License.
// You should have received a copy of the MIT License along with this software.
// If not, see <https://opensource.org/licenses/MIT>.

use aluasm::aluasm;
use aluvm::isa::Instr;
use aluvm::library::Lib;
use aluvm::{Prog, Vm};

fn main() {
    let code = aluasm! {
        clr     r1024[5]                        ;
        put     a16[8],5                        ;
        putif   r256[1],0xaf67937b5498dc        ;
        putif   a8[1],13                        ;
        swp     a8[1],a8[2]                     ;
        swp     f256[8],f256[7]                 ;
        dup     a256[1],a256[7]                 ;
        mov     a16[1],a16[2]                   ;
        mov     r256[8],r256[7]                 ;
        cpy     a256[1],a256[7]                 ;
        cnv     f128[4],a128[3]                 ;
        spy     a1024[15],r1024[24]             ;
        gt.u    a8[5],a64[9]                    ;
        lt.s    a8[5],a64[9]                    ;
        gt.e    f64[5],f64[9]                   ;
        lt.r    f64[5],f64[9]                   ;
        gt      r160[5],r256[9]                 ;
        lt      r160[5],r256[9]                 ;
        eq.e    a8[5],a8[9]                     ;
        eq.n    r160[5],r160[9]                 ;
        eq.e    f64[19],f64[29]                 ;
        ifn     a32[31]                         ;
        ifz     r2048[17]                       ;
        inv     st0                             ;
        st.s    a8[1]                           ;
        put     a32[12],13                      ;
        put     a32[13],66                      ;
        add.uc  a32[12],a32[13]                 ;
        add.sw  a32[12],a32[13]                 ;
        sub.sc  a32[13],a32[12]                 ;
        mul.uw  a32[12],a32[13]                 ;
        div.cu  a32[12],a32[13]                 ;
        put     f32[12],2.13                    ;
        put     f32[13],5.18                    ;
        add.z   f32[12],f32[13]                 ;
        sub.n   f32[13],f32[12]                 ;
        mul.c   f32[12],f32[13]                 ;
        div.f   f32[12],f32[13]                 ;
        rem     a8[2],a64[8]                    ;
        inc     a16[3]                          ;
        add     a16[4],5                        ;
        dec     a16[8]                          ;
        sub     a16[4],82                       ;
        neg     a64[15]                         ;
        abs     f128[11]                        ;
        and     a32[5],a32[6],a32[5]            ;
        xor     r128[5],r128[6],r128[5]         ;
        shr.u   a16[2],a256[12]                 ;
        shr.s   a16[2],a256[12]                 ;
        shl     a16[22],r256[24]                ;
        shr     a16[22],r256[24]                ;
        scr     a16[22],r256[24]                ;
        scl     a16[22],r256[24]                ;
        rev     a512[28]                        ;
        ripemd  s16[9],r160[7]                  ;
        sha2    s16[19],r256[2]                 ;
        secpgen r256[1],r512[1]                 ;
        dup     r512[1],r512[22]                ;
        spy     a512[1],r512[22]                ;
        secpmul r256[1],r512[1],r512[2]         ;
        secpadd r512[22],r512[1]                ;
        secpneg r512[1],r512[3]                 ;
        ifz     a16[8]                          ;
        jif     190                             ;
        call    56 @ alu07EnUZgFtu28sWqqH3womkTopXCkgAGsCLvLnYvNcPLRt ;
        ret                                     ;
    };

    println!("Instructions:\n{:#?}\n", code);
    let lib = Lib::assemble(&code).unwrap();
    let code = lib.disassemble::<Instr>().unwrap();
    println!("Assembly:");
    for instr in code {
        println!("\t\t{}", instr);
    }
    let lib_repr = lib.to_string();

    eprint!("\nExecuting the program {} ... ", lib.id());
    let program = Prog::<Instr>::new(lib);
    let mut runtime = Vm::<Instr>::new();
    match runtime.run(&program, &()) {
        true => eprintln!("success"),
        false => eprintln!("failure"),
    }

    println!("\nVM microprocessor core state:\n{:#?}", runtime.registers);
    println!("\n{}\n", lib_repr);
}
