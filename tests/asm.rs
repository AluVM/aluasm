macro_rules! aluasm_succ {
    ($( $tt:tt )+) => { {
        aluasm_macro_succ! { $( $tt )+ };
        let (res, runtime) = aluasm_compiler! { $( $tt )+ };
        assert!(res, "aluasm_compiler: expected success:\n{:#?}", runtime.registers);
    } }
}

macro_rules! aluasm_fail {
    ($( $tt:tt )+) => { {
        aluasm_macro_fail! { $( $tt )+ };
        let (res, runtime) = aluasm_compiler! { $( $tt )+ };
        assert!(!res, "aluasm_compiler: expected failure:\n{:#?}", runtime.registers);
    } }
}

macro_rules! aluasm_macro_succ {
    ($( $tt:tt )+) => { {
        let (res, runtime) = aluasm_macro! { $( $tt )+ };
        assert!(res, "aluvm_macro: expected success\n{:#?}", runtime.registers);
    } }
}

macro_rules! aluasm_macro_fail {
    ($( $tt:tt )+) => { {
        let (res, runtime) = aluasm_macro! { $( $tt )+ };
        assert!(!res, "aluvm_macro: expected failure\n{:#?}", runtime.registers);
    } }
}

macro_rules! aluasm_compiler {
    ($( $tt:tt )+) => { {
        use pest::Parser;
        let main = stringify!($( $tt )+);
        let main = main.replace(" [", "[");
        let main = main.replace("\n[", "[");
        let main = main.replace(",\n", ",");
        let main = main.replace("\n", " ");
        let main = main.replace(";", ";\n");
        let code = format!(
            r#".ISAE ; ISA Extensions segment
                    ALU
               .MAIN ; Code segment
                    {}
        "#, main);
        let pairs = aluasm::parser::Parser::parse(aluasm::parser::Rule::program, &code).unwrap();
        let (program, issues) = aluasm::ast::Program::analyze(pairs.into_iter().next().unwrap()).unwrap();
        assert!(!issues.has_errors(), "error(analyze): {}", issues);
        let (module, issues) = program.compile(&mut None).unwrap();
        assert!(!issues.has_errors(), "error(compile): {}", issues);
        let mut runtime = aluvm::Vm::<aluvm::isa::Instr>::new();
        let program = aluvm::Prog::<aluvm::isa::Instr>::new(module.as_static_lib().clone());
        let res = runtime.run(&program, &());
        (res, runtime)
    } }
}

macro_rules! aluasm_macro {
    ($( $tt:tt )+) => { {
        let mut runtime = aluvm::Vm::<aluvm::isa::Instr>::new();
        let code = aluasm::aluasm! { $( $tt )+ };
        let program = aluvm::Prog::<aluvm::isa::Instr>::new(aluvm::library::Lib::assemble(&code).unwrap());
        let res = runtime.run(&program, &());
        (res, runtime)
    } }
}

#[test]
fn a8_ne() {
    aluasm_fail! {
        put     a8[1],12;
        put     a8[2],9;
        eq.n    a8[1],a8[2];
        ret;
    }
}

#[test]
fn a8_eq() {
    aluasm_succ! {
        put     a8[1],9;
        put     a8[2],9;
        eq.n    a8[1],a8[2];
        ret;
    }
    aluasm_fail! {
        eq.n    a8[1],a8[2];
        ret;
    }
    aluasm_succ! {
        eq.e    a8[1],a8[2];
        ret;
    }
}

#[test]
fn a16_eq() {
    aluasm_succ! {
        put     a16[1],4;
        put     a16[2],4;
        eq.n    a16[1],a16[2];
        ret;
    }
}

#[test]
fn a_eq_fail() {
    aluasm_fail! {
        put     a16[1],3;
        put     a16[2],4;
        eq.n    a16[1],a16[2];
        ret;
    }
}

#[test]
fn a_eq_noneeq_eq() {
    aluasm_succ! {
        eq.e    a16[1],a16[2];
        ret;
    }
}

#[test]
fn a_eq_noneeq_noneq() {
    aluasm_fail! {
        eq.n    a16[1],a16[2];
        ret;
    }
}

#[test]
fn a_gt_u() {
    aluasm_succ! {
        put     a8[1],2;
        put     a8[2],1;
        gt.u    a8[1],a8[2];
        ret;
    }
}

#[test]
fn a_gt_s() {
    aluasm_succ! {
        put     a8[1],1;
        put     a8[2],255; // -1
        gt.s    a8[1],a8[2];
        ret;
    }
    // negative literal works only on macro
    aluasm_macro_succ! {
        put     a8[1],-1;
        put     a8[2],1;
        lt.s    a8[1],a8[2];
        ret;
    };
    aluasm_fail! {
        put     a8[1],1;
        put     a8[2],2;
        gt.s    a8[1],a8[2];
        ret;
    }
}

#[test]
fn a_lt_u() {
    aluasm_succ! {
        put     a8[1],1;
        put     a8[2],2;
        lt.u    a8[1],a8[2];
        ret;
    }
}

#[test]
fn a_lt_s() {
    aluasm_succ! {
        put     a8[1],255; // -1
        put     a8[2],1;
        lt.s    a8[1],a8[2];
        ret;
    }
    aluasm_fail! {
        put     a8[1],2;
        put     a8[2],1;
        lt.s    a8[1],a8[2];
        ret;
    }
}

#[test]
fn stp_add() {
    aluasm_succ! {
        put     a8[1],3;
        add     a8[1],4;
        put     a8[2],7;
        eq.n    a8[1],a8[2];
        ret;
    }
    aluasm_succ! {
        put     a16[1],65534;
        add     a16[1],1;
        put     a16[2],65535;
        eq.n    a16[1],a16[2];
        ret;
    }
}

#[test]
fn stp_add_overflow() {
    aluasm_fail! {
        put     a16[1],65534;
        add     a8[1],2;
        ret;
    }
}

#[test]
fn stp_sub() {
    aluasm_succ! {
        put     a32[1],7;
        sub     a32[1],1;
        put     a32[2],6;
        eq.n    a32[1],a32[2];
        ret;
    }
    aluasm_succ! {
        put a32[8],3;
        sub a32[8],3;
        put a32[9],0;
        eq.n a32[8],a32[9];
        ret;
    }
}

#[test]
fn stp_sub_underflow() {
    aluasm_fail! {
        put     a8[1],3;
        sub     a8[1],4;
        ret;
    }
}

#[test]
fn inc() {
    aluasm_succ! {
        put     a32[1],7;
        inc     a32[1];
        put     a32[2],8;
        eq.n    a32[1],a32[2];
        ret;
    }
    aluasm_succ! {
        put     a16[1],65534;
        inc     a16[1];
        put     a16[2],65535;
        eq.n    a16[1],a16[2];
        ret;
    }
}

#[test]
fn inc_overflow() {
    aluasm_fail! {
        put     a16[1],65535;
        inc     a16[1];
        ret;
    }
}

#[test]
fn dec() {
    aluasm_succ! {
        put     a1024[1],8;
        dec     a1024[1];
        put     a1024[2],7;
        eq.n    a1024[1],a1024[2];
        ret;
    }
    aluasm_succ! {
        put     a256[1],1;
        dec     a256[1];
        put     a256[2],0;
        eq.n    a256[1],a256[2];
        ret;
    }
}

#[test]
fn dec_underflow() {
    aluasm_fail! {
        put     a16[1],0;
        dec     a16[1];
        ret;
    }
}

#[test]
fn float() {
    aluasm_succ! {
            put   f32[8],1.25;
            put   f32[9],1.5;
            put   f32[10],2.75;
            add.f f32[8],f32[9];
            eq.e  f32[9],f32[10];
            ret;
    }
}

#[test]
fn bytes_put() {
    aluasm_succ! {
            put   s16[1],"aaa";
            put   s16[2],"aaa";
            eq    s16[1],s16[2];
            ret;
    }
    aluasm_fail! {
        put   s16[1],"aaa";
        put   s16[2],"bbb";
        eq    s16[1],s16[2];
        ret;
    }
    aluasm_succ! {
        put   s16[1],"";
        put   s16[2],"";
        eq    s16[1],s16[2];
        ret;
    }
}

#[test]
fn bytes_extr() {
    aluasm_succ! {
            put    s16[0],"################@@@@@@";
            put    a16[0],0;
            extr   s16[0],r128[0],a16[0];
            put    r128[1],0x23232323232323232323232323232323;
            eq.n   r128[0],r128[1];
            ret;
    };
    aluasm_succ! {
            put    s16[0],"################@@@@@@";
            put    a16[0],3;
            extr   s16[0],r128[0],a16[0];
            put    r128[1],0x40404023232323232323232323232323;
            eq.n   r128[0],r128[1];
            ret;
    }
}

#[test]
fn bytes_extr_offset_exceed() {
    aluasm_succ! {
            put    s16[0],"123456788901234567";
            put    a16[0],0;
            extr   s16[0],r128[0],a16[0];
            ret;
    }
    aluasm_succ! {
            put    s16[0],"123456788901234567";
            put    a16[0],1;
            extr   s16[0],r128[0],a16[0];
            ret;
    }
    aluasm_fail! {
            put    s16[0],"123456788901234567";
            put    a16[0],2;
            extr   s16[0],r128[0],a16[0];
            ret;
    }
    aluasm_fail! {
            put    s16[0],"123456788901234567";
            put    a16[0],2;
            extr   s16[0],r128[0],a16[0];
            ret;
    }
    aluasm_succ! {
            put    s16[0],"################@";
            put    a16[0],1;
            extr   s16[0],r128[0],a16[0];
            put    r128[1],0x40232323232323232323232323232323;
            eq.n   r128[0],r128[1];
            ret;
    }
    aluasm_fail! {
            put    s16[0],"123456788901234567";
            put    a16[0],100;
            extr   s16[0],r128[0],a16[0];
            ret;
    }
    aluasm_fail! {
            put    s16[0],"123";
            put    a16[0],0;
            extr   s16[0],r128[0],a16[0];
            ret;
    }
}

#[test]
fn bytes_fill() {
    aluasm_succ! {
        put    s16[0],"aaaaaaaa";
        put    s16[1],"aaabbbaa";
        put    a16[0],3;
        put    a16[1],6;
        put    a8[0],98;
        fill.e s16[0],a16[0],a16[1],a8[0];
        eq     s16[0],s16[1];
        ret;
    }
}

#[test]
fn bytes_fill_extend() {
    aluasm_succ! {
        put    s16[0],"aaaaaaaa";
        put    s16[1],"aaaaabbbbb";
        put    a16[0],5;
        put    a16[1],10;
        put    a8[0],98;
        fill.e s16[0],a16[0],a16[1],a8[0];
        eq     s16[0],s16[1];
        ret;
    }
    aluasm_succ! {
        put    s16[0],"aaaaaaaa";
        put    s16[1],"abaaaaaa";
        put    a16[0],1;
        put    a16[1],2;
        put    a8[0],98;
        fill.e s16[0],a16[0],a16[1],a8[0];
        eq     s16[0],s16[1];
        ret;
    }
}

#[test]
fn bytes_len() {
    aluasm_succ! {
        put    s16[0],"aaaaaaaa";
        put    a16[0],8;
        len    s16[0],a16[1];
        eq.n   a16[0],a16[1];
        ret;
    }
}

#[test]
fn bytes_len_overflow() {
    aluasm_succ! {
        put    a16[0],0;
        put    a16[1],255;
        put    a8[0],97;
        fill.e s16[0],a16[0],a16[1],a8[0];
        len    s16[0],a8[2];
        ret;
    }
    aluasm_fail! {
        put    a16[0],0;
        put    a16[1],256;
        put    a8[0],97;
        fill.e s16[0],a16[0],a16[1],a8[0];
        len    s16[0],a8[2];
        ret;
    }
    aluasm_succ! {
        put    a16[0],0;
        put    a16[1],255;
        put    a8[0],97;
        put    a8[2],1;
        fill.e s16[0],a16[0],a16[1],a8[0];
        len    s16[0],a8[2];
        eq.e   a8[2],a8[3];
        ret;
    }
}

#[test]
fn bytes_cnt() {
    aluasm_succ! {
        put    s16[0],"hello world";
        put    a8[0],108;
        put    a16[0],3;
        cnt    s16[0],a8[0],a16[1];
        eq.n   a16[0],a16[1];
        ret;
    }
}

#[test]
fn bytes_cnt_uninitialized_byte() {
    aluasm_fail! {
        put    s16[0],"hello world";
        cnt    s16[0],a8[0],a16[1];
        ret;
    }
    aluasm_succ! {
        put    s16[0],"hello world";
        put    a16[1],1;
        cnt    s16[0],a8[0],a16[1];
        eq.e   a16[0],a16[1];
        ret;
    }
}

#[test]
fn bytes_cnt_empty_string() {
    aluasm_fail! {
        put    s16[0],"";
        cnt    s16[0],a8[0],a16[1];
        ret;
    }
}

#[test]
fn bytes_con() {
    aluasm_succ! {
        put    s16[0],"hello@world!!";
        put    s16[1],"hello#world!";
        put    a16[0],0;
        put    a16[1],5;
        con    s16[0],s16[1],a16[0],a16[2],a16[3];
        eq.n   a16[0],a16[2];
        eq.n   a16[1],a16[3];
        ret;
    }
    aluasm_succ! {
        put    s16[0],"hello@world!!";
        put    s16[1],"hello#world!";
        put    a16[0],1;
        put    a16[1],6;
        con    s16[0],s16[1],a16[0],a16[2],a16[3];
        eq.n   a16[0],a16[2];
        eq.n   a16[1],a16[3];
        ret;
    }
    aluasm_succ! {
        put    s16[0],"hello world";
        put    s16[1],"hello world";
        put    a16[0],0;
        put    a16[1],11;
        con    s16[0],s16[1],a16[0],a16[2],a16[3];
        eq.n   a16[0],a16[2];
        eq.n   a16[1],a16[3];
        ret;
    }
    aluasm_succ! {
        put    s16[0],"hello world";
        put    s16[1],"hello world";
        put    a16[0],1;
        put    a16[2],1000;
        put    a16[3],1000;
        con    s16[0],s16[1],a16[0],a16[2],a16[3];
        eq.e   a16[0],a16[2];
        eq.e   a16[1],a16[3];
        ret;
    }
    aluasm_succ! {
        put    a16[0],1;
        put    a16[2],1000;
        put    a16[3],1000;
        con    s16[0],s16[1],a16[0],a16[2],a16[3];
        eq.e   a16[0],a16[2];
        eq.e   a16[1],a16[3];
        ret;
    }
}

#[test]
fn bytes_find() {
    aluasm_succ! {
        put    s16[0],"hello world";
        put    s16[1],"l";
        put    a16[1],3;
        find   s16[0],s16[1],a16[0];
        eq.n   a16[0],a16[1];
        ret;
    }
    aluasm_succ! {
        put    s16[0],"hello world";
        put    s16[1],"ll";
        put    a16[1],1;
        find   s16[0],s16[1],a16[0];
        eq.n   a16[0],a16[1];
        ret;
    }
    aluasm_succ! {
        put    s16[0],"hello world";
        put    s16[1],"lll";
        put    a16[1],0;
        find   s16[0],s16[1],a16[0];
        eq.n   a16[0],a16[1];
        ret;
    }
    aluasm_succ! {
        put    s16[0],"hello world";
        put    s16[1],"hello world!!!";
        put    a16[1],0;
        find   s16[0],s16[1],a16[0];
        eq.n   a16[0],a16[1];
        ret;
    }
}

#[test]
fn bytes_find_max() {
    aluasm_succ! {
        put    a16[1],0;
        put    a16[2],65535;
        put    a8[0],97;
        fill.e s16[0],a16[1],a16[2],a8[0];
        put    s16[1],"a";
        find   s16[0],s16[1],a16[0];
        eq.n   a16[0],a16[2];
        ret;
    }
}

#[test]
fn bytes_rev() {
    aluasm_succ! {
        put    s16[0],"abcd";
        put    s16[1],"dcba";
        rev    s16[0],s16[2];
        eq     s16[1],s16[2];
        ret;
    }
    aluasm_fail! {
        rev    s16[0],s16[1];
        ret;
    }
}

#[test]
fn shl() {
    aluasm_succ! {
        put    a8[0],1;
        put    a8[1],3;
        put    a8[2],8;
        shl    a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
}

#[test]
fn shl_st0() {
    aluasm_fail! {
        put    a8[0],1;
        put    a8[1],3;
        shl    a8[1],a8[0];
        ret;
    }
    aluasm_succ! {
        put    a8[0],128;
        put    a8[1],3;
        shl    a8[1],a8[0];
        ret;
    }
}

#[test]
fn shl_overflow() {
    aluasm_succ! {
        put    a8[0],0b11111111;
        put    a8[1],7;
        put    a8[2],0b10000000;
        shl    a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
    aluasm_succ! {
        put    a8[0],0b11111111;
        put    a8[1],8;
        put    a8[2],0b00000000;
        shl    a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
    aluasm_succ! {
        put    a8[0],0b11111111;
        put    a8[1],9;
        put    a8[2],0b00000000;
        shl    a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
}

#[test]
fn shl_rreg() {
    aluasm_succ! {
        put    r128[0],1;
        put    a8[1],3;
        put    r128[2],8;
        shl    a8[1],r128[0];
        eq.n   r128[0],r128[2];
        ret;
    }
    aluasm_succ! {
        put    r8192[0],1;
        put    a16[1],2;
        put    r8192[2],4;
        shl    a16[1],r8192[0];
        eq.n   r8192[0],r8192[2];
        ret;
    }
    aluasm_succ! {
        put    r8192[0],1;
        put    a16[1],2000;
        put    r8192[2],1;
        put    a16[2],1000;
        shl    a16[1],r8192[0];
        shl    a16[2],r8192[2];
        shl    a16[2],r8192[2];
        eq.n   r8192[0],r8192[2];
        ret;
    }
}

#[test]
fn shl_rreg_st0() {
    aluasm_fail! {
        put    r128[0],1;
        put    a8[1],3;
        shl    a8[1],r128[0];
        ret;
    }
    aluasm_succ! {
        put    r128[0],1;
        put    a16[1],127;
        shl    a16[1],r128[0];
        put    a16[1],1;
        shl    a16[1],r128[0];
        ret;
    }
}

#[test]
fn shr_u() {
    aluasm_succ! {
        put    a8[0],8;
        put    a8[1],3;
        put    a8[2],1;
        shr.u  a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
    aluasm_succ! {
        put    a8[0],8;
        put    a8[1],4;
        put    a8[2],0;
        shr.u  a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
    aluasm_succ! {
        put    a8[0],255;
        put    a8[1],1;
        put    a8[2],127;
        shr.u  a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
    // sets st0 to false if lsb is 0 before the operation
    aluasm_fail! {
        put    a8[0],2;
        put    a8[1],1;
        shr.u  a8[1],a8[0];
        ret;
    }
    aluasm_succ! {
        put    a8[0],2;
        put    a8[1],1;
        put    a8[2],1;
        shr.u  a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
    aluasm_succ! {
        put    a8[0],3;
        put    a8[1],1;
        shr.u  a8[1],a8[0];
        ret;
    }
}

#[test]
fn shr_u_st0() {
    aluasm_fail! {
        put    a8[0],2;
        put    a8[1],1;
        shr.u  a8[1],a8[0];
        ret;
    }
    aluasm_succ! {
        put    a8[0],3;
        put    a8[1],1;
        shr.u  a8[1],a8[0];
        ret;
    }
}

#[test]
fn shr_s() {
    aluasm_succ! {
        put    a8[0],8;
        put    a8[1],3;
        put    a8[2],1;
        shr.s  a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
    aluasm_succ! {
        put    a8[0],255;
        put    a8[1],1;
        put    a8[2],255;
        shr.s  a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
}

#[test]
fn shr_r() {
    aluasm_succ! {
        put    r128[0],8;
        put    a8[1],3;
        put    r128[2],1;
        shr    a8[1],r128[0];
        eq.n   r128[0],r128[2];
        ret;
    }
    aluasm_succ! {
        put    r8192[0],4;
        put    a16[1],2;
        put    r8192[2],1;
        shr    a16[1],r8192[0];
        eq.n   r8192[0],r8192[2];
        ret;
    }
    aluasm_succ! {
        put    r8192[0],0;
        put    a16[1],2;
        put    r8192[2],0;
        shr    a16[1],r8192[0];
        eq.n   r8192[0],r8192[2];
        ret;
    }
    aluasm_succ! {
        put    r8192[0],1;
        put    a16[1],2000;
        put    r8192[2],1;
        shl    a16[1],r8192[0];
        shr    a16[1],r8192[0];
        eq.n   r8192[0],r8192[2];
        ret;
    }
}

#[test]
fn shr_r_st0() {
    aluasm_fail! {
        put    r128[0],2;
        put    a8[1],3;
        shr    a8[1],r128[0];
        ret;
    }
    aluasm_succ! {
        put    r128[0],3;
        put    a8[1],3;
        shr    a8[1],r128[0];
        ret;
    }
}

#[test]
fn scl_a() {
    aluasm_succ! {
        put    a16[0],5;
        put    a16[1],1;
        put    a16[2],10;
        scl    a16[1],a16[0];
        eq.n   a16[0],a16[2];
        ret;
    }
    aluasm_succ! {
        put    a8[0],5;
        put    a8[1],7;
        put    a8[2],130;
        scl    a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
    aluasm_succ! {
        put    a8[0],2;
        put    a8[1],7;
        put    a8[2],1;
        scl    a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
    aluasm_succ! {
        put    a8[0],2;
        put    a8[1],81;
        put    a8[2],4;
        scl    a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
}

#[test]
fn scl_a_st0() {
    aluasm_fail! {
        put    a8[0],1;
        put    a8[1],1;
        scl    a8[1],a8[0];
        ret;
    }
    aluasm_succ! {
        put    a8[0],128;
        put    a8[1],1;
        scl    a8[1],a8[0];
        ret;
    }
}

#[test]
fn scl_r() {
    aluasm_succ! {
        put    r8192[0],5;
        put    a16[1],1;
        put    r8192[2],10;
        scl    a16[1],r8192[0];
        eq.n   r8192[0],r8192[2];
        ret;
    }
    aluasm_succ! {
        put    r8192[0],5;
        put    a16[1],8192;
        put    r8192[2],5;
        scl    a8[1],r8192[0];
        eq.n   r8192[0],r8192[2];
        ret;
    }
    aluasm_succ! {
        put    r8192[0],2;
        put    a16[1],8191;
        put    r8192[2],1;
        scl    a16[1],r8192[0];
        eq.n   r8192[0],r8192[2];
        ret;
    }
    aluasm_succ! {
        put    r1024[0],2;
        put    a16[1],2049;
        put    r1024[2],4;
        scl    a16[1],r1024[0];
        eq.n   r1024[0],r1024[2];
        ret;
    }
}

#[test]
fn scl_r_st0() {
    aluasm_fail! {
        put    r8192[0],5;
        put    a16[1],1;
        scl    a16[1],r8192[0];
        ret;
    }
    aluasm_succ! {
        put    r8192[0],1;
        put    a16[1],8191;
        scl    a16[1],r8192[0];
        put    a16[1],1;
        scl    a16[1],r8192[0];
        ret;
    }
}

#[test]
fn scr_a() {
    aluasm_succ! {
        put    a16[0],10;
        put    a16[1],1;
        put    a16[2],5;
        scr    a16[1],a16[0];
        eq.n   a16[0],a16[2];
        ret;
    }
    aluasm_succ! {
        put    a8[0],130;
        put    a8[1],7;
        put    a8[2],5;
        scr    a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
    aluasm_succ! {
        put    a8[0],1;
        put    a8[1],7;
        put    a8[2],2;
        scr    a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
    aluasm_succ! {
        put    a8[0],4;
        put    a8[1],81;
        put    a8[2],2;
        scr    a8[1],a8[0];
        eq.n   a8[0],a8[2];
        ret;
    }
}

#[test]
fn scr_a_st0() {
    aluasm_fail! {
        put    a8[0],128;
        put    a8[1],1;
        scr    a8[1],a8[0];
        ret;
    }
    aluasm_succ! {
        put    a8[0],1;
        put    a8[1],1;
        scr    a8[1],a8[0];
        ret;
    }
}

#[test]
fn scr_r() {
    aluasm_succ! {
        put    r8192[0],10;
        put    a16[1],1;
        put    r8192[2],5;
        scr    a16[1],r8192[0];
        eq.n   r8192[0],r8192[2];
        ret;
    }
    aluasm_succ! {
        put    r8192[0],5;
        put    a16[1],8192;
        put    r8192[2],5;
        scr    a8[1],r8192[0];
        eq.n   r8192[0],r8192[2];
        ret;
    }
    aluasm_succ! {
        put    r8192[0],1;
        put    a16[1],8191;
        put    r8192[2],2;
        scr    a16[1],r8192[0];
        eq.n   r8192[0],r8192[2];
        ret;
    }
    aluasm_succ! {
        put    r1024[0],4;
        put    a16[1],2049;
        put    r1024[2],2;
        scr    a16[1],r1024[0];
        eq.n   r1024[0],r1024[2];
        ret;
    }
}

#[test]
fn scr_r_st0() {
    aluasm_fail! {
        put    r8192[0],4;
        put    a16[1],1;
        scr    a16[1],r8192[0];
        ret;
    }
    aluasm_succ! {
        put    r8192[0],1;
        put    a16[1],1;
        scr    a16[1],r8192[0];
        ret;
    }
}
