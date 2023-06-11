macro_rules! aluasm_succ {
    ($( $tt:tt )+) => { {
        aluvm_macro_succ! { $( $tt )+ };
        let (res, runtime) = aluasm_compiler! { $( $tt )+ };
        assert!(res, "aluasm_compiler: expected success:\n{:#?}", runtime.registers);
    } }
}

macro_rules! aluasm_fail {
    ($( $tt:tt )+) => { {
        aluvm_macro_fail! { $( $tt )+ };
        let (res, runtime) = aluasm_compiler! { $( $tt )+ };
        assert!(!res, "aluasm_compiler: expected failure:\n{:#?}", runtime.registers);
    } }
}

macro_rules! aluvm_macro_succ {
    ($( $tt:tt )+) => { {
        let (res, runtime) = aluvm_macro! { $( $tt )+ };
        assert!(res, "aluvm_macro: expected success\n{:#?}", runtime.registers);
    } }
}

macro_rules! aluvm_macro_fail {
    ($( $tt:tt )+) => { {
        let (res, runtime) = aluvm_macro! { $( $tt )+ };
        assert!(!res, "aluvm_macro: expected failure\n{:#?}", runtime.registers);
    } }
}

macro_rules! aluasm_compiler {
    ($( $tt:tt )+) => { {
        use pest::Parser;
        let main = stringify!($( $tt )+);
        let main = main.replace(";", ";\n");
        let main = main.replace(" [", "[");
        let main = main.replace("\n[", "[");
        let code = format!(
            r#".ISAE ; ISA Extensions segment
                    ALU
               .MAIN ; Code segment
                    {}
        "#, main);
        let pairs = aluasm::parser::Parser::parse(aluasm::parser::Rule::program, &code).unwrap();
        let (program, _) = aluasm::ast::Program::analyze(pairs.into_iter().next().unwrap()).unwrap();
        let (module, _) = program.compile(&mut None).unwrap();
        let mut runtime = aluvm::Vm::<aluvm::isa::Instr>::new();
        let program = aluvm::Prog::<aluvm::isa::Instr>::new(module.as_static_lib().clone());
        let res = runtime.run(&program, &());
        (res, runtime)
    } }
}

macro_rules! aluvm_macro {
    ($( $tt:tt )+) => { {
        let mut runtime = aluvm::Vm::<aluvm::isa::Instr>::new();
        let code = aluvm::aluasm! { $( $tt )+ };
        let program = aluvm::Prog::<aluvm::isa::Instr>::new(aluvm::library::Lib::assemble(&code).unwrap());
        let res = runtime.run(&program, &());
        (res, runtime)
    } }
}

#[test]
fn a8_ne() {
    aluasm_fail! {
        put     12,a8[1];
        put     9,a8[2];
        eq.n    a8[1],a8[2];
        ret;
    }
}

#[test]
fn a8_eq() {
    aluasm_succ! {
        put     9,a8[1];
        put     9,a8[2];
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
        put     4,a16[1];
        put     4,a16[2];
        eq.n    a16[1],a16[2];
        ret;
    }
}

#[test]
fn a_eq_fail() {
    aluasm_fail! {
        put     3,a16[1];
        put     4,a16[2];
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
        put     2,a8[1];
        put     1,a8[2];
        gt.u    a8[1],a8[2];
        ret;
    }
}

#[test]
fn a_gt_s() {
    aluasm_succ! {
        put     1,a8[1];
        put     255,a8[2]; // -1
        gt.s    a8[1],a8[2];
        ret;
    }
    // negative literal works only on macro
    aluvm_macro_succ! {
        put     -1,a8[1];
        put     1,a8[2];
        lt.s    a8[1],a8[2];
        ret;
    };
    aluasm_fail! {
        put     1,a8[1];
        put     2,a8[2];
        gt.s    a8[1],a8[2];
        ret;
    }
}

#[test]
fn a_lt_u() {
    aluasm_succ! {
        put     1,a8[1];
        put     2,a8[2];
        lt.u    a8[1],a8[2];
        ret;
    }
}

#[test]
fn a_lt_s() {
    aluasm_succ! {
        put     255,a8[1]; // -1
        put     1,a8[2];
        lt.s    a8[1],a8[2];
        ret;
    }
    aluasm_fail! {
        put     2,a8[1];
        put     1,a8[2];
        lt.s    a8[1],a8[2];
        ret;
    }
}

#[test]
fn stp_add() {
    aluasm_succ! {
        put     3,a8[1];
        add     4,a8[1];
        put     7,a8[2];
        eq.n    a8[1],a8[2];
        ret;
    }
}

#[test]
fn stp_sub() {
    aluasm_fail! {
        put     3,a8[1];
        sub     4,a8[1];
        put     127,a8[2]; // -1
        eq.n    a8[1],a8[2];
        ret;
    }
}

#[test]
fn float() {
    aluasm_succ! {
            put   1.25,f32[8];
            put   1.5,f32[9];
            put   2.75,f32[10];
            add.f f32[8],f32[9];
            eq.e  f32[9],f32[10];
            ret;
    }
}
