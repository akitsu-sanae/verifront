use super::debug_print_check;

use crate::program::{domain::integer::*, ident};

#[test]
fn ml_program() {
    use crate::program::domain::boolean;
    use crate::program::ml::BooleanProgram;
    debug_print_check(
        // if a then true else b
        BooleanProgram::If(
            box BooleanProgram::Var(ident::make("a")),
            box BooleanProgram::Const(boolean::ConstSymbol::True),
            box BooleanProgram::Var(ident::make("b")),
        ),
        r#"If(Var("a"), Const(True), Var("b"))"#,
    );
}

#[test]
fn c_program() {
    use crate::program::c::*;
    debug_print_check(
        // fun main a { return a + 42; }
        vec![Function::<Integer> {
            name: ident::make("main"),
            params: vec![ident::make("a")],
            body: vec![Inst::Return(Expr::Op(
                OperatorSymbol::Add,
                vec![
                    Expr::Var(ident::make("a")),
                    Expr::Const(ConstSymbol::Num(42)),
                ],
            ))],
        }],
        r#"[Function { name: "main", params: ["a"], body: [Return(Op(Add, [Var("a"), Const(Num(42))]))] }]"#,
    );
}
