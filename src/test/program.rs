use super::debug_print_check;

#[test]
fn program() {
    use crate::ident;
    use crate::program::{boolean::Constant::*, BoolExpr};
    debug_print_check(
        // if a then true else b
        BoolExpr::If(
            box BoolExpr::Var(ident::make("a")),
            box BoolExpr::Constant(True),
            box BoolExpr::Var(ident::make("b")),
        ),
        r#"If(Var("a"), Constant(True), Var("b"))"#,
    );
}
