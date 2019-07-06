use super::debug_print_check;

#[test]
fn program() {
    use crate::program::{boolean::Constant::*, make_ident, BoolExpr};
    debug_print_check(
        // if a then true else b
        BoolExpr::If(
            box BoolExpr::Var(make_ident("a")),
            box BoolExpr::Constant(True),
            box BoolExpr::Var(make_ident("b")),
        ),
        r#"If(Var("a"), Constant(True), Var("b"))"#,
    );
}
