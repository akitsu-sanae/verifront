use super::debug_print_check;

#[test]
fn ml_program() {
    use crate::program::{domain::boolean::*, ident, ml::BooleanProgram};
    debug_print_check(
        // if a then true else b
        BooleanProgram::If(
            box BooleanProgram::Var(ident::make("a")),
            box BooleanProgram::Const(ConstSymbol::True),
            box BooleanProgram::Var(ident::make("b")),
        ),
        r#"If(Var("a"), Const(True), Var("b"))"#,
    );
}
