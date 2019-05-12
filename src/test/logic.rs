use crate::ident;
use crate::logic::{*, Propos, FOL};
use crate::theory::Theory;

use super::debug_print_check;

fn make_var<T: Theory, B: IsBinder>(name: &str) -> Expr<T, B> {
    Expr::Var(ident::make(name))
}

#[test]
fn theory() {
    use crate::theory::boolean::{FunctionSymbol::*, Const::*};

    debug_print_check(make_var("A") as Propos, r#"Var("A")"#);

    debug_print_check( // not (A and (B and C))
        Propos::Apply(
            Not,
            vec!(
                Propos::Apply(
                    And,
                    vec!(
                        make_var("A"),
                        Propos::Apply(
                            And,
                            vec!(
                                make_var("B"),
                                make_var("C")
                            )))))),
            r#"Apply(Not, [Apply(And, [Var("A"), Apply(And, [Var("B"), Var("C")])])])"#);

    debug_print_check( // forall a. a = true
        FOL::Binding(
            Quantifier::Forall,
            vec!(ident::make("a")),
            box FOL::Apply(
                Equal,
                vec!(
                    make_var("a"),
                    FOL::Const(True)
                ))),
                r#"Binding(Forall, ["a"], Apply(Equal, [Var("a"), Const(True)]))"#);
}

