use crate::ident;
use crate::logic::{*, Propos, FOL};
use crate::theory::Theory;
use crate::binder::{IsBinder, Quantifier};

use super::debug_print_check;

fn make_var<T: Theory, B: IsBinder>(name: &str) -> Expr<T, B> {
    Expr::Var(ident::make(name))
}

#[test]
fn theory() {
    use crate::theory::boolean::{SortSymbol::*, FunctionSymbol::*, Const::*};

    debug_print_check(make_var("A") as Propos, r#"Var("A")"#);

    debug_print_check( // not (A and (B and C))
        Propos::Apply(
            Function::Symbol(Not),
            vec!(
                Propos::Apply(
                    Function::Symbol(And),
                    vec!(
                        make_var("A"),
                        Propos::Apply(
                            Function::Symbol(And),
                            vec!(
                                make_var("B"),
                                make_var("C")
                            )))))),
            r#"Apply(Symbol(Not), [Apply(Symbol(And), [Var("A"), Apply(Symbol(And), [Var("B"), Var("C")])])])"#);

    debug_print_check( // forall a: Bool. a = true
        FOL::Binding(
            Quantifier::Forall,
            vec!((ident::make("a"), Sort::Symbol(Bool))),
            box FOL::Apply(
                Function::Symbol(Equal),
                vec!(
                    make_var("a"),
                    FOL::Const(True)
                ))),
                r#"Binding(Forall, [("a", Symbol(Bool))], Apply(Symbol(Equal), [Var("a"), Const(True)]))"#);
}

