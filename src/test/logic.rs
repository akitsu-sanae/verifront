use crate::ident;
use crate::logic::{binder::*, expr::*, theory::*};

use super::debug_print_check;

fn make_var<T: Theory, B: IsBinder>(name: &str) -> Expr<T, B> {
    Expr::Const(Const::Var(ident::make(name)))
}

#[test]
fn theory() {
    use crate::logic::theory::boolean::{ConstSymbol::*, FunctionSymbol::*, SortSymbol::*};

    debug_print_check(make_var("A") as Propos, r#"Const(Var("A"))"#);

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
            r#"Apply(Symbol(Not), [Apply(Symbol(And), [Const(Var("A")), Apply(Symbol(And), [Const(Var("B")), Const(Var("C"))])])])"#);

    debug_print_check( // forall a: Bool. a = true
        FOL::Binding(
            Quantifier::Forall,
            vec!((ident::make("a"), Sort::Symbol(Bool))),
            box FOL::Apply(
                Function::Symbol(Equal),
                vec!(
                    make_var("a"),
                    FOL::Const(Const::Symbol(True)),
                ))),
                r#"Binding(Forall, [("a", Symbol(Bool))], Apply(Symbol(Equal), [Const(Var("a")), Const(Symbol(True))]))"#);
}
