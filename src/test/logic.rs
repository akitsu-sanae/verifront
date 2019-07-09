use crate::logic::{binder::*, symbol, term::*, theory::*};

use super::debug_print_check;

fn make_var<T: Theory, B: IsBinder>(name: &str) -> Term<T, B> {
    Term::Const(Const::Var(symbol::make(name)))
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
            vec!((symbol::make("a"), Sort::Symbol(Bool))),
            box FOL::Apply(
                Function::Symbol(Equal),
                vec!(
                    make_var("a"),
                    FOL::Const(Const::Symbol(True)),
                ))),
                r#"Binding(Forall, [("a", Symbol(Bool))], Apply(Symbol(Equal), [Const(Var("a")), Const(Symbol(True))]))"#);
}

#[test]
fn nnf() {
    use boolean::FunctionSymbol::{And, Equal, Or};
    use integer::ConstSymbol::Number;
    use Quantifier::*;
    let int_sort: Sort<integer::Integer> = Sort::Symbol(integer::SortSymbol::Int);
    fn make_fun(sym: boolean::FunctionSymbol) -> Function<integer::Integer> {
        Function::Symbol(integer::FunctionSymbol::from(sym))
    }
    // not (forall x. x=1 or x = 2)
    // exists x. not(x=1) and not(x=2)
    assert_eq!(
        Term::Binding(
            Forall,
            vec![(symbol::make("x"), int_sort.clone())],
            box Term::Apply(
                make_fun(Or),
                vec![
                    Term::Apply(
                        make_fun(Equal),
                        vec![
                            Term::Const(Const::Var(symbol::make("x"))),
                            Term::Const(Const::Symbol(Number(1))),
                        ]
                    ),
                    Term::Apply(
                        make_fun(Equal),
                        vec![
                            Term::Const(Const::Var(symbol::make("x"))),
                            Term::Const(Const::Symbol(Number(2))),
                        ]
                    ),
                ]
            )
        )
        .neg()
        .to_nnf(),
        Term::Binding(
            Exists,
            vec![(symbol::make("x"), int_sort.clone())],
            box Term::Apply(
                make_fun(And),
                vec![
                    Term::Apply(
                        make_fun(Equal),
                        vec![
                            Term::Const(Const::Var(symbol::make("x"))),
                            Term::Const(Const::Symbol(Number(1))),
                        ]
                    )
                    .neg(),
                    Term::Apply(
                        make_fun(Equal),
                        vec![
                            Term::Const(Const::Var(symbol::make("x"))),
                            Term::Const(Const::Symbol(Number(2))),
                        ]
                    )
                    .neg()
                ]
            )
        )
    );

    // forall x. not (x=1 or not(x=2))
    // forall x. not(x=1) and x=2
    assert_eq!(
        Term::Binding(
            Forall,
            vec![(symbol::make("x"), int_sort.clone())],
            box Term::Apply(
                make_fun(Or),
                vec![
                    Term::Apply(
                        make_fun(Equal),
                        vec![
                            Term::Const(Const::Var(symbol::make("x"))),
                            Term::Const(Const::Symbol(Number(1))),
                        ]
                    ),
                    Term::Apply(
                        make_fun(Equal),
                        vec![
                            Term::Const(Const::Var(symbol::make("x"))),
                            Term::Const(Const::Symbol(Number(2))),
                        ]
                    )
                    .neg(),
                ]
            )
            .neg()
        )
        .to_nnf(),
        Term::Binding(
            Forall,
            vec![(symbol::make("x"), int_sort.clone())],
            box Term::Apply(
                make_fun(And),
                vec![
                    Term::Apply(
                        make_fun(Equal),
                        vec![
                            Term::Const(Const::Var(symbol::make("x"))),
                            Term::Const(Const::Symbol(Number(1))),
                        ]
                    )
                    .neg(),
                    Term::Apply(
                        make_fun(Equal),
                        vec![
                            Term::Const(Const::Var(symbol::make("x"))),
                            Term::Const(Const::Symbol(Number(2))),
                        ]
                    )
                ]
            )
        )
    );
}
