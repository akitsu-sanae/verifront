use crate::logic::{binder::*, symbol, term::*, theory::*};

use super::debug_print_check;

fn make_var<T: Theory, B: IsBinder>(name: &str) -> Term<T, B> {
    Term::Const(Const::Var(symbol::make(name)))
}

fn make_true() -> Propos {
    use crate::logic::theory::boolean::ConstSymbol::True;
    Propos::Const(Const::Symbol(True))
}

fn make_integer(n: i64) -> FOLWithTheory<integer::Integer> {
    use integer::ConstSymbol::Number;
    Term::Const(Const::Symbol(Number(n)))
}

#[test]
fn subst_term() {
    use crate::logic::theory::boolean::{ConstSymbol::*, FunctionSymbol::*};

    // x[true/x] = true
    assert_eq!(
        make_var("x").subst_term("x", Propos::Const(Const::Symbol(True))),
        make_true()
    );

    // (x and x)[true/x] = (true and true)
    assert_eq!(
        Propos::and_of(vec![make_var("x"), make_var("x"),]).subst_term("x", make_true()),
        Propos::and_of(vec![make_true(), make_true(),])
    );

    use crate::logic::theory::integer::{FunctionSymbol::*, SortSymbol::*};
    // (forall (x, y). x + y + z = 0)[42/y][12/z] = (forall (x, y). x + y + 12 = 0)
    type FOL = FOLWithTheory<integer::Integer>;
    assert_eq!(
        FOL::Binding(
            Quantifier::Forall,
            vec![
                (symbol::make("x"), Sort::Symbol(Int)),
                (symbol::make("y"), Sort::Symbol(Int))
            ],
            box FOL::Apply(
                Function::Symbol(integer::FunctionSymbol::from(Equal)),
                vec![
                    FOL::Apply(
                        Function::Symbol(Add),
                        vec![
                            make_var("x"),
                            FOL::Apply(Function::Symbol(Add), vec![make_var("y"), make_var("z")])
                        ]
                    ),
                    make_integer(0),
                ]
            )
        )
        .subst_term("y", make_integer(42))
        .subst_term("z", make_integer(12)),
        FOL::Binding(
            Quantifier::Forall,
            vec![
                (symbol::make("x"), Sort::Symbol(Int)),
                (symbol::make("y"), Sort::Symbol(Int))
            ],
            box FOL::Apply(
                Function::Symbol(integer::FunctionSymbol::from(Equal)),
                vec![
                    FOL::Apply(
                        Function::Symbol(Add),
                        vec![
                            make_var("x"),
                            FOL::Apply(
                                Function::Symbol(Add),
                                vec![make_var("y"), make_integer(12)]
                            )
                        ]
                    ),
                    make_integer(0),
                ]
            )
        )
    );
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
    fn make_fun(sym: boolean::FunctionSymbol) -> Function<integer::Integer, Quantifier> {
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
