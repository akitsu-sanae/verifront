use crate::logic::{binder::*, expr::*, sortcheck, symbol, theory::*};

type FOL = crate::logic::expr::FOLWithTheory<integer::Integer>;

fn make_var<T: Theory, B: IsBinder>(name: &str) -> Expr<T, B> {
    Expr::Const(Const::Var(symbol::make(name)))
}

fn check(expr: FOL, sort: Sort<integer::Integer>) {
    assert_eq!(sortcheck::expr(&expr).unwrap(), sort)
}

#[test]
fn literal() {
    use crate::logic::theory::boolean::ConstSymbol::*;
    use crate::logic::theory::integer::*;
    check(
        // True
        FOL::Const(Const::Symbol(ConstSymbol::Boolean(True))),
        Sort::Symbol(SortSymbol::Bool),
    );

    check(
        // 42
        FOL::Const(Const::Symbol(ConstSymbol::Number(42))),
        Sort::Symbol(SortSymbol::Int),
    );
}

#[test]
fn binding() {
    use crate::logic::theory::boolean::{ConstSymbol::*, FunctionSymbol::*};
    use crate::logic::theory::integer::{FunctionSymbol::*, *};

    check(
        // forall a: Bool. a = true
        FOL::Binding(
            Quantifier::Forall,
            vec![(symbol::make("a"), Sort::Symbol(SortSymbol::Bool))],
            box FOL::Apply(
                Function::Symbol(FunctionSymbol::Boolean(Equal)),
                vec![
                    make_var("a"),
                    FOL::Const(Const::Symbol(ConstSymbol::Boolean(True))),
                ],
            ),
        ),
        Sort::Symbol(SortSymbol::Bool),
    );

    check(
        // exists a: Int. 42 + a = 0
        FOL::Binding(
            Quantifier::Exists,
            vec![(symbol::make("a"), Sort::Symbol(SortSymbol::Int))],
            box FOL::Apply(
                Function::Symbol(FunctionSymbol::Boolean(Equal)),
                vec![
                    FOL::Apply(
                        Function::Symbol(Add),
                        vec![
                            FOL::Const(Const::Symbol(ConstSymbol::Number(42))),
                            make_var("a"),
                        ],
                    ),
                    FOL::Const(Const::Symbol(ConstSymbol::Number(0))),
                ],
            ),
        ),
        Sort::Symbol(SortSymbol::Bool),
    );
}
