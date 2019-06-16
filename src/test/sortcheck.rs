use crate::ident;
use crate::logic::{binder::*, expr::*, theory::*};
use crate::sortcheck;

type FOL = crate::logic::expr::FOLWithTheory<integer::Integer>;

fn make_var<T: Theory, B: IsBinder>(name: &str) -> Expr<T, B> {
    Expr::Const(Const::Var(ident::make(name)))
}

fn check(expr: FOL, sort: Sort<integer::SortSymbol>) {
    assert_eq!(sortcheck::expr(&expr).unwrap(), sort)
}

#[test]
fn literal() {
    use crate::logic::theory::integer::{*, FunctionSymbol::*};
    use crate::logic::theory::boolean::{ConstSymbol::*, FunctionSymbol::*};

    check( // True
        FOL::Const(Const::Symbol(ConstSymbol::Boolean(True))),
        Sort::Symbol(SortSymbol::Bool));

    check( // 42
        FOL::Const(Const::Symbol(ConstSymbol::Number(42))),
        Sort::Symbol(SortSymbol::Int));
}

#[test]
fn binding() {
    use crate::logic::theory::integer::{*, FunctionSymbol::*};
    use crate::logic::theory::boolean::{ConstSymbol::*, FunctionSymbol::*};

    check( // forall a: Bool. a = true
        FOL::Binding(
            Quantifier::Forall,
            vec!((ident::make("a"), Sort::Symbol(SortSymbol::Bool))),
            box FOL::Apply(
                Function::Symbol(FunctionSymbol::Boolean(Equal)),
                vec!(
                    make_var("a"),
                    FOL::Const(Const::Symbol(ConstSymbol::Boolean(True))),
                ))),
        Sort::Symbol(SortSymbol::Bool));

    check( // exists a: Int. 42 + a = 0
        FOL::Binding(
            Quantifier::Exists,
            vec!((ident::make("a"), Sort::Symbol(SortSymbol::Int))),
            box FOL::Apply(
                Function::Symbol(FunctionSymbol::Boolean(Equal)),
                vec!(
                    FOL::Apply(
                        Function::Symbol(Add),
                        vec!(
                            FOL::Const(Const::Symbol(ConstSymbol::Number(42))),
                            make_var("a"))),
                    FOL::Const(Const::Symbol(ConstSymbol::Number(0)))))),
        Sort::Symbol(SortSymbol::Bool));
}
