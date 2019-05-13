use crate::theory::{Theory, boolean};
use crate::binder::{IsBinder, EmptyBinder, Quantifier};
use crate::ident::Ident;

#[derive(Debug, Clone)]
pub enum Sort<S> { // S : Sort Symbols
    Symbol (S),
    Var (Ident),
}

#[derive(Debug, Clone)]
pub enum Function<F> { // F : Function Symbol
    Symbol(F),
    Var (Ident),
}

#[derive(Debug, Clone)]
pub enum Expr<T: Theory, Binder> {
    Binding(Binder, Vec<(Ident, Sort<T::SortSymbol>)>, Box<Expr<T, Binder>>),
    Apply(Function<T::FunctionSymbol>, Vec<Expr<T, Binder>>),
    Const(T::Const),
    Var(Ident),
}

impl<T: Theory, B: IsBinder> Expr<T, B> {
    pub fn and_of(mut exprs: Vec<Self>) -> Self {
        use boolean::Const::True;
        use boolean::FunctionSymbol::And;
        if let Some(hd) = exprs.pop() {
            exprs.into_iter().fold(hd, |acc, expr| {
                Expr::Apply(Function::Symbol(T::FunctionSymbol::from(And)), vec!(acc, expr))
            })
        } else {
            Expr::Const(T::Const::from(True))
        }

        /*
        match &exprs[..] {
            [] => Expr::Const(T::Const::from(True)),
            [ref hd, ref tl ..] =>
                tl.into_iter().fold(*hd, |acc, expr| {
                    Expr::Apply(T::FunctionSymbol::from(And), vec!(acc, *expr))
                })
        } */
    }
}

pub type ProposWithTheory<T> = Expr<T, EmptyBinder>;
pub type Propos = ProposWithTheory<boolean::Boolean>;
pub type FOLWithTheory<T> = Expr<T, Quantifier>;
pub type FOL = FOLWithTheory<boolean::Boolean>;

