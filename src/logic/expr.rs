use crate::logic::{
    theory::*,
    binder::*
};
use crate::ident::Ident;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<T: Theory, B: IsBinder> {
    Binding(B, Vec<(Ident, Sort<T::SortSymbol>)>, Box<Expr<T, B>>),
    Apply(Function<T::SortSymbol, T::FunctionSymbol>, Vec<Expr<T, B>>),
    Const(Const<T::SortSymbol, T::ConstSymbol>),
}

impl<T: Theory, B: IsBinder> Expr<T, B> {
    pub fn and_of(mut exprs: Vec<Self>) -> Self {
        use boolean::ConstSymbol::True;
        use boolean::FunctionSymbol::And;
        if let Some(hd) = exprs.pop() {
            exprs.into_iter().fold(hd, |acc, expr| {
                Expr::Apply(Function::Symbol(T::FunctionSymbol::from(And)), vec!(acc, expr))
            })
        } else {
            Expr::Const(Const::Symbol(T::ConstSymbol::from(True)))
        }
    }
}

pub type ProposWithTheory<T> = Expr<T, EmptyBinder>;
pub type Propos = ProposWithTheory<boolean::Boolean>;
pub type FOLWithTheory<T> = Expr<T, Quantifier>;
pub type FOL = FOLWithTheory<boolean::Boolean>;

