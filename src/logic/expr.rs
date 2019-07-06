use crate::logic::{binder::*, symbol::Symbol, theory::*};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<T: Theory, B: IsBinder> {
    Binding(B, Vec<(Symbol, Sort<T>)>, Box<Expr<T, B>>),
    Apply(Function<T>, Vec<Expr<T, B>>),
    Const(Const<T>),
}

impl<T: Theory, B: IsBinder> Expr<T, B> {
    fn acc(mut exprs: Vec<Self>, op: boolean::FunctionSymbol) -> Self {
        use boolean::ConstSymbol::True;
        if let Some(hd) = exprs.pop() {
            exprs.into_iter().fold(hd, |acc, expr| {
                Expr::Apply(
                    Function::Symbol(T::FunctionSymbol::from(op)),
                    vec![acc, expr],
                )
            })
        } else {
            Expr::Const(Const::Symbol(T::ConstSymbol::from(True)))
        }
    }

    pub fn and_of(exprs: Vec<Self>) -> Self {
        Self::acc(exprs, boolean::FunctionSymbol::And)
    }
    pub fn or_of(exprs: Vec<Self>) -> Self {
        Self::acc(exprs, boolean::FunctionSymbol::Or)
    }
}

pub type ProposWithTheory<T> = Expr<T, EmptyBinder>;
pub type Propos = ProposWithTheory<boolean::Boolean>;
pub type FOLWithTheory<T> = Expr<T, Quantifier>;
pub type FOL = FOLWithTheory<boolean::Boolean>;
