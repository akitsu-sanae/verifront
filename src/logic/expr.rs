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

impl<T: Theory> Expr<T, Quantifier> {
    pub fn neg(self) -> Self {
        use boolean::FunctionSymbol::*;
        Expr::Apply(Function::Symbol(T::FunctionSymbol::from(Not)), vec![self])
    }

    pub fn to_nnf(self) -> Self {
        use boolean::FunctionSymbol::*;
        use Expr::*;
        match self {
            Binding(q, bounds, box inner) => Binding(q, bounds, box inner.to_nnf()),
            Apply(Function::Symbol(op), mut args) => {
                if op == T::FunctionSymbol::from(Not) {
                    let arg = args.pop().unwrap();
                    match arg {
                        Binding(op, bounds, box inner) => {
                            Binding(op.flip(), bounds, box inner.neg().to_nnf())
                        }
                        Apply(Function::Symbol(op), mut args)
                            if op.clone() == T::FunctionSymbol::from(Not) =>
                        {
                            args.pop().unwrap()
                        }
                        Apply(Function::Symbol(op), mut args)
                            if op.clone() == T::FunctionSymbol::from(And) =>
                        {
                            let rhs = args.pop().unwrap();
                            let lhs = args.pop().unwrap();
                            Apply(
                                Function::Symbol(T::FunctionSymbol::from(Or)),
                                vec![lhs.neg().to_nnf(), rhs.neg().to_nnf()],
                            )
                        }
                        Apply(Function::Symbol(op), mut args)
                            if op.clone() == T::FunctionSymbol::from(Or) =>
                        {
                            let rhs = args.pop().unwrap();
                            let lhs = args.pop().unwrap();
                            Apply(
                                Function::Symbol(T::FunctionSymbol::from(And)),
                                vec![lhs.neg().to_nnf(), rhs.neg().to_nnf()],
                            )
                        }
                        Apply(Function::Symbol(op), mut args)
                            if op.clone() == T::FunctionSymbol::from(Imply) =>
                        {
                            // not(lhs => rhs) = lhs and (not rhs)
                            let rhs = args.pop().unwrap();
                            let lhs = args.pop().unwrap();
                            Apply(
                                Function::Symbol(T::FunctionSymbol::from(And)),
                                vec![lhs.to_nnf(), rhs.neg().to_nnf()],
                            )
                        }
                        e => e.neg(),
                    }
                } else {
                    let args = args.into_iter().map(Self::to_nnf).collect();
                    Apply(Function::Symbol(op), args)
                }
            }
            e => e,
        }
    }
}

pub type ProposWithTheory<T> = Expr<T, EmptyBinder>;
pub type Propos = ProposWithTheory<boolean::Boolean>;
pub type FOLWithTheory<T> = Expr<T, Quantifier>;
pub type FOL = FOLWithTheory<boolean::Boolean>;
