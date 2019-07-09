use crate::logic::{binder::*, symbol::Symbol, theory::*};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term<T: Theory, B: IsBinder> {
    Binding(B, Vec<(Symbol, Sort<T>)>, Box<Term<T, B>>),
    Apply(Function<T>, Vec<Term<T, B>>),
    Const(Const<T>),
}

impl<T: Theory> From<Term<T, EmptyBinder>> for Term<T, Quantifier> {
    fn from(phi: Term<T, EmptyBinder>) -> Self {
        match phi {
            Term::Binding(_, _, _) => unreachable!(),
            Term::Apply(f, args) => {
                let args = args.into_iter().map(|arg| Self::from(arg)).collect();
                Term::Apply(f, args)
            }
            Term::Const(c) => Term::Const(c),
        }
    }
}

impl<T: Theory, B: IsBinder> Term<T, B> {
    fn acc(mut exprs: Vec<Self>, op: boolean::FunctionSymbol) -> Self {
        use boolean::ConstSymbol::True;
        if let Some(hd) = exprs.pop() {
            exprs.into_iter().fold(hd, |acc, expr| {
                Term::Apply(
                    Function::Symbol(T::FunctionSymbol::from(op)),
                    vec![acc, expr],
                )
            })
        } else {
            Term::Const(Const::Symbol(T::ConstSymbol::from(True)))
        }
    }

    pub fn and_of(exprs: Vec<Self>) -> Self {
        Self::acc(exprs, boolean::FunctionSymbol::And)
    }
    pub fn or_of(exprs: Vec<Self>) -> Self {
        Self::acc(exprs, boolean::FunctionSymbol::Or)
    }
}

impl<T: Theory> Term<T, Quantifier> {
    pub fn neg(self) -> Self {
        use boolean::FunctionSymbol::*;
        Term::Apply(Function::Symbol(T::FunctionSymbol::from(Not)), vec![self])
    }

    pub fn to_nnf(self) -> Self {
        use boolean::FunctionSymbol::*;
        use Term::*;
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

impl<T: Theory> Term<T, EmptyBinder> {
    pub fn to_cnf(self) -> super::cnf::Cnf<T> {
        super::cnf::Cnf::from_formula(self)
    }
}

pub type ProposWithTheory<T> = Term<T, EmptyBinder>;
pub type Propos = ProposWithTheory<boolean::Boolean>;
pub type FOLWithTheory<T> = Term<T, Quantifier>;
pub type FOL = FOLWithTheory<boolean::Boolean>;
