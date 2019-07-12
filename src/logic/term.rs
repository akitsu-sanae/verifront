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
    fn acc(mut terms: Vec<Self>, op: boolean::FunctionSymbol) -> Self {
        use boolean::ConstSymbol::True;
        if let Some(hd) = terms.pop() {
            terms.into_iter().fold(hd, |acc, term| {
                Term::Apply(
                    Function::Symbol(T::FunctionSymbol::from(op)),
                    vec![acc, term],
                )
            })
        } else {
            Term::Const(Const::Symbol(T::ConstSymbol::from(True)))
        }
    }

    pub fn and_of(terms: Vec<Self>) -> Self {
        Self::acc(terms, boolean::FunctionSymbol::And)
    }
    pub fn or_of(terms: Vec<Self>) -> Self {
        Self::acc(terms, boolean::FunctionSymbol::Or)
    }

    pub fn subst(self, name: &str, val: Self) -> Self {
        match self {
            Term::Binding(binder, params, box body) => {
                let body = if params.iter().any(|&(ref param, _)| param == name) {
                    body
                } else {
                    body.subst(name, val)
                };
                Term::Binding(binder, params, box body)
            }
            Term::Apply(f, args) => {
                let args = args
                    .into_iter()
                    .map(|arg| arg.subst(name, val.clone()))
                    .collect();
                Term::Apply(f, args)
            }
            Term::Const(Const::Var(ref name_)) if name == name_.as_str() => val,
            Term::Const(c) => Term::Const(c),
        }
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
