use crate::logic::{
    binder::*,
    symbol::{self, Symbol},
    theory::*,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term<T: Theory, B: IsBinder> {
    Binding(B, Vec<(Symbol, Sort<T>)>, Box<Term<T, B>>),
    Apply(Function<T>, Vec<Term<T, B>>),
    Const(Const<T>),
    Let(Symbol, Box<Term<T, B>>, Box<Term<T, B>>),
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
            Term::Let(name, box init, box body) => {
                Term::Let(name, box Self::from(init), box Self::from(body))
            }
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

    pub fn remove_let(self) -> Self {
        use Term::*;
        match self {
            Binding(q, bounds, box inner) => Binding(q, bounds, box inner.remove_let()),
            Apply(f, args) => {
                let args = args.into_iter().map(Self::remove_let).collect();
                Apply(f, args)
            }
            Const(c) => Const(c),
            Let(name, box init, box body) => body.subst_term(name.as_str(), init),
        }
    }

    pub fn subst_sort(self, name: &str, val: Sort<T>) -> Self {
        match self {
            Term::Binding(binder, params, box body) => {
                let params = params
                    .into_iter()
                    .map(|(param_name, param_sort)| {
                        if param_sort == Sort::Var(symbol::make(name)) {
                            (param_name, val.clone())
                        } else {
                            (param_name, param_sort)
                        }
                    })
                    .collect();
                let body = body.subst_sort(name, val);
                Term::Binding(binder, params, box body)
            }
            Term::Apply(f, args) => {
                let args = args
                    .into_iter()
                    .map(|arg| arg.subst_sort(name, val.clone()))
                    .collect();
                Term::Apply(f, args)
            }
            Term::Const(c) => Term::Const(c),
            Term::Let(name_, box init, box body) => Term::Let(
                name_,
                box init.subst_sort(name, val.clone()),
                box body.subst_sort(name, val),
            ),
        }
    }

    pub fn subst_term(self, name: &str, val: Self) -> Self {
        match self {
            Term::Binding(binder, params, box body) => {
                let body = if params.iter().any(|&(ref param, _)| param == name) {
                    body
                } else {
                    body.subst_term(name, val)
                };
                Term::Binding(binder, params, box body)
            }
            Term::Apply(f, args) => {
                let args = args
                    .into_iter()
                    .map(|arg| arg.subst_term(name, val.clone()))
                    .collect();
                Term::Apply(f, args)
            }
            Term::Const(Const::Var(ref name_)) if name == name_.as_str() => val,
            Term::Const(c) => Term::Const(c),
            Term::Let(name_, box init, box body) => {
                let (init, body) = if name == name_.as_str() {
                    (init.subst_term(name, val), body)
                } else {
                    (
                        init.subst_term(name, val.clone()),
                        body.subst_term(name, val),
                    )
                };
                Term::Let(name_, box init, box body)
            }
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
        fn aux<T: Theory>(term: Term<T, Quantifier>) -> Term<T, Quantifier> {
            match term {
                Let(_, _, _) => unreachable!(),
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
                        let args = args
                            .into_iter()
                            .map(Term::<T, Quantifier>::to_nnf)
                            .collect();
                        Apply(Function::Symbol(op), args)
                    }
                }
                Apply(f, args) => Apply(f, args),
                Const(c) => Const(c),
            }
        }
        aux(self.remove_let())
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
