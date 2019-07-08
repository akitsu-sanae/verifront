use super::binder::EmptyBinder;
use super::expr::Expr;
use super::theory::{boolean, Const, Function, Theory};

pub type Formula<T> = Expr<T, EmptyBinder>;

#[derive(Debug, Clone)]
pub struct Atom<T: Theory>(Formula<T>);

#[derive(Debug, Clone)]
pub struct Literal<T: Theory> {
    pub is_neg: bool,
    pub atom: Atom<T>,
}

impl<T: Theory> Literal<T> {
    pub fn make(atom: Atom<T>) -> Self {
        Literal {
            is_neg: false,
            atom: atom,
        }
    }

    pub fn make_neg(atom: Atom<T>) -> Self {
        Literal {
            is_neg: true,
            atom: atom,
        }
    }

    pub fn neg(mut self) -> Self {
        self.is_neg = !self.is_neg;
        self
    }
}

#[derive(Debug, Clone)]
pub struct Clause<T: Theory>(Vec<Literal<T>>); // disjunction
#[derive(Debug, Clone)]
pub struct Cnf<T: Theory>(Vec<Clause<T>>); // conjunction

lazy_static! {
    static ref CNF_FRESH_COUNTER: ::std::sync::RwLock<i32> = ::std::sync::RwLock::new(0);
}

fn cnf_fresh_var<T: Theory>() -> Literal<T> {
    let mut fresh_counter = CNF_FRESH_COUNTER.write().unwrap();
    let name = format!("cnf.fresh-counter.{}", fresh_counter);
    *fresh_counter += 1;
    Literal::make(Atom(Expr::Const(Const::Var(name))))
}

impl<T: Theory> Cnf<T> {
    pub fn from_formula(phi: Formula<T>) -> Self {
        use boolean::FunctionSymbol::{And, Not, Or};
        fn aux<T: Theory>(
            phi: Formula<T>,
            clauses: Vec<Clause<T>>,
        ) -> (Literal<T>, Vec<Clause<T>>) {
            match phi {
                Expr::Binding(_, _, _) => unreachable!(),
                Expr::Apply(Function::Symbol(op), mut args)
                    if args.len() == 1 && op == T::FunctionSymbol::from(Not) =>
                {
                    let arg = args.pop().unwrap();
                    let (phi, clauses) = aux(arg, clauses);
                    (phi.neg(), clauses)
                }
                Expr::Apply(Function::Symbol(op), mut args)
                    if args.len() == 2 && op == T::FunctionSymbol::from(And) =>
                {
                    let arg2 = args.pop().unwrap();
                    let arg1 = args.pop().unwrap();
                    let (phi1, clauses) = aux(arg1, clauses);
                    let (phi2, mut clauses) = aux(arg2, clauses);
                    let p = cnf_fresh_var();
                    clauses.push(Clause(vec![p.clone().neg(), phi1.clone()]));
                    clauses.push(Clause(vec![p.clone().neg(), phi2.clone()]));
                    clauses.push(Clause(vec![phi1.neg(), phi2.neg(), p.clone()]));
                    (p, clauses)
                }
                Expr::Apply(Function::Symbol(op), mut args)
                    if args.len() == 2 && op == T::FunctionSymbol::from(Or) =>
                {
                    let arg2 = args.pop().unwrap();
                    let arg1 = args.pop().unwrap();
                    let (phi1, clauses) = aux(arg1, clauses);
                    let (phi2, mut clauses) = aux(arg2, clauses);
                    let p = cnf_fresh_var();
                    clauses.push(Clause(vec![p.clone().neg(), phi1.clone(), phi2.clone()]));
                    clauses.push(Clause(vec![phi1.neg(), p.clone()]));
                    clauses.push(Clause(vec![phi2.neg(), p.clone()]));
                    (p, clauses)
                }
                phi => (Literal::make(Atom(phi)), clauses),
            }
        }
        let (phi, mut clauses) = aux(phi, vec![]);
        clauses.push(Clause(vec![phi]));
        Cnf(clauses)
    }
}
