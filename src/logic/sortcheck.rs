use super::{binder::*, symbol::Symbol, term::*, theory::*};
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Env<T: Theory> {
    data: HashMap<Symbol, Sort<T>>,
}

impl<T: Theory> Env<T> {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }
    pub fn push(&self, symbol: Symbol, sort: Sort<T>) -> Env<T> {
        let mut ret = self.clone();
        ret.data.insert(symbol, sort);
        ret
    }
    pub fn append(&self, elems: Vec<(Symbol, Sort<T>)>) -> Env<T> {
        let mut ret = self.clone();
        for (symbol, sort) in elems {
            ret.data.insert(symbol, sort);
        }
        ret
    }
    pub fn lookup(&self, symbol: &Symbol) -> Result<Sort<T>, String> {
        self.data
            .get(symbol)
            .cloned()
            .ok_or(format!("not found {} in {:?} ", symbol, self))
    }
}

fn unify<T: Theory>(params: Vec<Sort<T>>, args: Vec<Sort<T>>) -> Result<Env<T>, String> {
    if params.len() != args.len() {
        return Err(format!(
            "unmatch sorts between params and args: {:?} vs {:?}",
            params, args
        ));
    }
    let mut subst = Env::new();
    for (i, param) in params.iter().enumerate() {
        let arg = &args[i];
        match param {
            Sort::Symbol(sym) => {
                if arg != &Sort::Symbol(sym.clone()) {
                    return Err(format!("unmatch sorts: {:?} vs {:?}", arg, param)); // FIXME: fix not to use `:?`
                }
            }
            Sort::Var(param) => subst = subst.push(param.clone(), arg.clone()),
        }
    }
    Ok(subst)
}

fn apply_subst<T: Theory>(ret: Sort<T>, subst: Env<T>) -> Result<Sort<T>, String> {
    match ret {
        Sort::Symbol(sym) => Ok(Sort::Symbol(sym)),
        Sort::Var(symbol) => subst.lookup(&symbol),
    }
}

fn term_with_env<T: Theory, B: IsBinder>(
    term: &Term<T, B>,
    env: &Env<T>,
) -> Result<Sort<T>, String> {
    match term {
        Term::Binding(_binder, params, term) => {
            let env = env.append(params.clone());
            term_with_env(term, &env)?;
            Ok(Sort::Symbol(T::SortSymbol::from(boolean::SortSymbol::Bool)))
        }
        Term::Apply(f, args) => {
            let args: Result<Vec<_>, _> = args.iter().map(|arg| term_with_env(arg, env)).collect();
            let args = args?;
            match f {
                Function::Symbol(fs) => {
                    let params = fs.arg_sorts();
                    let subst = unify::<T>(params, args)?;
                    let ret = fs.ret_sort();
                    let ret = apply_subst::<T>(ret, subst)?;
                    Ok(ret)
                }
                Function::Var(symbol) => env.lookup(symbol),
            }
        }
        Term::Const(c) => match c {
            Const::Symbol(cs) => Ok(cs.sort()),
            Const::Var(symbol) => env.lookup(symbol),
        },
    }
}

pub fn term<T: Theory, B: IsBinder>(term: &Term<T, B>) -> Result<Sort<T>, String> {
    term_with_env(term, &Env::new())
}
