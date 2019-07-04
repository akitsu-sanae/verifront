use crate::ident::Ident;
use crate::logic::{binder::*, expr::*, theory::*};
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Env<T: Theory> {
    data: HashMap<Ident, Sort<T>>,
}

impl<T: Theory> Env<T> {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }
    pub fn push(&self, ident: Ident, sort: Sort<T>) -> Env<T> {
        let mut ret = self.clone();
        ret.data.insert(ident, sort);
        ret
    }
    pub fn append(&self, elems: Vec<(Ident, Sort<T>)>) -> Env<T> {
        let mut ret = self.clone();
        for (ident, sort) in elems {
            ret.data.insert(ident, sort);
        }
        ret
    }
    pub fn lookup(&self, ident: &Ident) -> Result<Sort<T>, String> {
        self.data
            .get(ident)
            .cloned()
            .ok_or(format!("not found {} in {:?} ", ident, self))
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
        Sort::Var(ident) => subst.lookup(&ident),
    }
}

fn expr_with_env<T: Theory, B: IsBinder>(
    expr: &Expr<T, B>,
    env: &Env<T>,
) -> Result<Sort<T>, String> {
    match expr {
        Expr::Binding(_binder, params, expr) => {
            let env = env.append(params.clone());
            expr_with_env(expr, &env)?;
            Ok(Sort::Symbol(T::SortSymbol::from(boolean::SortSymbol::Bool)))
        }
        Expr::Apply(f, args) => {
            let args: Result<Vec<_>, _> = args.iter().map(|arg| expr_with_env(arg, env)).collect();
            let args = args?;
            match f {
                Function::Symbol(fs) => {
                    let params = fs.arg_sorts();
                    let subst = unify::<T>(params, args)?;
                    let ret = fs.ret_sort();
                    let ret = apply_subst::<T>(ret, subst)?;
                    Ok(ret)
                }
                Function::Var(ident) => env.lookup(ident),
            }
        }
        Expr::Const(c) => match c {
            Const::Symbol(cs) => Ok(cs.sort()),
            Const::Var(ident) => env.lookup(ident),
        },
    }
}

pub fn expr<T: Theory, B: IsBinder>(expr: &Expr<T, B>) -> Result<Sort<T>, String> {
    expr_with_env(expr, &Env::new())
}
