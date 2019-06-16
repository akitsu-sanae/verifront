use std::collections::HashMap;
use crate::logic::{expr::*, binder::*, theory::*};
use crate::ident::Ident;

#[derive(Debug, Clone)]
struct Env<SS: IsSortSymbol> {
    data: HashMap<Ident, Sort<SS>>,
}

impl<SS: IsSortSymbol> Env<SS> {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }
    pub fn push(&self, ident: Ident, sort: Sort<SS>) -> Self {
        let mut ret = self.clone();
        ret.data.insert(ident, sort);
        ret
    }
    pub fn append(&self, elems: Vec<(Ident, Sort<SS>)>) -> Self {
        let mut ret = self.clone();
        for (ident, sort) in elems {
            ret.data.insert(ident, sort);
        }
        ret
    }
    pub fn lookup(&self, ident: &Ident) -> Result<Sort<SS>, String> {
        self.data.get(ident).cloned().ok_or(format!("not found {} in {:?} ", ident, self))
    }
}

fn unify<T: Theory>(params: Vec<Sort<T::SortSymbol>>, args: Vec<Sort<T::SortSymbol>>) -> Result<Env<T::SortSymbol>, String> {
    if params.len() != args.len() {
        return Err(format!("unmatch sorts between params and args: {:?} vs {:?}", params, args))
    }
    let mut subst = Env::new();
    for (i, param) in params.iter().enumerate() {
        let arg = &args[i];
        match param {
            Sort::Symbol(sym) => {
                if arg != &Sort::Symbol(sym.clone()) {
                    return Err(format!("unmatch sorts: {:?} vs {:?}", arg, param)) // FIXME: fix not to use `:?`
                }
            }
            Sort::Var(param) => {
                subst = subst.push(param.clone(), arg.clone())
            },
        }
    }
    Ok(subst)
}

fn apply_subst<T: Theory>(ret: Sort<T::SortSymbol>, subst: Env<T::SortSymbol>) -> Result<Sort<T::SortSymbol>, String> {
    match ret {
        Sort::Symbol(sym) => Ok(Sort::Symbol(sym)),
        Sort::Var(ident) => subst.lookup(&ident),
    }
}

fn expr_with_env<T: Theory, B: IsBinder>(expr: &Expr<T, B>, env: &Env<T::SortSymbol>) -> Result<Sort<T::SortSymbol>, String> {
    match expr {
        Expr::Binding(_binder, params, expr) => {
            let env = env.append(params.clone());
            expr_with_env(expr, &env)?;
            Ok(Sort::Symbol(T::SortSymbol::from(boolean::SortSymbol::Bool)))
        },
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
                },
                Function::Var(ident) => env.lookup(ident),
                _ => unreachable!(),
            }
        },
        Expr::Const(c) => {
            match c {
                Const::Symbol(cs) => Ok(cs.sort()),
                Const::Var(ident) => env.lookup(ident),
                _ => unreachable!(),
            }
        }
    }
}

pub fn expr<T: Theory, B: IsBinder>(expr: &Expr<T, B>) -> Result<Sort<T::SortSymbol>, String> {
    expr_with_env(expr, &Env::new())
}

