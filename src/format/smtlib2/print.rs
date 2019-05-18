use std::error::Error;
use std::fmt;
use sexp::{Sexp, Atom};

use crate::ident::Ident;
use crate::logic::*;

use super::{Smtlib2Theory, Smtlib2Binder};

#[derive(Debug)]
pub struct PrintError {
    msg: String, // FIXME
}

impl fmt::Display for PrintError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "print error: {}", self.msg)
    }
}

impl Error for PrintError {
    fn description(&self) -> &str { "print error" }
}

impl PrintError {
    pub fn  new(msg: String) -> Self {
        Self {
            msg: msg
        }
    }
}

fn sexp_of_sort<T: Smtlib2Theory>(sort: &Sort<T::SortSymbol>) -> Result<Sexp, PrintError> {
    match sort {
        Sort::Var(ident) => Ok(Sexp::Atom(sexp::Atom::S(ident.clone()))),
        Sort::Symbol(sym) => T::sexp_of_sort_symbol(sym),
    }
}

fn sexp_of_function<T: Smtlib2Theory>(fun: &Function<T::FunctionSymbol>) -> Result<Sexp, PrintError> {
    match fun {
        Function::Var(ident) => Ok(Sexp::Atom(sexp::Atom::S(ident.clone()))),
        Function::Symbol(sym) => T::sexp_of_function_symbol(sym),
    }
}

fn sexp_of_params<T: Smtlib2Theory>(params: &Vec<(Ident, Sort<T::SortSymbol>)>) -> Result<Sexp, PrintError>
{
    let params: Result<Vec<_>, _> = params.iter().map(|(ident, sort)| {
        let ident = Sexp::Atom(sexp::Atom::S(ident.clone()));
        let sort = sexp_of_sort::<T>(sort)?;
        Ok(Sexp::List(vec!(ident, sort)))
    }).collect();
    let params = params?;
    Ok(Sexp::List(params))
}

fn sexp_of_expr<T, B>(expr: &Expr<T, B>) -> Result<Sexp, PrintError>
    where T: Smtlib2Theory, B: Smtlib2Binder
{
    use Expr::*;
    match expr {
        Binding(binder, params, box phi) => {
            Ok(Sexp::List(vec!(
                    B::sexp_of_binder(binder)?,
                    sexp_of_params::<T>(params)?,
                    sexp_of_expr(phi)?)))
        }
        Apply(fun, args)=> {
            let args: Result<Vec<_>, _> = args.into_iter().map(|arg| sexp_of_expr(arg)).collect();
            let mut args = args?;
            let mut exprs = vec!(sexp_of_function::<T>(fun)?);
            exprs.append(&mut args);
            Ok(Sexp::List(exprs))
        },
        Const(c) => T::sexp_of_const(c),
        Var(ident) => Ok(Sexp::Atom(sexp::Atom::S(ident.clone()))),
    }
}

pub fn toplevels<T, B>(expr: &Expr<T, B>) -> Result<Vec<Sexp>, PrintError>
    where T: Smtlib2Theory, B: Smtlib2Binder
{
    let mut result = vec!();
    result.push(Sexp::List(vec!(
                Sexp::Atom(Atom::S("assert".to_string())),
                sexp_of_expr(expr)?)));
    result.push(Sexp::List(vec!(Sexp::Atom(Atom::S("check-sat".to_string())))));
    Ok(result)
}

