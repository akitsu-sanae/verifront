use sexp::{Atom, Sexp};
use std::error::Error;
use std::fmt;

use super::*;
use crate::ident::{self, Ident};
use crate::logic::theory::*;

#[derive(Debug)]
pub struct ParseError {
    msg: String, // FIXME
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "parser error: {}", self.msg)
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        "parse error"
    }
}

impl ParseError {
    pub fn new(msg: String) -> Self {
        Self { msg: msg }
    }
}

fn params_of_sexp<T: Smtlib2Theory, B: Smtlib2Binder>(
    sexp: &Sexp,
) -> Result<Vec<(Ident, Sort<T::SortSymbol>)>, ParseError> {
    if let Sexp::List(params) = sexp {
        let params: Result<Vec<_>, _> = params
            .iter()
            .map(|param| {
                if let Sexp::List(param) = param {
                    match param.as_slice() {
                        [Sexp::Atom(Atom::S(ident)), sort] => {
                            let sort = sort_of_sexp::<T, B>(sort)?;
                            Ok((ident.clone(), sort))
                        }
                        param => Err(ParseError::new(format!(
                            "invalid sexp as param : {:?}",
                            param
                        ))),
                    }
                } else {
                    Err(ParseError::new(format!(
                        "invalid sexp as param : {}",
                        param
                    )))
                }
            })
            .collect();
        params
    } else {
        Err(ParseError::new(format!(
            "invalid sexp as params : {}",
            sexp
        )))
    }
}

fn sort_of_sexp<T: Smtlib2Theory, B: Smtlib2Binder>(
    sexp: &Sexp,
) -> Result<Sort<T::SortSymbol>, ParseError> {
    T::sort_symbol_of_sexp(sexp)
        .map(|fs| Sort::Symbol(fs))
        .or_else(|_| {
            if let Sexp::Atom(Atom::S(var)) = sexp {
                Ok(Sort::Var(ident::make(&var)))
            } else {
                Err(ParseError::new(format!("invalid sexp as sort : {}", sexp)))
            }
        })
}

fn function_of_sexp<T: Smtlib2Theory, B: Smtlib2Binder>(
    sexp: &Sexp,
) -> Result<Function<T::SortSymbol, T::FunctionSymbol>, ParseError> {
    T::function_symbol_of_sexp(sexp)
        .map(|fs| Function::Symbol(fs))
        .or_else(|_| {
            if let Sexp::Atom(Atom::S(var)) = sexp {
                Ok(Function::Var(ident::make(&var)))
            } else {
                Err(ParseError::new(format!(
                    "invalid sexp as function : {}",
                    sexp
                )))
            }
        })
}

fn const_of_sexp<T: Smtlib2Theory, B: Smtlib2Binder>(
    sexp: &Sexp,
) -> Result<Const<T::SortSymbol, T::ConstSymbol>, ParseError> {
    T::const_symbol_of_sexp(sexp)
        .map(|fs| Const::Symbol(fs))
        .or_else(|_| {
            if let Sexp::Atom(Atom::S(var)) = sexp {
                Ok(Const::Var(ident::make(&var)))
            } else {
                Err(ParseError::new(format!("invalid sexp as const : {}", sexp)))
            }
        })
}

fn datatype_dec_of_sexp<T: Smtlib2Theory>(
    _sexp: &Sexp,
) -> Result<DatatypeDec<T::SortSymbol>, ParseError> {
    unimplemented!();
}

fn expr_of_sexp<T: Smtlib2Theory, B: Smtlib2Binder>(sexp: &Sexp) -> Result<Expr<T, B>, ParseError> {
    match sexp {
        Sexp::List(sexps) => match sexps.as_slice() {
            [head, tail..] => {
                if let Ok(binder) = B::binder_of_sexp(head) {
                    match tail {
                        [params, expr] => {
                            let params = params_of_sexp::<T, B>(params)?;
                            let expr = expr_of_sexp(expr)?;
                            Ok(Expr::Binding(binder, params, box expr))
                        }
                        _ => Err(ParseError::new(format!(
                            "invalid sexp as binding : {}",
                            sexp
                        ))),
                    }
                } else if let Ok(fun) = function_of_sexp::<T, B>(head) {
                    let args: Result<_, _> = tail.iter().map(|arg| expr_of_sexp(arg)).collect();
                    let args = args?;
                    use boolean::FunctionSymbol::{And, Or};
                    if fun == Function::Symbol(T::FunctionSymbol::from(And)) {
                        Ok(Expr::and_of(args))
                    } else if fun == Function::Symbol(T::FunctionSymbol::from(Or)) {
                        Ok(Expr::or_of(args))
                    } else {
                        Ok(Expr::Apply(fun, args))
                    }
                } else {
                    Ok(Expr::Const(const_of_sexp::<T, B>(sexp)?))
                }
            }
            _ => Err(ParseError::new(format!(
                "invalid sexp as expr : {}",
                Sexp::List(sexps.to_vec())
            ))),
        },
        Sexp::Atom(atom) => Ok(Expr::Const(
            const_of_sexp::<T, B>(&Sexp::Atom(atom.clone())).or_else(|_| match atom {
                Atom::S(name) => Ok(Const::Var(ident::make(name))),
                _ => Err(ParseError::new(format!(
                    "invalid sexp as expr : {}",
                    Sexp::Atom(atom.clone())
                ))),
            })?,
        )),
    }
}

pub fn toplevels<T, B>(toplevels: &Vec<Sexp>) -> Result<Smtlib2<T, B>, ParseError>
where
    T: Smtlib2Theory,
    B: Smtlib2Binder,
{
    let mut commands = vec![];
    for toplevel in toplevels {
        let cmd = if let Sexp::List(toplevel) = toplevel {
            match toplevel.as_slice() {
                [Sexp::Atom(Atom::S(head)), expr] if head.as_str() == "assert" => {
                    Command::Assert(expr_of_sexp(expr)?)
                }
                [Sexp::Atom(Atom::S(head))] if head.as_str() == "check-sat" => Command::CheckSat,
                [Sexp::Atom(Atom::S(head))] if head.as_str() == "check-sat-assuming" => {
                    Command::CheckSatAssuming(vec![], vec![])
                } // TODO
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(ident)), sort]
                    if head.as_str() == "declare-const" =>
                {
                    let sort = sort_of_sexp::<T, B>(sort)?;
                    Command::DeclareConst((ident.clone(), sort))
                }
                [Sexp::Atom(Atom::S(head)), datatype_dec]
                    if head.as_str() == "declare-datatype" =>
                {
                    let datatype_dec = datatype_dec_of_sexp::<T>(datatype_dec)?;
                    Command::DeclareDatatype(datatype_dec)
                }
                [Sexp::Atom(Atom::S(head))] if head.as_str() == "declare-datatypes" => {
                    unimplemented!()
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(name)), Sexp::List(params), sort]
                    if head.as_str() == "declare-fun" =>
                {
                    let params: Result<Vec<_>, _> =
                        params.iter().map(sort_of_sexp::<T, B>).collect();
                    let params = params?;
                    let sort = sort_of_sexp::<T, B>(sort)?;
                    Command::DeclareFun(FunDec {
                        name: name.to_string(),
                        params: params,
                        ret: sort,
                    })
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(ident)), Sexp::Atom(Atom::I(n))]
                    if head.as_str() == "declare-sort" =>
                {
                    Command::DeclareSort(ident.clone(), *n)
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(name)), params, sort, expr]
                    if head.as_str() == "define-fun" =>
                {
                    let params = params_of_sexp::<T, B>(params)?;
                    let sort = sort_of_sexp::<T, B>(sort)?;
                    let expr = expr_of_sexp(expr)?;
                    Command::DefineFun(FunDef {
                        name: name.to_string(),
                        params: params,
                        ret: sort,
                        body: expr,
                    })
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(name)), params, sort, expr]
                    if head.as_str() == "define-fun-rec" =>
                {
                    let params = params_of_sexp::<T, B>(params)?;
                    let sort = sort_of_sexp::<T, B>(sort)?;
                    let expr = expr_of_sexp(expr)?;
                    Command::DefineFunRec(FunDef {
                        name: name.to_string(),
                        params: params,
                        ret: sort,
                        body: expr,
                    })
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(ident)), Sexp::List(params), sort]
                    if head.as_str() == "define-sort" =>
                {
                    let params: Result<Vec<_>, _> = params
                        .iter()
                        .map(|param| {
                            if let Sexp::Atom(Atom::S(ident)) = param {
                                Ok(ident.clone())
                            } else {
                                Err(ParseError::new(format!(
                                    "ident expected, but {} come",
                                    param
                                )))
                            }
                        })
                        .collect();
                    let params = params?;
                    let sort = sort_of_sexp::<T, B>(sort)?;
                    Command::DefineSort(ident.clone(), params, sort)
                }
                toplevel => {
                    return Err(ParseError::new(format!("invalid toplevel {:?}", toplevel)))
                }
            }
        } else {
            return Err(ParseError::new(format!("invalid toplevel {:?}", toplevel)));
        };
        commands.push(cmd);
    }
    Ok(Smtlib2 { commands: commands })
}
