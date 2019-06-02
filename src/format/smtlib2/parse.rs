use sexp::{Atom, Sexp};
use std::error::Error;
use std::fmt;

use super::*;
use crate::ident::{self, Ident};
use crate::logic::theory::*;

#[derive(Debug, Clone)]
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
                            let sort = sort_of_sexp::<T>(sort)?;
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

fn sort_of_sexp<T: Smtlib2Theory>(sexp: &Sexp) -> Result<Sort<T::SortSymbol>, ParseError> {
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

fn check_sat_assuming_of_sexp(sexp: &Sexp) -> Result<(Vec<Ident>, Vec<Ident>), ParseError> {
    if let Sexp::List(literals) = sexp {
        let mut poss = vec![];
        let mut negs = vec![];
        for literal in literals {
            // TODO: be more elegant
            if let Sexp::Atom(Atom::S(ident)) = literal {
                poss.push(ident::make(ident.as_str()));
            } else if let Sexp::List(sexps) = literal {
                match sexps.as_slice() {
                    [Sexp::Atom(Atom::S(not)), Sexp::Atom(Atom::S(ident))]
                        if not.as_str() == "not" =>
                    {
                        negs.push(ident::make(ident.as_str()));
                    }
                    sexps => {
                        return Err(ParseError::new(format!(
                            "invalid sexp as literal: {}",
                            Sexp::List(sexps.to_vec())
                        )))
                    }
                }
            } else {
                return Err(ParseError::new(format!(
                    "invalid sexp as literal: {}",
                    literal
                )));
            }
        }
        Ok((poss, negs))
    } else {
        Err(ParseError::new(format!(
            "invalid sexp as check-sat-assuming : {}",
            sexp
        )))
    }
}

fn selector_dec_of_sexp<T: Smtlib2Theory>(
    sexp: &Sexp,
) -> Result<SelectorDec<T::SortSymbol>, ParseError> {
    let err = ParseError::new(format!("invalid sexp as selector_dec : {}", sexp));
    if let Sexp::List(sexps) = sexp {
        match sexps.as_slice() {
            [Sexp::Atom(Atom::S(name)), sort] => {
                let sort = sort_of_sexp::<T>(sort)?;
                Ok(SelectorDec {
                    name: name.clone(),
                    sort: sort,
                })
            }
            _ => Err(err),
        }
    } else {
        Err(err)
    }
}

fn constructor_dec_of_sexp<T: Smtlib2Theory>(
    sexp: &Sexp,
) -> Result<ConstructorDec<T::SortSymbol>, ParseError> {
    let err = ParseError::new(format!("invalid sexp as constructor_dec : {}", sexp));
    if let Sexp::List(sexps) = sexp {
        let mut sexp_iter = sexps.iter();
        let name = sexp_iter
            .next()
            .and_then(|head| match head {
                Sexp::Atom(Atom::S(name)) => Some(name),
                _ => None,
            })
            .ok_or(err)?;
        let mut selector_decs = vec![];
        for sexp in sexp_iter {
            selector_decs.push(selector_dec_of_sexp::<T>(sexp)?);
        }
        Ok(ConstructorDec {
            name: name.clone(),
            selector_decs: selector_decs,
        })
    } else {
        Err(err)
    }
}

fn datatype_dec_of_sexp<T: Smtlib2Theory>(
    name: &str,
    sexp: &Sexp,
) -> Result<DatatypeDec<T::SortSymbol>, ParseError> {
    // TODO: handle parameterized datatype declarion
    if let Sexp::List(sexps) = sexp {
        let mut constructor_decs = vec![];
        for sexp in sexps {
            constructor_decs.push(constructor_dec_of_sexp::<T>(sexp)?);
        }
        Ok(DatatypeDec {
            name: ident::make(name),
            params: vec![],
            constructor_decs: constructor_decs,
        })
    } else {
        Err(ParseError::new(format!(
            "invalid sexp as declare-datatype : {}",
            sexp
        )))
    }
}

fn datatype_decs_of_sexp<T: Smtlib2Theory>(
    sexp: &Sexp,
) -> Result<Vec<DatatypeDec<T::SortSymbol>>, ParseError> {
    let err = ParseError::new(format!("invalid sexp as declare-datatype : {}", sexp));
    if let Sexp::List(sexps) = sexp {
        let mut datatype_decs = vec![];
        for sexp in sexps.iter() {
            if let Sexp::List(sexps) = sexp {
                let mut sexp_iter = sexps.iter();
                let name = sexp_iter
                    .next()
                    .and_then(|head| match head {
                        Sexp::Atom(Atom::S(name)) => Some(name),
                        _ => None,
                    })
                    .ok_or(err.clone())?;
                let sexp = Sexp::List(sexp_iter.cloned().collect()); // TODO: remove `coloned` for performance
                datatype_decs.push(datatype_dec_of_sexp::<T>(name, &sexp)?);
            } else {
                return Err(err.clone());
            }
        }
        Ok(datatype_decs)
    } else {
        Err(err)
    }
}

fn fun_defs_of_sexp<T: Smtlib2Theory, B: Smtlib2Binder>(
    fun_decs: &Vec<Sexp>,
    terms: &Vec<Sexp>,
) -> Result<Vec<FunDef<T, B>>, ParseError> {
    let err = ParseError::new(format!(
        "invalid sexp as declare-datatype : {:?}, {:?}",
        fun_decs, terms
    ));
    let mut fun_defs = vec![];
    for (i, fun_dec) in fun_decs.iter().enumerate() {
        let term = &terms[i]; // TODO: more safety
        let fun_dec = if let Sexp::List(fun_dec) = fun_dec {
            fun_dec
        } else {
            return Err(err.clone());
        };
        match fun_dec.as_slice() {
            [Sexp::Atom(Atom::S(name)), params, ret_sort] => {
                let params = params_of_sexp::<T, B>(params)?;
                let ret_sort = sort_of_sexp::<T>(ret_sort)?;
                let term = expr_of_sexp::<T, B>(term)?;
                fun_defs.push(FunDef {
                    name: name.clone(),
                    params: params,
                    ret: ret_sort,
                    body: term,
                });
            }
            _ => return Err(err.clone()),
        }
    }
    Ok(fun_defs)
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
                [Sexp::Atom(Atom::S(head)), body] if head.as_str() == "check-sat-assuming" => {
                    let (poss, negs) = check_sat_assuming_of_sexp(body)?;
                    Command::CheckSatAssuming(poss, negs)
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(ident)), sort]
                    if head.as_str() == "declare-const" =>
                {
                    let sort = sort_of_sexp::<T>(sort)?;
                    Command::DeclareConst((ident.clone(), sort))
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(name)), datatype_dec_body]
                    if head.as_str() == "declare-datatype" =>
                {
                    let datatype_dec = datatype_dec_of_sexp::<T>(name, datatype_dec_body)?;
                    Command::DeclareDatatype(datatype_dec)
                }
                [Sexp::Atom(Atom::S(head)), Sexp::List(sort_decs), datatype_decs]
                    if head.as_str() == "declare-datatypes" =>
                {
                    let sort_decs: Result<Vec<_>, _> = sort_decs
                        .iter()
                        .map(|sort_dec| match sort_dec {
                            Sexp::Atom(Atom::S(ident)) => Ok(ident.clone()),
                            sexp => Err(ParseError::new(format!("invalid sort dec : {}", sexp))),
                        })
                        .collect();
                    let sort_decs = sort_decs?;

                    let datatype_decs = datatype_decs_of_sexp::<T>(datatype_decs)?;

                    Command::DeclareDatatypes(sort_decs, datatype_decs)
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(name)), Sexp::List(params), sort]
                    if head.as_str() == "declare-fun" =>
                {
                    let params: Result<Vec<_>, _> = params.iter().map(sort_of_sexp::<T>).collect();
                    let params = params?;
                    let sort = sort_of_sexp::<T>(sort)?;
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
                    let sort = sort_of_sexp::<T>(sort)?;
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
                    let sort = sort_of_sexp::<T>(sort)?;
                    let expr = expr_of_sexp(expr)?;
                    Command::DefineFunRec(FunDef {
                        name: name.to_string(),
                        params: params,
                        ret: sort,
                        body: expr,
                    })
                }
                [Sexp::Atom(Atom::S(head)), Sexp::List(fun_decs), Sexp::List(terms)]
                    if head.as_str() == "define-funs-rec" =>
                {
                    Command::DefineFunsRec(fun_defs_of_sexp(fun_decs, terms)?)
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
                    let sort = sort_of_sexp::<T>(sort)?;
                    Command::DefineSort(ident.clone(), params, sort)
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(msg))]
                    if head.as_str() == "echo" =>
                {
                    Command::Echo(msg.clone())
                }
                [Sexp::Atom(Atom::S(head))] if head.as_str() == "exit" => Command::Exit,
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
