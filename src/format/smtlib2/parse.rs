use sexp::{Atom, Sexp};
use std::error::Error;
use std::fmt;

use super::*;
use crate::util;

#[derive(Debug, Clone)]
pub enum ParseError {
    InvalidSexp(&'static str, Sexp),
    UnknownSortSymbol(String),
    UnknownFunctionSymbol(String),
    UnknownBinder(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseError::*;
        match self {
            InvalidSexp(kind, sexp) => {
                write!(f, "parse error: invalid sexp as {} : {}", kind, sexp)
            }
            UnknownSortSymbol(sym) => write!(f, "parse error: unknown sort symbol {}", sym),
            UnknownFunctionSymbol(sym) => write!(f, "parse error: unknown function symbol {}", sym),
            UnknownBinder(sym) => write!(f, "parse error: unknown binder {}", sym),
        }
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        "parse error"
    }
}

fn params_of_sexp<T: Smtlib2Theory, B: Smtlib2Binder>(
    sexp: &Sexp,
) -> Result<Vec<(Symbol, Sort<T>)>, ParseError> {
    if let Sexp::List(params) = sexp {
        let params: Result<Vec<_>, _> = params
            .iter()
            .map(|param| {
                if let Sexp::List(param) = param {
                    match param.as_slice() {
                        [Sexp::Atom(Atom::S(symbol)), sort] => {
                            let sort = sort_of_sexp::<T>(sort)?;
                            Ok((symbol.clone(), sort))
                        }
                        param => Err(ParseError::InvalidSexp("param", Sexp::List(param.to_vec()))),
                    }
                } else {
                    Err(ParseError::InvalidSexp("param", param.clone()))
                }
            })
            .collect();
        params
    } else {
        Err(ParseError::InvalidSexp("params", sexp.clone()))
    }
}

fn sort_of_sexp<T: Smtlib2Theory>(sexp: &Sexp) -> Result<Sort<T>, ParseError> {
    T::sort_symbol_of_sexp(sexp)
        .map(|fs| Sort::Symbol(fs))
        .or_else(|_| {
            if let Sexp::Atom(Atom::S(var)) = sexp {
                Ok(Sort::Var(symbol::make(&var)))
            } else {
                Err(ParseError::InvalidSexp("sort", sexp.clone()))
            }
        })
}

fn function_of_sexp<T: Smtlib2Theory, B: Smtlib2Binder>(
    sexp: &Sexp,
) -> Result<Function<T>, ParseError> {
    T::function_symbol_of_sexp(sexp)
        .map(|fs| Function::Symbol(fs))
        .or_else(|_| {
            if let Sexp::Atom(Atom::S(var)) = sexp {
                Ok(Function::Var(symbol::make(&var)))
            } else {
                Err(ParseError::InvalidSexp("function", sexp.clone()))
            }
        })
}

fn const_of_sexp<T: Smtlib2Theory, B: Smtlib2Binder>(sexp: &Sexp) -> Result<Const<T>, ParseError> {
    T::const_symbol_of_sexp(sexp)
        .map(|fs| Const::Symbol(fs))
        .or_else(|_| {
            if let Sexp::Atom(Atom::S(var)) = sexp {
                Ok(Const::Var(symbol::make(&var)))
            } else {
                Err(ParseError::InvalidSexp("const", sexp.clone()))
            }
        })
}

fn check_sat_assuming_of_sexp(sexp: &Sexp) -> Result<(Vec<Symbol>, Vec<Symbol>), ParseError> {
    if let Sexp::List(literals) = sexp {
        let mut poss = vec![];
        let mut negs = vec![];
        for literal in literals {
            // TODO: be more elegant
            if let Sexp::Atom(Atom::S(symbol)) = literal {
                poss.push(symbol::make(symbol.as_str()));
            } else if let Sexp::List(sexps) = literal {
                match sexps.as_slice() {
                    [Sexp::Atom(Atom::S(not)), Sexp::Atom(Atom::S(symbol))]
                        if not.as_str() == "not" =>
                    {
                        negs.push(symbol::make(symbol.as_str()));
                    }
                    sexps => {
                        return Err(ParseError::InvalidSexp(
                            "literal",
                            Sexp::List(sexps.to_vec()),
                        ))
                    }
                }
            } else {
                return Err(ParseError::InvalidSexp("literal", literal.clone()));
            }
        }
        Ok((poss, negs))
    } else {
        Err(ParseError::InvalidSexp("check-sat-assuming", sexp.clone()))
    }
}

fn selector_dec_of_sexp<T: Smtlib2Theory>(sexp: &Sexp) -> Result<SelectorDec<T>, ParseError> {
    let err = ParseError::InvalidSexp("selector_dec", sexp.clone());
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

fn constructor_dec_of_sexp<T: Smtlib2Theory>(sexp: &Sexp) -> Result<ConstructorDec<T>, ParseError> {
    let err = ParseError::InvalidSexp("constructor_dec", sexp.clone());
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
) -> Result<DatatypeDec<T>, ParseError> {
    // TODO: handle parameterized datatype declarion
    if let Sexp::List(sexps) = sexp {
        let mut constructor_decs = vec![];
        for sexp in sexps {
            constructor_decs.push(constructor_dec_of_sexp::<T>(sexp)?);
        }
        Ok(DatatypeDec {
            name: symbol::make(name),
            params: vec![],
            constructor_decs: constructor_decs,
        })
    } else {
        Err(ParseError::InvalidSexp("declare-datatype", sexp.clone()))
    }
}

fn datatype_decs_of_sexp<T: Smtlib2Theory>(sexp: &Sexp) -> Result<Vec<DatatypeDec<T>>, ParseError> {
    let err = ParseError::InvalidSexp("declare-datatype", sexp.clone());
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
    let err = ParseError::InvalidSexp(
        "declare-datatype",
        Sexp::List(vec![
            Sexp::List(fun_decs.clone()),
            Sexp::List(terms.clone()),
        ]),
    );
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
                        _ => Err(ParseError::InvalidSexp("binding", sexp.clone())),
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
            _ => Err(ParseError::InvalidSexp("expr", Sexp::List(sexps.to_vec()))),
        },
        Sexp::Atom(atom) => Ok(Expr::Const(
            const_of_sexp::<T, B>(&Sexp::Atom(atom.clone())).or_else(|_| match atom {
                Atom::S(name) => Ok(Const::Var(symbol::make(name))),
                _ => Err(ParseError::InvalidSexp("expr", Sexp::Atom(atom.clone()))),
            })?,
        )),
    }
}

fn info_flag_of_str(str: &str) -> InfoFlag {
    use InfoFlag::*;
    match str {
        ":all-statistics" => AllStatistics,
        ":assertion-stack-levels" => AssertionStackLevels,
        ":authors" => Authors,
        ":error-behavior" => ErrorBehavior,
        ":name" => Name,
        ":reason-unknown" => ReasonUnknown,
        ":version" => Version,
        str => Keyword(str.to_string()),
    }
}

fn attr_value_of_sexp(sexp: &Sexp) -> Result<AttributeValue, ParseError> {
    use AttributeValue::*;
    Ok(match sexp {
        // FIXME
        Sexp::Atom(Atom::S(str)) => Symbol(str.clone()),
        sexp => return Err(ParseError::InvalidSexp("attribute-value", sexp.clone())),
    })
}

fn attr_of_sexp(sexp: &[Sexp]) -> Result<Attribute, ParseError> {
    use Attribute::*;
    Ok(match sexp {
        [Sexp::Atom(Atom::S(str))] => Keyword(str.clone()),
        [Sexp::Atom(Atom::S(str)), attr_value] => {
            KeywordWithAttributeValue(str.clone(), attr_value_of_sexp(attr_value)?)
        }
        sexp => {
            return Err(ParseError::InvalidSexp(
                "attribute",
                Sexp::List(sexp.to_vec()),
            ))
        }
    })
}

fn sexp_to_bool(str: &String) -> Result<bool, ParseError> {
    Ok(match str.as_str() {
        "true" => true,
        "false" => false,
        e => return Err(ParseError::InvalidSexp("bool", util::make_str_atom(e))),
    })
}

fn option_of_sexp(opt: &[Sexp]) -> Result<Option, ParseError> {
    Ok(match opt {
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::S(str))]
            if keyword.as_str() == ":diagnostic-output-channel" =>
        {
            Option::DiagnosticOutputChannel(str.clone())
        }
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::S(str))]
            if keyword.as_str() == ":global-declarations" =>
        {
            Option::GlobalDeclarations(sexp_to_bool(str)?)
        }
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::S(str))]
            if keyword.as_str() == ":interactive-mode" =>
        {
            Option::InteractiveMode(sexp_to_bool(str)?)
        }
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::S(str))]
            if keyword.as_str() == ":print-success" =>
        {
            Option::PrintSuccess(sexp_to_bool(str)?)
        }
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::S(str))]
            if keyword.as_str() == ":produce-assertions" =>
        {
            Option::ProduceAssertions(sexp_to_bool(str)?)
        }
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::S(str))]
            if keyword.as_str() == ":produce-assignment" =>
        {
            Option::ProduceAssignments(sexp_to_bool(str)?)
        }
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::S(str))]
            if keyword.as_str() == ":produce-models" =>
        {
            Option::ProduceModels(sexp_to_bool(str)?)
        }
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::S(str))]
            if keyword.as_str() == ":produce-proofs" =>
        {
            Option::ProduceProofs(sexp_to_bool(str)?)
        }
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::S(str))]
            if keyword.as_str() == ":produce-unsat-assumptions" =>
        {
            Option::ProduceUnsatAssumptions(sexp_to_bool(str)?)
        }
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::S(str))]
            if keyword.as_str() == ":produce-unsat-cores" =>
        {
            Option::ProduceUnsatCores(sexp_to_bool(str)?)
        }
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::I(n))]
            if keyword.as_str() == ":random-seed" =>
        {
            Option::RandomSeed(*n)
        }
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::S(str))]
            if keyword.as_str() == ":regular-output-channel" =>
        {
            Option::RegularOutputChannel(str.clone())
        }
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::I(n))]
            if keyword.as_str() == ":reproducible-resource-limit" =>
        {
            Option::ReproducibleResourceLimit(*n)
        }
        [Sexp::Atom(Atom::S(keyword)), Sexp::Atom(Atom::I(n))]
            if keyword.as_str() == ":verbosity" =>
        {
            Option::Verbosity(*n)
        }
        attr => Option::Attribute(attr_of_sexp(attr)?),
    })
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
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(symbol)), sort]
                    if head.as_str() == "declare-const" =>
                {
                    let sort = sort_of_sexp::<T>(sort)?;
                    Command::DeclareConst((symbol.clone(), sort))
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
                            Sexp::Atom(Atom::S(symbol)) => Ok(symbol.clone()),
                            sexp => Err(ParseError::InvalidSexp("sort dec", sexp.clone())),
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
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(symbol)), Sexp::Atom(Atom::I(n))]
                    if head.as_str() == "declare-sort" =>
                {
                    Command::DeclareSort(symbol.clone(), *n)
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
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(symbol)), Sexp::List(params), sort]
                    if head.as_str() == "define-sort" =>
                {
                    let params: Result<Vec<_>, _> = params
                        .iter()
                        .map(|param| {
                            if let Sexp::Atom(Atom::S(symbol)) = param {
                                Ok(symbol.clone())
                            } else {
                                Err(ParseError::InvalidSexp("symbol", param.clone()))
                            }
                        })
                        .collect();
                    let params = params?;
                    let sort = sort_of_sexp::<T>(sort)?;
                    Command::DefineSort(symbol.clone(), params, sort)
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(msg))]
                    if head.as_str() == "echo" =>
                {
                    Command::Echo(msg.clone())
                }
                [Sexp::Atom(Atom::S(head))] if head.as_str() == "exit" => Command::Exit,
                [Sexp::Atom(Atom::S(head))] if head.as_str() == "get-assertions" => {
                    Command::GetAssertions
                }
                [Sexp::Atom(Atom::S(head))] if head.as_str() == "get-assignment" => {
                    Command::GetAssignment
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(info_flag))]
                    if head.as_str() == "get-info" =>
                {
                    Command::GetInfo(info_flag_of_str(info_flag.as_str()))
                }
                [Sexp::Atom(Atom::S(head))] if head.as_str() == "get-model" => Command::GetModel,
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(keyword))]
                    if head.as_str() == "get-option" =>
                {
                    Command::GetOption(keyword.clone())
                }
                [Sexp::Atom(Atom::S(head))] if head.as_str() == "get-proof" => Command::GetProof,
                [Sexp::Atom(Atom::S(head))] if head.as_str() == "get-unsat-assumptions" => {
                    Command::GetUnsatAssumptions
                }
                [Sexp::Atom(Atom::S(head))] if head.as_str() == "get-unsat-core" => {
                    Command::GetUnsatCore
                }
                [Sexp::Atom(Atom::S(head)), Sexp::List(es)] if head.as_str() == "get-value" => {
                    let es: Result<Vec<_>, _> = es.iter().map(|e| expr_of_sexp(e)).collect();
                    let es = es?;
                    Command::GetValue(es)
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::I(n))] if head.as_str() == "pop" => {
                    Command::Pop(*n)
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::I(n))] if head.as_str() == "push" => {
                    Command::Push(*n)
                }
                [Sexp::Atom(Atom::S(head))] if head.as_str() == "reset" => Command::Reset,
                [Sexp::Atom(Atom::S(head))] if head.as_str() == "reset-assertions" => {
                    Command::ResetAssertions
                }
                [Sexp::Atom(Atom::S(head)), Sexp::List(attr)] if head.as_str() == "set-info" => {
                    Command::SetInfo(attr_of_sexp(attr)?)
                }
                [Sexp::Atom(Atom::S(head)), Sexp::Atom(Atom::S(sym))]
                    if head.as_str() == "set-logic" =>
                {
                    Command::SetLogic(sym.clone())
                }
                [Sexp::Atom(Atom::S(head)), opt..] if head.as_str() == "set-option" => {
                    Command::SetOption(option_of_sexp(opt)?)
                }
                toplevel => {
                    return Err(ParseError::InvalidSexp(
                        "toplevel",
                        Sexp::List(toplevel.to_vec()),
                    ))
                }
            }
        } else {
            return Err(ParseError::InvalidSexp("toplevel", toplevel.clone()));
        };
        commands.push(cmd);
    }
    Ok(Smtlib2 { commands: commands })
}
