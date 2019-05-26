use std::error::Error;
use std::fmt;
use sexp::Sexp;

use crate::util;
use crate::ident::Ident;

use super::*;

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

fn sexp_of_function<T: Smtlib2Theory>(fun: &Function<T::SortSymbol, T::FunctionSymbol>) -> Result<Sexp, PrintError> {
    match fun {
        Function::Var(ident) => Ok(Sexp::Atom(sexp::Atom::S(ident.clone()))),
        Function::Symbol(sym) => T::sexp_of_function_symbol(sym),
        Function::_Phantom(_) => unreachable!(),
    }
}

fn sexp_of_params<T: Smtlib2Theory>(params: &Vec<SortedSymbol<T::SortSymbol>>) -> Result<Sexp, PrintError>
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
        Const(c) => {
            use crate::logic::theory::Const::*;
            Ok(match c {
                Symbol(cs) => T::sexp_of_const_symbol(cs)?,
                Var(ident) => util::make_str_atom(ident),
                _Phantom(_) => unreachable!(),
            })
        }
    }
}

fn sexp_of_fundec<T: Smtlib2Theory>(dec_fun: &FunDec<T>) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec!(
            util::make_str_atom("declare-fun"),
            util::make_str_atom(dec_fun.name.as_str()),
            Sexp::List({
                let params: Result<Vec<_>, _> = dec_fun.params.iter()
                    .map(|param| sexp_of_sort::<T>(param)).collect();
                params?
            }),
            sexp_of_sort::<T>(&dec_fun.ret)?)))
}

fn sexp_of_declare_sort(ident: &String, n: i64) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec!(
            util::make_str_atom(ident.as_str()),
            util::make_int_atom(n))))
}

fn sexp_of_fundef<T: Smtlib2Theory, B: Smtlib2Binder>(def_fun: &FunDef<T, B>) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec!(
            util::make_str_atom("define-fun"),
            util::make_str_atom(def_fun.name.as_str()),
            sexp_of_params::<T>(&def_fun.params)?,
            sexp_of_sort::<T>(&def_fun.ret)?,
            sexp_of_expr(&def_fun.body)?)))
}

fn sexp_of_fundef_rec<T: Smtlib2Theory, B: Smtlib2Binder>(def_fun: &FunDef<T, B>) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec!(
            util::make_str_atom("define-fun-rec"),
            util::make_str_atom(def_fun.name.as_str()),
            sexp_of_params::<T>(&def_fun.params)?,
            sexp_of_sort::<T>(&def_fun.ret)?,
            sexp_of_expr(&def_fun.body)?)))
}

fn sexp_of_funsdef_rec<T: Smtlib2Theory, B: Smtlib2Binder>(def_funs: &Vec<FunDef<T, B>>) -> Result<Sexp, PrintError> {
    let mut fun_decs = vec!();
    let mut terms = vec!();
    for def_fun in def_funs.iter() {
        fun_decs.push(Sexp::List(vec!(
                    util::make_str_atom(def_fun.name.as_str()),
                    sexp_of_params::<T>(&def_fun.params)?,
                    sexp_of_sort::<T>(&def_fun.ret)?)));
        terms.push(sexp_of_expr::<T, B>(&def_fun.body)?);
    }
    Ok(Sexp::List(vec!(
                util::make_str_atom("define-funs-rec"),
                Sexp::List(fun_decs),
                Sexp::List(terms))))
}

fn sexp_of_define_sort<T: Smtlib2Theory>(ident: &String, params: &Vec<Ident>, sort: &Sort<T::SortSymbol>) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec!(
                util::make_str_atom("define-sort"),
                util::make_str_atom(ident.as_str()),
                Sexp::List(params.iter().map(|ident| util::make_str_atom(ident.as_str())).collect()),
                sexp_of_sort::<T>(sort)?)))
}

fn sexp_of_assert<T: Smtlib2Theory, B:Smtlib2Binder>(expr: &Expr<T, B>) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec!(
            util::make_str_atom("assert"),
            sexp_of_expr(expr)?)))
}

fn sexp_of_declare_const<T: Smtlib2Theory>((ref ident, ref sort): &SortedSymbol<T::SortSymbol>) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec!(
            util::make_str_atom("declare-sort"),
            util::make_str_atom(ident.as_str()),
            sexp_of_sort::<T>(sort)?)))
}

fn sexp_of_datatype_dec<T: Smtlib2Theory>(datatype: &DatatypeDec<T::SortSymbol>) -> Result<Sexp, PrintError> {
    let mut result = vec!();

    let ctors: Result<Vec<Sexp>, PrintError> = datatype.ctors.iter().map(|(ident, params)| {
        let params: Result<Vec<Sexp>, PrintError> = params.iter().map(|(ident, sort)| {
            let sort = sexp_of_sort::<T>(sort)?;
            Ok(Sexp::List(vec!(
                    util::make_str_atom(ident.as_str()),
                    sort)))
        }).collect();
        let params: Vec<Sexp> = params?;
        Ok(Sexp::List(vec!(
            util::make_str_atom(ident.as_str()),
            Sexp::List(params))))
    }).collect();
    let ctors = ctors?;

    if datatype.param.is_empty() {
        result.push(util::make_str_atom("par"));
        let params = datatype.param.iter().map(|ident| util::make_str_atom(ident.as_str())).collect();
        result.push(Sexp::List(params));
        result.push(Sexp::List(ctors));
        Ok(Sexp::List(result))
    } else {
        Ok(Sexp::List(ctors))
    }
}

fn sexp_of_declare_datatype<T: Smtlib2Theory>(ident: &String, datatype_dec: &DatatypeDec<T::SortSymbol>) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec!(
            util::make_str_atom("declare-datatype"),
            util::make_str_atom(ident.as_str()),
            sexp_of_datatype_dec::<T>(datatype_dec)?)))
}

fn sexp_of_declare_datatypes<SS: IsSortSymbol>(_hoge: &Vec<(Ident, i64, DatatypeDec<SS>)>) -> Result<Sexp, PrintError> {
    unimplemented!()
}

fn sexp_of_check_sat() -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec!(util::make_str_atom("check-sat"))))
}

fn sexp_of_check_sat_assuming() -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec!(util::make_str_atom("check-sat-assuming"))))
}

fn sexp_of_echo(msg: &String) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec!(
                util::make_str_atom("echo"),
                util::make_str_atom(msg.as_str()))))
}

fn sexp_of_exit() -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec!(util::make_str_atom("exit"))))
}

pub fn toplevels<T, B>(smtlib2: &Smtlib2<T, B>) -> Result<Vec<Sexp>, PrintError>
    where T: Smtlib2Theory, B: Smtlib2Binder
{
    use Command::*;
    let result: Result<Vec<_>, _> = smtlib2.commands.iter()
        .map(|command| match command {
            Assert(expr) => sexp_of_assert(&expr),
            CheckSat => sexp_of_check_sat(),
            CheckSatAssuming => sexp_of_check_sat_assuming(),
            DeclareConst(sorted_symbol) => sexp_of_declare_const::<T>(sorted_symbol),
            DeclareDatatype(ident, datatype) => sexp_of_declare_datatype::<T>(ident, datatype),
            DeclareDatatypes(datatypes) => sexp_of_declare_datatypes(datatypes),
            DeclareFun(dec_fun) => sexp_of_fundec(&dec_fun),
            DeclareSort(ident, n) => sexp_of_declare_sort(ident, *n),
            DefineFun(def_fun) => sexp_of_fundef(&def_fun),
            DefineFunRec(def_fun) => sexp_of_fundef_rec(&def_fun),
            DefineFunsRec(def_funs) => sexp_of_funsdef_rec(&def_funs),
            DefineSort(ident, param, sort) => sexp_of_define_sort::<T>(ident, param, sort),

            Echo(msg) => sexp_of_echo(msg),
            Exit => sexp_of_exit(),
        })
        .collect();
    result
}

