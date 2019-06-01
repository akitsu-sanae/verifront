use sexp::Sexp;
use std::error::Error;
use std::fmt;

use crate::ident::Ident;
use crate::util;

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
    fn description(&self) -> &str {
        "print error"
    }
}

impl PrintError {
    pub fn new(msg: String) -> Self {
        Self { msg: msg }
    }
}

fn sexp_of_sort<T: Smtlib2Theory>(sort: &Sort<T::SortSymbol>) -> Result<Sexp, PrintError> {
    match sort {
        Sort::Var(ident) => Ok(Sexp::Atom(sexp::Atom::S(ident.clone()))),
        Sort::Symbol(sym) => T::sexp_of_sort_symbol(sym),
    }
}

fn sexp_of_function<T: Smtlib2Theory>(
    fun: &Function<T::SortSymbol, T::FunctionSymbol>,
) -> Result<Sexp, PrintError> {
    match fun {
        Function::Var(ident) => Ok(Sexp::Atom(sexp::Atom::S(ident.clone()))),
        Function::Symbol(sym) => T::sexp_of_function_symbol(sym),
        Function::_Phantom(_) => unreachable!(),
    }
}

fn sexp_of_params<T: Smtlib2Theory>(
    params: &Vec<SortedSymbol<T::SortSymbol>>,
) -> Result<Sexp, PrintError> {
    let params: Result<Vec<_>, _> = params
        .iter()
        .map(|(ident, sort)| {
            let ident = Sexp::Atom(sexp::Atom::S(ident.clone()));
            let sort = sexp_of_sort::<T>(sort)?;
            Ok(Sexp::List(vec![ident, sort]))
        })
        .collect();
    let params = params?;
    Ok(Sexp::List(params))
}

fn sexp_of_expr<T, B>(expr: &Expr<T, B>) -> Result<Sexp, PrintError>
where
    T: Smtlib2Theory,
    B: Smtlib2Binder,
{
    use Expr::*;
    match expr {
        Binding(binder, params, box phi) => Ok(Sexp::List(vec![
            B::sexp_of_binder(binder)?,
            sexp_of_params::<T>(params)?,
            sexp_of_expr(phi)?,
        ])),
        Apply(fun, args) => {
            let args: Result<Vec<_>, _> = args.into_iter().map(|arg| sexp_of_expr(arg)).collect();
            let mut args = args?;
            let mut exprs = vec![sexp_of_function::<T>(fun)?];
            exprs.append(&mut args);
            Ok(Sexp::List(exprs))
        }
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
    Ok(Sexp::List(vec![
        util::make_str_atom("declare-fun"),
        util::make_str_atom(dec_fun.name.as_str()),
        Sexp::List({
            let params: Result<Vec<_>, _> = dec_fun
                .params
                .iter()
                .map(|param| sexp_of_sort::<T>(param))
                .collect();
            params?
        }),
        sexp_of_sort::<T>(&dec_fun.ret)?,
    ]))
}

fn sexp_of_declare_sort(ident: &String, n: i64) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec![
        util::make_str_atom("declare-sort"),
        util::make_str_atom(ident.as_str()),
        util::make_int_atom(n),
    ]))
}

fn sexp_of_fundef<T: Smtlib2Theory, B: Smtlib2Binder>(
    def_fun: &FunDef<T, B>,
) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec![
        util::make_str_atom("define-fun"),
        util::make_str_atom(def_fun.name.as_str()),
        sexp_of_params::<T>(&def_fun.params)?,
        sexp_of_sort::<T>(&def_fun.ret)?,
        sexp_of_expr(&def_fun.body)?,
    ]))
}

fn sexp_of_fundef_rec<T: Smtlib2Theory, B: Smtlib2Binder>(
    def_fun: &FunDef<T, B>,
) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec![
        util::make_str_atom("define-fun-rec"),
        util::make_str_atom(def_fun.name.as_str()),
        sexp_of_params::<T>(&def_fun.params)?,
        sexp_of_sort::<T>(&def_fun.ret)?,
        sexp_of_expr(&def_fun.body)?,
    ]))
}

fn sexp_of_funsdef_rec<T: Smtlib2Theory, B: Smtlib2Binder>(
    def_funs: &Vec<FunDef<T, B>>,
) -> Result<Sexp, PrintError> {
    let mut fun_decs = vec![];
    let mut terms = vec![];
    for def_fun in def_funs.iter() {
        fun_decs.push(Sexp::List(vec![
            util::make_str_atom(def_fun.name.as_str()),
            sexp_of_params::<T>(&def_fun.params)?,
            sexp_of_sort::<T>(&def_fun.ret)?,
        ]));
        terms.push(sexp_of_expr::<T, B>(&def_fun.body)?);
    }
    Ok(Sexp::List(vec![
        util::make_str_atom("define-funs-rec"),
        Sexp::List(fun_decs),
        Sexp::List(terms),
    ]))
}

fn sexp_of_define_sort<T: Smtlib2Theory>(
    ident: &String,
    params: &Vec<Ident>,
    sort: &Sort<T::SortSymbol>,
) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec![
        util::make_str_atom("define-sort"),
        util::make_str_atom(ident.as_str()),
        Sexp::List(
            params
                .iter()
                .map(|ident| util::make_str_atom(ident.as_str()))
                .collect(),
        ),
        sexp_of_sort::<T>(sort)?,
    ]))
}

fn sexp_of_assert<T: Smtlib2Theory, B: Smtlib2Binder>(
    expr: &Expr<T, B>,
) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec![
        util::make_str_atom("assert"),
        sexp_of_expr(expr)?,
    ]))
}

fn sexp_of_declare_const<T: Smtlib2Theory>(
    (ref ident, ref sort): &SortedSymbol<T::SortSymbol>,
) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec![
        util::make_str_atom("declare-const"),
        util::make_str_atom(ident.as_str()),
        sexp_of_sort::<T>(sort)?,
    ]))
}

fn sexp_of_selector_dec<T: Smtlib2Theory>(
    selector_dec: &SelectorDec<T::SortSymbol>,
) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec![
        util::make_str_atom(&selector_dec.name),
        sexp_of_sort::<T>(&selector_dec.sort)?,
    ]))
}

fn sexp_of_constructor_dec<T: Smtlib2Theory>(
    constructor_dec: &ConstructorDec<T::SortSymbol>,
) -> Result<Sexp, PrintError> {
    let mut result = vec![util::make_str_atom(constructor_dec.name.as_str())];
    for selector_dec in constructor_dec.selector_decs.iter() {
        result.push(sexp_of_selector_dec::<T>(selector_dec)?);
    }
    Ok(Sexp::List(result))
}

fn sexp_of_datatype_dec<T: Smtlib2Theory>(
    datatype_dec: &DatatypeDec<T::SortSymbol>,
) -> Result<Sexp, PrintError> {
    let ctors: Result<Vec<Sexp>, _> = datatype_dec
        .constructor_decs
        .iter()
        .map(|ctor_dec| sexp_of_constructor_dec::<T>(ctor_dec))
        .collect();
    let ctors = ctors?;

    if datatype_dec.params.is_empty() {
        Ok(Sexp::List(ctors))
    } else {
        let params: Vec<Sexp> = datatype_dec
            .params
            .iter()
            .map(|param| util::make_str_atom(param.as_str()))
            .collect();
        let params = Sexp::List(vec![util::make_str_atom("par"), Sexp::List(params)]);
        let datatype = vec![params, Sexp::List(ctors)];
        Ok(Sexp::List(datatype))
    }
}

fn sexp_of_declare_datatype<T: Smtlib2Theory>(
    datatype_dec: &DatatypeDec<T::SortSymbol>,
) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec![
        util::make_str_atom("declare-datatype"),
        util::make_str_atom(datatype_dec.name.as_str()),
        sexp_of_datatype_dec::<T>(datatype_dec)?,
    ]))
}

fn sexp_of_declare_datatypes<T: Smtlib2Theory>(
    sort_decs: &Vec<Ident>,
    datatype_decs: &Vec<DatatypeDec<T::SortSymbol>>,
) -> Result<Sexp, PrintError> {
    let mut sorts_sexp = vec![];
    let mut datatypes_sexp = vec![];

    for ident in sort_decs.iter() {
        sorts_sexp.push(util::make_str_atom(ident.as_str()));
    }
    for datatype_dec in datatype_decs.iter() {
        let mut inner = vec![util::make_str_atom(datatype_dec.name.as_str())];
        if let Sexp::List(mut datatype) = sexp_of_datatype_dec::<T>(&datatype_dec)? {
            inner.append(&mut datatype);
        } else {
            unreachable!();
        }
        datatypes_sexp.push(Sexp::List(inner));
    }

    Ok(Sexp::List(vec![
        util::make_str_atom("declare-datatypes"),
        Sexp::List(sorts_sexp),
        Sexp::List(datatypes_sexp),
    ]))
}

fn sexp_of_check_sat() -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec![util::make_str_atom("check-sat")]))
}

fn sexp_of_check_sat_assuming(poss: &Vec<Ident>, negs: &Vec<Ident>) -> Result<Sexp, PrintError> {
    let mut propos = vec![];
    for pos in poss.iter() {
        propos.push(util::make_str_atom(pos.as_str()));
    }
    for neg in negs.iter() {
        propos.push(Sexp::List(vec![
            util::make_str_atom("not"),
            util::make_str_atom(neg.as_str()),
        ]));
    }
    Ok(Sexp::List(vec![
        util::make_str_atom("check-sat-assuming"),
        Sexp::List(propos),
    ]))
}

fn sexp_of_echo(msg: &String) -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec![
        util::make_str_atom("echo"),
        util::make_str_atom(msg.as_str()),
    ]))
}

fn sexp_of_exit() -> Result<Sexp, PrintError> {
    Ok(Sexp::List(vec![util::make_str_atom("exit")]))
}

pub fn toplevels<T, B>(smtlib2: &Smtlib2<T, B>) -> Result<Vec<Sexp>, PrintError>
where
    T: Smtlib2Theory,
    B: Smtlib2Binder,
{
    use Command::*;
    let result: Result<Vec<_>, _> = smtlib2
        .commands
        .iter()
        .map(|command| match command {
            Assert(expr) => sexp_of_assert(&expr),
            CheckSat => sexp_of_check_sat(),
            CheckSatAssuming(pos, neg) => sexp_of_check_sat_assuming(pos, neg),
            DeclareConst(sorted_symbol) => sexp_of_declare_const::<T>(sorted_symbol),
            DeclareDatatype(datatype) => sexp_of_declare_datatype::<T>(datatype),
            DeclareDatatypes(sort_decs, datatype_decs) => {
                sexp_of_declare_datatypes::<T>(sort_decs, datatype_decs)
            }
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
