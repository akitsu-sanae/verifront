use sexp::Sexp;
use crate::ident::Ident;
use crate::logic::{expr::*, theory::*, binder::*};
use super::Format;

mod print;
pub use print::PrintError;

mod parse;
pub use parse::ParseError;

pub trait Smtlib2Theory : Theory {
    fn sexp_of_sort_symbol(ss: &Self::SortSymbol) -> Result<Sexp, PrintError>;
    fn sexp_of_function_symbol(fs: &Self::FunctionSymbol) -> Result<Sexp, PrintError>;
    fn sexp_of_const_symbol(c: &Self::ConstSymbol) -> Result<Sexp, PrintError>;

    fn sort_symbol_of_sexp(expr: &Sexp) -> Result<Self::SortSymbol, ParseError>;
    fn function_symbol_of_sexp(expr: &Sexp) -> Result<Self::FunctionSymbol, ParseError>;
    fn const_symbol_of_sexp(expr: &Sexp) -> Result<Self::ConstSymbol, ParseError>;
}

pub trait Smtlib2Binder : IsBinder + Sized {
    fn sexp_of_binder(&self) -> Result<Sexp, PrintError>;
    fn binder_of_sexp(expr: &Sexp) -> Result<Self, ParseError>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunDec<T: Smtlib2Theory> {
    name: Ident,
    params: Vec<Sort<T::SortSymbol>>,
    ret: Sort<T::SortSymbol>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunDef<T: Smtlib2Theory, B: Smtlib2Binder> {
    name: Ident,
    params: Vec<(Ident, Sort<T::SortSymbol>)>,
    ret: Sort<T::SortSymbol>,
    body: Expr<T, B>,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command<T: Smtlib2Theory, B: Smtlib2Binder> {
    DeclareFun(FunDec<T>),
    DefineFun(FunDef<T, B>),
    // DefineFunRec(FunDef),
    // DefineFunsRec(Vec<FunDef>),
    Assert(Expr<T, B>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Smtlib2<T: Smtlib2Theory, B: Smtlib2Binder> {
    pub commands: Vec<Command<T, B>>,
}

impl<T, B> Format<Vec<Sexp>> for Smtlib2<T,B>
    where T: Smtlib2Theory, B: Smtlib2Binder
{
    type PrintError = PrintError;
    type ParseError = ParseError;

    fn print(expr: &Smtlib2<T, B>) -> Result<Vec<Sexp>, PrintError> {
        print::toplevels(expr)
    }

    fn parse(toplevels: &Vec<Sexp>) -> Result<Smtlib2<T, B>, ParseError> {
        parse::toplevels(toplevels)
    }
}
