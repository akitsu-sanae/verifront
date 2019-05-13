use sexp::Sexp;
use crate::logic::*;
use crate::theory::*;
use crate::binder::*;
use super::Format;

mod print;
pub use print::PrintError;

mod parse;
pub use parse::ParseError;

pub trait Smtlib2Theory : Theory {
    fn sexp_of_sort_symbol(ss: &Self::SortSymbol) -> Result<Sexp, PrintError>;
    fn sexp_of_function_symbol(fs: &Self::FunctionSymbol) -> Result<Sexp, PrintError>;
    fn sexp_of_const(c: &Self::Const) -> Result<Sexp, PrintError>;

    fn sort_symbol_of_sexp(expr: &Sexp) -> Result<Self::SortSymbol, ParseError>;
    fn function_symbol_of_sexp(expr: &Sexp) -> Result<Self::FunctionSymbol, ParseError>;
    fn const_of_sexp(expr: &Sexp) -> Result<Self::Const, ParseError>;
}

pub trait Smtlib2Binder : IsBinder + Sized {
    fn sexp_of_binder(&self) -> Result<Sexp, PrintError>;
    fn binder_of_sexp(expr: &Sexp) -> Result<Self, ParseError>;

}

use std::marker::PhantomData;
pub struct Smtlib2<E> {
    phantom_data: PhantomData<fn () -> E>,
}

impl<T, B> Format<Expr<T, B>, Vec<Sexp>> for Smtlib2<Expr<T,B>>
    where T: Smtlib2Theory, B: Smtlib2Binder
{
    type PrintError = PrintError;
    type ParseError = ParseError;

    fn print(expr: &Expr<T, B>) -> Result<Vec<Sexp>, PrintError> {
        print::toplevels(expr)
    }

    fn parse(toplevels: &Vec<Sexp>) -> Result<Expr<T, B>, ParseError> {
        parse::toplevels(toplevels)
    }
}
