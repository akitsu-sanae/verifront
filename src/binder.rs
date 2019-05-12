use crate::format::smtlib2::{Smtlib2Binder, PrintError, ParseError};
use sexp::Sexp;

pub trait IsBinder {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmptyBinder {}

impl IsBinder for EmptyBinder {}

impl Smtlib2Binder for EmptyBinder {
    fn sexp_of_binder(&self) -> Result<Sexp, PrintError> {
        Err(PrintError::new(format!("empty binder has no binders to print")))
    }
    fn binder_of_sexp(_expr: &Sexp) -> Result<Self, ParseError> {
        Err(ParseError::new(format!("empty binder has no binders to parse")))
    }
}

/// Quantifier Symbols
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Quantifier {
    Forall, Exists
}

impl IsBinder for Quantifier {}


