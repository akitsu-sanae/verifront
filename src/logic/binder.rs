use crate::format::smtlib2::{Smtlib2Binder, PrintError, ParseError};
use crate::util;
use sexp::{Sexp, Atom};

use std::fmt::Debug;
pub trait IsBinder : Debug + PartialEq + Eq {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmptyBinder {}

impl IsBinder for EmptyBinder {}

impl Smtlib2Binder for EmptyBinder {
    fn sexp_of_binder(&self) -> Result<Sexp, PrintError> {
        unreachable!()
    }
    fn binder_of_sexp(_expr: &Sexp) -> Result<Self, ParseError> {
        unreachable!()
    }
}

/// Quantifier Symbols
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Quantifier {
    Forall, Exists
}

impl IsBinder for Quantifier {}

impl Smtlib2Binder for Quantifier {
    fn sexp_of_binder(&self) -> Result<Sexp, PrintError> {
        use Quantifier::*;
        match self {
            Forall => Ok(util::make_str_atom("forall")),
            Exists => Ok(util::make_str_atom("exists")),
        }
    }
    fn binder_of_sexp(expr: &Sexp) -> Result<Self, ParseError> {
        if let Sexp::Atom(Atom::S(str)) = expr {
            match str.as_str() {
                "forall" => Ok(Quantifier::Forall),
                "exist" => Ok(Quantifier::Exists),
                str => Err(ParseError::new(format!("unknown binder : {}", str)))
            }
        } else {
            Err(ParseError::new(format!("invalid sexp as binder : {}", expr)))
        }
    }
}


