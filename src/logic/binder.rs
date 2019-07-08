use crate::format::smtlib2::{ParseError, PrintError, Smtlib2Binder};
use crate::util;
use sexp::{Atom, Sexp};

use std::fmt::Debug;
pub trait IsBinder: Debug + Eq {}

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
    Forall,
    Exists,
}

impl Quantifier {
    pub fn flip(self) -> Self {
        use Quantifier::*;
        match self {
            Forall => Exists,
            Exists => Forall,
        }
    }
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
                "exists" => Ok(Quantifier::Exists),
                str => Err(ParseError::UnknownBinder(str.to_string())),
            }
        } else {
            Err(ParseError::InvalidSexp("binder", expr.clone()))
        }
    }
}
