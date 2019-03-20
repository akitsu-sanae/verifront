use super::theory::Theory;
use super::ident::Ident;
use super::parse;

/// Quantifier Symbols
#[derive(Debug)]
pub enum Quantifier {
    Forall, Exists
}

#[derive(Debug)]
pub enum Sort<S> { // S : Sort Symbols
    Symbol (S),
    Var (Ident),
}


#[derive(Debug)]
pub enum Expr<T: Theory, Binder, Extension> {
    Binding(Binder, Vec<Ident>, Box<Expr<T, Binder, Extension>>),
    Apply (T::FunctionSymbol, Vec<Expr<T, Binder, Extension>>),
    Var(Ident),
    Extension (Extension),
}

pub type Propos<T> = Expr<T, (), ()>;
pub type DefaultPropos = Propos<super::theory::core::Core>;
pub type FOL<T> = Expr<T, Quantifier, ()>;
pub type DefaultFOL = FOL<super::theory::core::Core>;

impl<T: Theory, Binder, Extension> parse::Parsable for Expr<T, Binder, Extension> {
    fn parse(content: &str) -> Result<Box<Self>, parse::ParseError> {
        use sexp::{Sexp, Atom};
        use parse::{UnexpectedError, ParseError::*};
        let sexp = match sexp::parse(content) {
            Ok (sexp) => sexp,
            Err (err) => return Err(Other(err)),
        };
        // TODO
        match sexp {
            Sexp::Atom(Atom::S(ref ident)) => Ok(box Self::Var(ident.clone())),
            _ => Err(Unexpected(UnexpectedError{
                line: 0,
                column: 0,
                expected: vec!["<identifier>".to_string()],
                found: "dummy".to_string()
            })),
        }
    }
}

