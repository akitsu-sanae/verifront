use std::error::Error;
use std::fmt;
use sexp::Sexp;

use crate::logic::{expr::*, theory::*, binder::*};
use super::{Smtlib2Theory, Smtlib2Binder};

#[derive(Debug)]
pub struct ParseError {
    msg: String, // FIXME
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "parser error: {}", self.msg)
    }
}

impl Error for ParseError {
    fn description(&self) -> &str { "parse error" }
}

impl ParseError {
    pub fn  new(msg: String) -> Self {
        Self {
            msg: msg
        }
    }
}

fn expr_of_sexp<T: Theory, B: IsBinder>(_sexp: &Sexp) -> Expr<T, B> {
    unimplemented!()
}

pub fn toplevels<T, B>(toplevels: &Vec<Sexp>) -> Result<Expr<T, B>, ParseError>
    where T: Smtlib2Theory, B: Smtlib2Binder
{
    let mut exprs = vec!();
    for toplevel in toplevels {
        if let Sexp::List(toplevel) = toplevel {
            if let Some(Sexp::Atom(sexp::Atom::S(first))) = toplevel.get(0) {
                match first.as_str() {
                    "set-logic" | "set-info" | "check-sat" | "exit" => (),
                    "declare-fun" => unimplemented!(),
                    "assert" => {
                        if let Some(second) = toplevel.get(1) {
                            exprs.push(expr_of_sexp(second));
                        } else {
                            return Err(ParseError::new(format!("invalid toplevel {:?}", toplevel)))
                        }
                    },
                    _ => {
                        return Err(ParseError::new(format!("invalid toplevel {:?}", toplevel)))
                    }
                }
            } else {
                return Err(ParseError::new(format!("invalid toplevel {:?}", toplevel)))
            }
        } else {
            return Err(ParseError::new(format!("invalid toplevel {:?}", toplevel)))
        }
    }
    Ok(Expr::and_of(exprs))
}

