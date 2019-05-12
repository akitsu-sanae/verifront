use std::error::Error;
use std::fmt;
use sexp::Sexp;

use super::Format;
use crate::logic::*;
use crate::theory::*;

use std::marker::PhantomData;
struct Smtlib2<T: Theory, B: IsBinder> {
    phantom_theory: PhantomData<fn () -> T>,
    phantom_binder: PhantomData<fn () -> B>,
}

#[derive(Debug)]
struct PrintError {
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

#[derive(Debug)]
struct ParseError {
    msg: String, // FIXME
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "parser error: {}", self.msg)
    }
}

impl ParseError {
    fn  new(msg: String) -> Self {
        Self {
            msg: msg
        }
    }
}

impl Error for ParseError {
    fn description(&self) -> &str { "parse error" }
}

fn parse_formula<T: Theory, B: IsBinder>(_sexp: &Sexp) -> Expr<T, B> {
    unimplemented!()
}

impl<T, B> Format<Expr<T, B>, Vec<Sexp>> for Smtlib2<T, B>
    where T: Theory, B: IsBinder
{
    type PrintError = PrintError;
    type ParseError = ParseError;

    fn print(_expr: &Expr<T, B>) -> Result<Vec<Sexp>, PrintError> {
        unimplemented!() // TODO
    }

    fn parse(toplevels: &Vec<Sexp>) -> Result<Expr<T, B>, ParseError> {
        let mut exprs = vec!();
        for toplevel in toplevels {
            if let Sexp::List(toplevel) = toplevel {
                if let Some(Sexp::Atom(sexp::Atom::S(first))) = toplevel.get(0) {
                    match first.as_str() {
                        "set-logic" | "set-info" | "check-sat" | "exit" => (),
                        "declare-fun" => unimplemented!(),
                        "assert" => {
                            if let Some(second) = toplevel.get(1) {
                                exprs.push(parse_formula(second));
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
}



