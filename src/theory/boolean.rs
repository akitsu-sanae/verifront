use crate::ident;
use crate::util;
use crate::logic::*;
use crate::theory::*;
use crate::format::smtlib2::Smtlib2Theory;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortSymbol {
    Bool,
}

impl IsSortSymbol for SortSymbol {}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionSymbol {
    Not,
    And, Or, Imply, Equal, IfThenElse,
}

impl IsFunctionSymbol<SortSymbol> for FunctionSymbol {
    fn arg_sorts(&self) -> Vec<Sort<SortSymbol>> {
        use SortSymbol::*;
        use FunctionSymbol::*;
        match self {
            Not => vec!(Sort::Symbol(Bool)),
            And | Or | Imply => vec!(
                Sort::Symbol(Bool),
                Sort::Symbol(Bool)),
            Equal | IfThenElse => vec!(
                Sort::Var(ident::make("A")),
                Sort::Var(ident::make("A"))),
        }
    }

    fn ret_sort(&self) -> Sort<SortSymbol> {
        use SortSymbol::*;
        use FunctionSymbol::*;
        match self {
            Not | And | Or | Imply => Sort::Symbol(Bool),
            Equal | IfThenElse => Sort::Var(ident::make("A")),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Const {
    True, False
}

impl IsConst<SortSymbol> for Const {
    fn sort(&self) -> Sort<SortSymbol> {
        use Const::*;
        use SortSymbol::*;
        match self {
            True | False => Sort::Symbol(Bool),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Boolean {}

impl Theory for Boolean {
    type SortSymbol = SortSymbol;
    type FunctionSymbol = FunctionSymbol;
    type Const = Const;
}


use sexp::{Sexp, Atom};
use crate::format::smtlib2::{PrintError, ParseError};
impl Smtlib2Theory for Boolean {
    fn sexp_of_sort_symbol(ss: &SortSymbol) -> Result<Sexp, PrintError> {
        match ss {
            SortSymbol::Bool => Ok(util::make_atom("Bool")),
        }
    }

    fn sexp_of_function_symbol(fs: &FunctionSymbol) -> Result<Sexp, PrintError> {
        use FunctionSymbol::*;
        Ok(match fs {
            Not => util::make_atom("not"),
            And => util::make_atom("and"),
            Or => util::make_atom("or"),
            Imply => util::make_atom("=>"),
            Equal => util::make_atom("="),
            IfThenElse => util::make_atom("ite"),
        })
    }

    fn sexp_of_const(c: &Const) -> Result<Sexp, PrintError> {
        Ok(match c {
            Const::True => util::make_atom("true"),
            Const::False => util::make_atom("false"),
        })
    }

    fn sort_symbol_of_sexp(expr: &Sexp) -> Result<SortSymbol, ParseError> {
        if let Sexp::Atom(Atom::S(str)) = expr {
            match str.as_str() {
                "Bool" => Ok(SortSymbol::Bool),
                s => Err(ParseError::new(format!("unknown sort symbol : {}", s)))
            }
        } else {
            Err(ParseError::new(format!("invalid sexp as sort symbol : {}", expr)))
        }
    }

    fn function_symbol_of_sexp(expr: &Sexp) -> Result<FunctionSymbol, ParseError> {
        use FunctionSymbol::*;
        if let Sexp::Atom(Atom::S(str)) = expr {
            match str.as_str() {
                "not" => Ok(Not),
                "and" => Ok(And),
                "or" => Ok(Or),
                "=>" => Ok(Imply),
                "=" => Ok(Equal),
                "ite" => Ok(IfThenElse),
                str => Err(ParseError::new(format!("unknown function symbol: {}", str)))
            }
        } else {
            Err(ParseError::new(format!("invalid sexp as function symbol : {}", expr)))
        }
    }

    fn const_of_sexp(expr: &Sexp) -> Result<Const, ParseError> {
        if expr == &util::make_atom("true") {
            Ok(Const::True)
        } else if expr == &util::make_atom("false") {
            Ok(Const::False)
        } else {
            Err(ParseError::new(format!("unknown const: {}", expr)))
        }
    }
}

