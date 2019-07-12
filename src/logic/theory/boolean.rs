use crate::format::smtlib2::Smtlib2Theory;
use crate::logic::{symbol, theory::*};
use crate::util;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortSymbol {
    Bool,
}

impl IsSortSymbol for SortSymbol {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionSymbol {
    Not,
    And,
    Or,
    Imply,
    Equal,
    IfThenElse,
}

impl IsFunctionSymbol<Boolean> for FunctionSymbol {
    fn arg_sorts(&self) -> Vec<Sort<Boolean>> {
        use FunctionSymbol::*;
        use SortSymbol::*;
        match self {
            Not => vec![Sort::Symbol(Bool)],
            And | Or | Imply => vec![Sort::Symbol(Bool), Sort::Symbol(Bool)],
            Equal | IfThenElse => vec![Sort::Var(symbol::make("A")), Sort::Var(symbol::make("A"))],
        }
    }

    fn ret_sort(&self) -> Sort<Boolean> {
        use FunctionSymbol::*;
        use SortSymbol::*;
        match self {
            Not | And | Or | Imply => Sort::Symbol(Bool),
            Equal | IfThenElse => Sort::Var(symbol::make("A")),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstSymbol {
    True,
    False,
}

impl IsConstSymbol<Boolean> for ConstSymbol {
    fn sort(&self) -> Sort<Boolean> {
        use ConstSymbol::*;
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
    type ConstSymbol = ConstSymbol;
}

use crate::format::smtlib2::{ParseError, PrintError};
use sexp::{Atom, Sexp};
impl Smtlib2Theory for Boolean {
    fn sexp_of_sort_symbol(ss: &SortSymbol) -> Result<Sexp, PrintError> {
        match ss {
            SortSymbol::Bool => Ok(util::make_str_atom("Bool")),
        }
    }

    fn sexp_of_function_symbol(fs: &FunctionSymbol) -> Result<Sexp, PrintError> {
        use FunctionSymbol::*;
        Ok(match fs {
            Not => util::make_str_atom("not"),
            And => util::make_str_atom("and"),
            Or => util::make_str_atom("or"),
            Imply => util::make_str_atom("=>"),
            Equal => util::make_str_atom("="),
            IfThenElse => util::make_str_atom("ite"),
        })
    }

    fn sexp_of_const_symbol(c: &ConstSymbol) -> Result<Sexp, PrintError> {
        Ok(match c {
            ConstSymbol::True => util::make_str_atom("true"),
            ConstSymbol::False => util::make_str_atom("false"),
        })
    }

    fn sort_symbol_of_sexp(term: &Sexp) -> Result<SortSymbol, ParseError> {
        if let Sexp::Atom(Atom::S(str)) = term {
            match str.as_str() {
                "Bool" => Ok(SortSymbol::Bool),
                s => Err(ParseError::UnknownSortSymbol(s.to_string())),
            }
        } else {
            Err(ParseError::InvalidSexp("sort symbol", term.clone()))
        }
    }

    fn function_symbol_of_sexp(term: &Sexp) -> Result<FunctionSymbol, ParseError> {
        use FunctionSymbol::*;
        if let Sexp::Atom(Atom::S(str)) = term {
            match str.as_str() {
                "not" => Ok(Not),
                "and" => Ok(And),
                "or" => Ok(Or),
                "=>" => Ok(Imply),
                "=" => Ok(Equal),
                "ite" => Ok(IfThenElse),
                str => Err(ParseError::UnknownFunctionSymbol(str.to_string())),
            }
        } else {
            Err(ParseError::InvalidSexp("function symbol", term.clone()))
        }
    }

    fn const_symbol_of_sexp(term: &Sexp) -> Result<ConstSymbol, ParseError> {
        if term == &util::make_str_atom("true") {
            Ok(ConstSymbol::True)
        } else if term == &util::make_str_atom("false") {
            Ok(ConstSymbol::False)
        } else {
            Err(ParseError::InvalidSexp("const symbol", term.clone()))
        }
    }
}
