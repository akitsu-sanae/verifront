use crate::format::smtlib2::Smtlib2Theory;
use crate::logic::theory::*;
use crate::util;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortSymbol {
    Bool,
    Int,
}

use std::convert::From;
impl From<Sort<boolean::Boolean>> for Sort<Integer> {
    fn from(s: Sort<boolean::Boolean>) -> Sort<Integer> {
        match s {
            Sort::Symbol(ss) => Sort::Symbol(SortSymbol::from(ss)),
            Sort::Var(symbol) => Sort::Var(symbol),
        }
    }
}

impl From<boolean::SortSymbol> for SortSymbol {
    fn from(_bs: boolean::SortSymbol) -> SortSymbol {
        SortSymbol::Bool
    }
}

impl IsSortSymbol for SortSymbol {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionSymbol {
    Boolean(boolean::FunctionSymbol),
    Add,
    Sub,
    Mult,
    Div,
    Lt,
    Gt,
    Leq,
    Geq,
}

impl From<boolean::FunctionSymbol> for FunctionSymbol {
    fn from(bf: boolean::FunctionSymbol) -> FunctionSymbol {
        FunctionSymbol::Boolean(bf)
    }
}

impl IsFunctionSymbol<Integer> for FunctionSymbol {
    fn arg_sorts(&self) -> Vec<Sort<Integer>> {
        use FunctionSymbol::*;
        use SortSymbol::*;
        match self {
            Boolean(bool_ss) => {
                let sorts = boolean::FunctionSymbol::arg_sorts(bool_ss);
                sorts
                    .into_iter()
                    .map(|sort| Sort::<Integer>::from(sort))
                    .collect()
            }
            Add | Sub | Mult | Div | Lt | Gt | Leq | Geq => {
                vec![Sort::Symbol(Int), Sort::Symbol(Int)]
            }
        }
    }

    fn ret_sort(&self) -> Sort<Integer> {
        use FunctionSymbol::*;
        use SortSymbol::*;
        match self {
            Boolean(bool_ss) => match boolean::FunctionSymbol::ret_sort(bool_ss) {
                Sort::Var(symbol) => Sort::Var(symbol),
                Sort::Symbol(boolean::SortSymbol::Bool) => Sort::Symbol(Bool),
            },
            Add | Sub | Mult | Div => Sort::Symbol(Int),
            Lt | Gt | Leq | Geq => Sort::Symbol(Bool),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstSymbol {
    Boolean(boolean::ConstSymbol),
    Number(i64),
}

impl From<boolean::ConstSymbol> for ConstSymbol {
    fn from(bc: boolean::ConstSymbol) -> ConstSymbol {
        ConstSymbol::Boolean(bc)
    }
}

impl IsConstSymbol<Integer> for ConstSymbol {
    fn sort(&self) -> Sort<Integer> {
        use ConstSymbol::*;
        use SortSymbol::*;
        match self {
            Boolean(_) => Sort::Symbol(Bool),
            Number(_) => Sort::Symbol(Int),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Integer {}

impl ::std::convert::From<boolean::Boolean> for Integer {
    fn from(_: boolean::Boolean) -> Integer {
        Integer {}
    }
}

impl Theory for Integer {
    type SortSymbol = SortSymbol;
    type FunctionSymbol = FunctionSymbol;
    type ConstSymbol = ConstSymbol;
}

use crate::format::smtlib2::{ParseError, PrintError};
use sexp::{Atom, Sexp};
impl Smtlib2Theory for Integer {
    fn sexp_of_sort_symbol(ss: &SortSymbol) -> Result<Sexp, PrintError> {
        Ok(match ss {
            SortSymbol::Bool => util::make_str_atom("Bool"),
            SortSymbol::Int => util::make_str_atom("Int"),
        })
    }

    fn sexp_of_function_symbol(fs: &FunctionSymbol) -> Result<Sexp, PrintError> {
        use FunctionSymbol::*;
        match fs {
            Boolean(bfs) => boolean::Boolean::sexp_of_function_symbol(bfs),
            Add => Ok(util::make_str_atom("+")),
            Sub => Ok(util::make_str_atom("-")),
            Mult => Ok(util::make_str_atom("*")),
            Div => Ok(util::make_str_atom("/")),
            Lt => Ok(util::make_str_atom("<")),
            Gt => Ok(util::make_str_atom(">")),
            Leq => Ok(util::make_str_atom("<=")),
            Geq => Ok(util::make_str_atom(">=")),
        }
    }

    fn sexp_of_const_symbol(c: &ConstSymbol) -> Result<Sexp, PrintError> {
        use ConstSymbol::*;
        match c {
            Boolean(bc) => boolean::Boolean::sexp_of_const_symbol(bc),
            Number(n) => Ok(util::make_int_atom(*n)),
        }
    }

    fn sort_symbol_of_sexp(term: &Sexp) -> Result<SortSymbol, ParseError> {
        use SortSymbol::*;
        if let Sexp::Atom(Atom::S(str)) = term {
            match str.as_str() {
                "Bool" => Ok(Bool),
                "Int" => Ok(Int),
                s => Err(ParseError::UnknownSortSymbol(s.to_string())),
            }
        } else {
            Err(ParseError::InvalidSexp("sort symbol", term.clone()))
        }
    }

    fn function_symbol_of_sexp(term: &Sexp) -> Result<FunctionSymbol, ParseError> {
        use FunctionSymbol::*;
        let int_fun = if let Sexp::Atom(Atom::S(str)) = term {
            match str.as_str() {
                "+" => Ok(Add),
                "-" => Ok(Sub),
                "*" => Ok(Mult),
                "/" => Ok(Div),
                "<" => Ok(Lt),
                ">" => Ok(Gt),
                "<=" => Ok(Leq),
                ">=" => Ok(Geq),
                s => Err(ParseError::UnknownFunctionSymbol(s.to_string())),
            }
        } else {
            Err(ParseError::InvalidSexp("function symbol", term.clone()))
        };
        int_fun.or_else(|_| match boolean::Boolean::function_symbol_of_sexp(term) {
            Ok(bf) => Ok(FunctionSymbol::from(bf)),
            Err(err) => Err(err),
        })
    }

    fn const_symbol_of_sexp(term: &Sexp) -> Result<ConstSymbol, ParseError> {
        use ConstSymbol::*;
        if let Sexp::Atom(Atom::I(n)) = term {
            Ok(Number(*n))
        } else {
            match boolean::Boolean::const_symbol_of_sexp(term) {
                Ok(bc) => Ok(Boolean(bc)),
                Err(err) => Err(err),
            }
        }
    }
}
