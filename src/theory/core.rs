
use super::{Theory, FunAttr};
use crate::logic::{Sort, make_ident};

#[derive(Debug)]
pub struct Core {}

#[derive(Debug)]
pub enum SortSymbol {
    Bool,
    Var (String),
}

#[derive(Debug)]
pub enum FunctionSymbol {
    True, False, Not,
    And, Or, Imply, Xor,
    Equal, Distinct, IfThenElse,
}

impl Theory for Core {
    type SortSymbol = self::SortSymbol;
    type FunctionSymbol = self::FunctionSymbol;
    type Info = i32;

    fn args_sorts_of(func: &Self::FunctionSymbol) -> Vec<Sort<Self::SortSymbol> > {
        use FunctionSymbol::*;
        match func {
            True | False => vec![],
            Not => vec![
                Sort::Symbol(SortSymbol::Bool),
            ],
            And | Or | Imply | Xor => vec![
                Sort::Symbol(SortSymbol::Bool),
                Sort::Symbol(SortSymbol::Bool),
            ],
            Equal | Distinct | IfThenElse => vec![
                Sort::Var (make_ident("A")),
                Sort::Var (make_ident("A")),
            ],
        }
    }

    fn ret_sort_of(func: &Self::FunctionSymbol) -> Sort<Self::SortSymbol> {
        use FunctionSymbol::*;
        match func {
            IfThenElse => Sort::Var (make_ident("A")),
            _ => Sort::Symbol(SortSymbol::Bool),
        }
    }

    fn attr_of(func: &Self::FunctionSymbol) -> Option<FunAttr> {
        use FunctionSymbol::*;
        match func {
            True | False | Not | IfThenElse => None,
            Imply => Some(FunAttr::RightAssoc),
            And | Or | Xor => Some(FunAttr::LeftAssoc),
            Equal => Some(FunAttr::Chainable),
            Distinct => Some(FunAttr::Pairwise),
        }
    }

    fn info() -> Self::Info {
        Self::Info::default()
    }
}

