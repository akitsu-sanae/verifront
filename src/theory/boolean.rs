use crate::ident;
use crate::logic::*;
use crate::theory::*;

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

