pub mod core;
use super::logic::{Sort};

pub enum FunAttr {
    LeftAssoc,
    RightAssoc,
    Chainable,
    Pairwise,
}

pub trait Theory {
    type SortSymbol;
    type FunctionSymbol;
    type Info: std::fmt::Debug + Default;

    fn args_sorts_of(func: &Self::FunctionSymbol) -> Vec<Sort<Self::SortSymbol> >;
    fn ret_sort_of(func: &Self::FunctionSymbol) -> Sort<Self::SortSymbol>;
    fn attr_of(func: &Self::FunctionSymbol) -> Option<FunAttr>;
    fn info() -> Self::Info;
}

