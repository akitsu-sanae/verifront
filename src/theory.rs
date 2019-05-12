pub mod boolean;
use crate::logic::*;

pub trait IsSortSymbol : From<boolean::SortSymbol> {}

pub trait IsFunctionSymbol<S: IsSortSymbol> : From<boolean::FunctionSymbol> {
    fn arg_sorts(&self) -> Vec<Sort<S>>;
    fn ret_sort(&self) -> Sort<S>;
}

pub trait IsConst<S: IsSortSymbol> : From<boolean::Const> {
    fn sort(&self) -> Sort<S>;
}


pub trait Theory : From<boolean::Boolean> {
    type SortSymbol : IsSortSymbol;
    type FunctionSymbol : IsFunctionSymbol<Self::SortSymbol>;
    type Const : IsConst<Self::SortSymbol>;
}

