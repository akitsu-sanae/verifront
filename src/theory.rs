pub mod boolean;
pub mod integer;
use crate::logic::*;

use std::fmt::Debug;
pub trait IsSortSymbol : From<boolean::SortSymbol> + PartialEq + Eq + Debug {}

pub trait IsFunctionSymbol<S: IsSortSymbol> : From<boolean::FunctionSymbol> + PartialEq + Eq + Debug {
    fn arg_sorts(&self) -> Vec<Sort<S>>;
    fn ret_sort(&self) -> Sort<S>;
}

pub trait IsConst<S: IsSortSymbol> : From<boolean::Const> + PartialEq + Eq + Debug {
    fn sort(&self) -> Sort<S>;
}


pub trait Theory : From<boolean::Boolean> + PartialEq + Eq + Debug {
    type SortSymbol : IsSortSymbol;
    type FunctionSymbol : IsFunctionSymbol<Self::SortSymbol>;
    type Const : IsConst<Self::SortSymbol>;
}

