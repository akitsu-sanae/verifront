pub mod boolean;
pub mod integer;

use std::fmt::Debug;

use crate::ident::Ident;

pub trait IsSortSymbol: From<boolean::SortSymbol> + Eq + Debug + Clone {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sort<T: Theory> {
    Symbol(T::SortSymbol),
    Var(Ident),
}

pub trait IsFunctionSymbol<T: Theory>: From<boolean::FunctionSymbol> + Eq + Debug + Clone {
    fn arg_sorts(&self) -> Vec<Sort<T>>;
    fn ret_sort(&self) -> Sort<T>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Function<T: Theory> {
    Symbol(T::FunctionSymbol),
    Var(Ident),
}

pub trait IsConstSymbol<T: Theory>: From<boolean::ConstSymbol> + Eq + Debug + Clone {
    fn sort(&self) -> Sort<T>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const<T: Theory> {
    Symbol(T::ConstSymbol),
    Var(Ident), // term variable
}

pub type SortedSymbol<T> = (Ident, Sort<T>);

pub trait Theory: Eq + Debug + Clone + Sized {
    type SortSymbol: IsSortSymbol;
    type FunctionSymbol: IsFunctionSymbol<Self>;
    type ConstSymbol: IsConstSymbol<Self>;
}
