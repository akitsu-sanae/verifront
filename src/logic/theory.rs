pub mod boolean;
pub mod integer;

use std::fmt::Debug;
use std::marker::PhantomData;

use crate::ident::Ident;

pub trait IsSortSymbol : From<boolean::SortSymbol> + PartialEq + Eq + Debug {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sort<S: IsSortSymbol> {
    Symbol (S),
    Var (Ident),
}

pub trait IsFunctionSymbol<S: IsSortSymbol> : From<boolean::FunctionSymbol> + PartialEq + Eq + Debug {
    fn arg_sorts(&self) -> Vec<Sort<S>>;
    fn ret_sort(&self) -> Sort<S>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Function<S: IsSortSymbol, F: IsFunctionSymbol<S>> {
    Symbol (F),
    Var (Ident),
    _Phantom (PhantomData<Fn () -> S>),

}

pub trait IsConstSymbol<S: IsSortSymbol> : From<boolean::ConstSymbol> + PartialEq + Eq + Debug {
    fn sort(&self) -> Sort<S>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const<S: IsSortSymbol, C: IsConstSymbol<S>> {
    Symbol (C),
    Var (Ident), // term variable
    _Phantom (PhantomData<Fn () -> S>),
}

pub trait Theory : Debug + PartialEq + Eq {
    type SortSymbol : IsSortSymbol;
    type FunctionSymbol : IsFunctionSymbol<Self::SortSymbol>;
    type ConstSymbol : IsConstSymbol<Self::SortSymbol>;
}

