use super::theory::Theory;
use super::ident::Ident;

/// Quantifier Symbols
#[derive(Debug)]
pub enum Quantifier {
    Forall, Exists
}

#[derive(Debug)]
pub enum Sort<S> { // S : Sort Symbols
    Symbol (S),
    Var (Ident),
}


#[derive(Debug)]
pub enum Expr<T: Theory, Binder, Extension> {
    Binding(Binder, Vec<Ident>, Box<Expr<T, Binder, Extension>>),
    Apply (T::FunctionSymbol, Vec<Expr<T, Binder, Extension>>),
    Var(Ident),
    Extension (Extension),
}

pub type Propos<T> = Expr<T, (), ()>;
pub type DefaultPropos = Propos<super::theory::core::Core>;
pub type FOL<T> = Expr<T, Quantifier, ()>;
pub type DefaultFOL = FOL<super::theory::core::Core>;

