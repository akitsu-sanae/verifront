use super::theory::Theory;

/// Quantifier Symbols
#[derive(Debug)]
pub enum Quantifier {
    Forall, Exists
}

pub type Ident = String;

pub fn make_ident(s: &str) -> Ident {
    s.to_string()
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

pub type Propos<T: Theory> = Expr<T, (), ()>;
pub type DefaultPropos = Propos<super::theory::core::Core>;
pub type FOL<T: Theory> = Expr<T, Quantifier, ()>;
pub type DefaultFOL = FOL<super::theory::core::Core>;

