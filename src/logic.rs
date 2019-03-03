
use super::ident;
use super::theory;

/// Unary Operations
#[derive(Debug)]
pub enum UnaryOp {
    Neg
}

/// Binary Operations
#[derive(Debug)]
pub enum BinOp {
    And, Or, Imply, Iff
}

/// Quantifier Symbols
#[derive(Debug)]
pub enum Quantifier {
    Forall, Exists
}

/// Propositional Logic
#[derive(Debug)]
pub enum Propos {
    Atom (ident::Ident),
    UnaryOp (UnaryOp, Box<Propos>),
    BinaryOp (BinOp, Box<Propos>, Box<Propos>)
}

impl Propos {
    pub fn make_atom(s: &str) -> Self {
        Propos::Atom (ident::make(s))
    }
    pub fn make_binop(op: BinOp, phi1: Box<Self>, phi2: Box<Self>) -> Self {
        Propos::BinaryOp(op, phi1, phi2)
    }
    pub fn make_neg(phi: Box<Self>) -> Self {
        Propos::UnaryOp(UnaryOp::Neg, phi)
    }
    pub fn make_and(phi1: Box<Self>, phi2: Box<Self>) -> Self {
        Propos::BinaryOp(BinOp::And, phi1, phi2)
    }
    pub fn make_or(phi1: Box<Self>, phi2: Box<Self>) -> Self {
        Propos::BinaryOp(BinOp::Or, phi1, phi2)
    }
    pub fn make_imply(phi1: Box<Self>, phi2: Box<Self>) -> Self {
        Propos::BinaryOp(BinOp::Imply, phi1, phi2)
    }
    pub fn make_iff(phi1: Box<Self>, phi2: Box<Self>) -> Self {
        Propos::BinaryOp(BinOp::Iff, phi1, phi2)
    }
}

/// First-Order Predicate Logic
#[derive(Debug)]
pub enum FOL<T: theory::Theory> {
    UnaryOp (UnaryOp, Box<FOL<T>>),
    BinaryOp (BinOp, Box<FOL<T>>, Box<FOL<T>>),
    Quantified (Quantifier, Vec<ident::Ident>, Box<FOL<T>>),
    Apply (T::Predicate, Vec<Term<T>>),
}

/// terms
/// T : Theory
#[derive(Debug)]
pub enum Term<T : theory::Theory> {
    Var (ident::Ident), /// Variables
    Apply (T::Function, Vec<Term<T>>), // Function Applications
}


