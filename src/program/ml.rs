// tiny ML-like program
use super::{domain::*, ident::Ident};

#[derive(Debug)]
pub enum Expr<D: Domain> {
    Const(D::ConstSymbol),               // constatn values (e.g. true, 42)
    Op(D::OperatorSymbol, Vec<Expr<D>>), // primitive operation (e.g. and, sub, mult and div)
    If(Box<Expr<D>>, Box<Expr<D>>, Box<Expr<D>>),
    Let(Ident, Box<Expr<D>>, Box<Expr<D>>),
    Apply(Box<Expr<D>>, Box<Expr<D>>),
    Fun(Ident, Box<Expr<D>>),
    Var(Ident),
}

pub type BooleanProgram = Expr<boolean::Boolean>;
