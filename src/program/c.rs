// tiny C-like program
use super::{domain::*, ident::Ident};

#[derive(Debug)]
pub enum Expr<D: Domain> {
    Const(D::ConstSymbol),               // constatn values (e.g. true, 42)
    Op(D::OperatorSymbol, Vec<Expr<D>>), // primitive operation (e.g. and, sub, mult and div)
    If(Box<Expr<D>>, Box<Expr<D>>, Box<Expr<D>>),
    Let(Ident, Box<Expr<D>>, Box<Expr<D>>),
    Apply(Ident, Vec<Expr<D>>),
    Var(Ident),
}

pub type Block<D> = Vec<Inst<D>>;

#[derive(Debug)]
pub enum Inst<D: Domain> {
    Let(Ident, Box<Expr<D>>),
    If(Expr<D>, Block<D>, Block<D>),
    While(Expr<D>, Block<D>),
    Return(Expr<D>),
}

#[derive(Debug)]
pub struct Function<D: Domain> {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub body: Vec<Inst<D>>,
}

pub type Program<D> = Vec<Function<D>>;

pub type BooleanProgram = Program<boolean::Boolean>;
