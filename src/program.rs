use crate::ident::Ident;

// tiny ML-like program

pub trait Value {
    type Constant;
    type Operation;
}

pub mod boolean {

    #[derive(Debug)]
    pub struct Boolean {}

    #[derive(Debug)]
    pub enum Constant {
        True,
        False,
    }

    #[derive(Debug)]
    pub enum Operation {
        And,
        Or,
        Not,
        Equal,
    }

    impl super::Value for Boolean {
        type Constant = self::Constant;
        type Operation = self::Operation;
    }

}

#[derive(Debug)]
pub enum Expr<V: Value> {
    Constant(V::Constant),          // constatn values (e.g. true, 42)
    Op(V::Operation, Vec<Expr<V>>), // primitive operation (e.g. and, sub, mult and div)
    If(Box<Expr<V>>, Box<Expr<V>>, Box<Expr<V>>),
    Let(Ident, Box<Expr<V>>, Box<Expr<V>>),
    Apply(Vec<Expr<V>>),
    Var(Ident),
}

pub type BoolExpr = Expr<boolean::Boolean>;
