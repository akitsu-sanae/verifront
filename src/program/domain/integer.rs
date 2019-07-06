use super::{Domain, boolean};

#[derive(Debug)]
pub struct Integer {}

#[derive(Debug)]
pub enum ConstSymbol {
    Boolean(boolean::ConstSymbol),
    Num(i64),
}


#[derive(Debug)]
pub enum OperatorSymbol {
    Boolean(boolean::OperatorSymbol),
    Add, Sub, Mult, Div, Mod
}

impl Domain for Integer {
    type ConstSymbol = ConstSymbol;
    type OperatorSymbol = OperatorSymbol;
}

