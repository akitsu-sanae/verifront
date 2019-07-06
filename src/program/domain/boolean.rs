use super::Domain;

#[derive(Debug)]
pub struct Boolean {}

#[derive(Debug)]
pub enum ConstSymbol {
    True,
    False,
}

#[derive(Debug)]
pub enum OperatorSymbol {
    And,
    Or,
    Not,
    Equal,
}

impl Domain for Boolean {
    type ConstSymbol = ConstSymbol;
    type OperatorSymbol = OperatorSymbol;
}
