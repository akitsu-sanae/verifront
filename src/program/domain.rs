use std::fmt::Debug;

pub mod boolean;
pub mod integer;

pub trait Domain {
    type ConstSymbol: Debug;
    type OperatorSymbol: Debug;
}
