
use super::Theory;

#[derive(Debug)]
pub enum Function {
    True, False, Eq
}

#[derive(Debug)]
pub enum Predicate {
    True, False, Eq
}

#[derive(Debug)]
pub struct Boolean {
}

impl Theory for Boolean {
    type Function = self::Function;
    type Predicate = self::Predicate;
}

