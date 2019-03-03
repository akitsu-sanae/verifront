
pub mod boolean;

pub trait Theory {
    type Function: std::fmt::Debug;
    type Predicate: std::fmt::Debug;
}


