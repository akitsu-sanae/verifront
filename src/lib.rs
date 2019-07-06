#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(slice_patterns)]

extern crate sexp;

pub mod format;
pub mod logic;
pub mod program;
pub mod sortcheck;
mod util;

#[cfg(test)]
mod test;
