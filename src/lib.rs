#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(slice_patterns)]
#![feature(bind_by_move_pattern_guards)]

#[macro_use]
extern crate lazy_static;

extern crate sexp;

pub mod format;
pub mod logic;
pub mod program;
mod util;

#[cfg(test)]
mod test;
