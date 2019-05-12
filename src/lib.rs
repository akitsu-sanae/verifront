#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(type_alias_enum_variants)]
#![feature(slice_patterns)]

extern crate sexp;

pub mod logic;
pub mod theory;
pub mod program;
pub mod ident;
pub mod format;

#[cfg(test)]
mod test;

