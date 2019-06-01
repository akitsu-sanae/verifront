use sexp::{Atom, Sexp};

pub fn make_str_atom(str: &str) -> Sexp {
    Sexp::Atom(Atom::S(str.to_string()))
}

pub fn make_int_atom(n: i64) -> Sexp {
    Sexp::Atom(Atom::I(n))
}
