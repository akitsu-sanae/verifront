use sexp::{Atom, Sexp};

pub fn make_str_atom(str: &str) -> Sexp {
    Sexp::Atom(Atom::S(str.to_string()))
}

pub fn make_bool_atom(b: bool) -> Sexp {
    Sexp::Atom(Atom::S(if b {
        "true".to_string()
    } else {
        "false".to_string()
    }))
}

pub fn make_int_atom(n: i64) -> Sexp {
    Sexp::Atom(Atom::I(n))
}
