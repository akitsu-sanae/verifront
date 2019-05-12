use sexp::{Sexp, Atom};
pub fn make_atom(str: &str) -> Sexp {
    Sexp::Atom(Atom::S(str.to_string()))
}



