use sexp::{Sexp, Atom};
use crate::ident;
use crate::logic::{*, Propos};
use crate::format::{Format, smtlib2::{Smtlib2, Smtlib2Theory, Smtlib2Binder}};

fn make_var<T: Smtlib2Theory, B: Smtlib2Binder>(name: &str) -> Expr<T, B> {
    Expr::Var(ident::make(name))
}

#[test]
fn print() {
    assert_eq!(
        Smtlib2::<Propos>::print(&make_var("A")).unwrap(),
        vec!(
            Sexp::Atom(Atom::S("assert".to_string())),
            Sexp::Atom(Atom::S("A".to_string()))));
}

