use std::str::{self, FromStr};
use sexp::{Sexp, Atom};
use crate::ident;
use crate::logic::{expr::*, theory::*, binder::*};
use crate::format::{Format, smtlib2::{Smtlib2, Smtlib2Theory, Smtlib2Binder}};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Z3Status {
    Sat, Unsat,
}

impl FromStr for Z3Status {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim() {
            "sat" | "Sat" | "SAT" => Ok(Z3Status::Sat),
            "unsat" | "Unsat" | "UNSAT" => Ok(Z3Status::Unsat),
            s => Err(format!("unknown z3 status: {}", s))
        }
    }
}

fn check_with_z3<T: Smtlib2Theory, B: Smtlib2Binder>(expr: &Expr<T, B>, sexpr: &Vec<Sexp>, status: Z3Status) {
    assert_eq!(
        &Smtlib2::<Expr<T, B>>::print(expr).unwrap(),
        sexpr);

    assert_eq!(
        expr,
        &Smtlib2::<Expr<T, B>>::parse(sexpr).unwrap());

    use std::io::Write;

    let mut file = tempfile::NamedTempFile::new().unwrap();
    for sexpr in sexpr {
        writeln!(file, "{}\n", sexpr).unwrap();
    }

    use std::process::Command;

    let result = Command::new("z3")
        .arg("-smt2")
        .arg(file.path())
        .output()
        .expect("failed to execute z3");
    let output = Z3Status::from_str(str::from_utf8(&result.stdout).expect("unrecognazed output")).unwrap();
    assert_eq!(output, status);
}


#[test]
fn print() {
    check_with_z3( // true
        &Propos::Const(Const::Symbol(boolean::ConstSymbol::True)),
        &vec!(
            Sexp::List(vec!(
                Sexp::Atom(Atom::S("assert".to_string())),
                Sexp::Atom(Atom::S("true".to_string())))),
            Sexp::List(vec!(Sexp::Atom(Atom::S("check-sat".to_string()))))),
        Z3Status::Sat);

    type FOL = FOLWithTheory<integer::Integer>;
    check_with_z3( // exists x:Int. x < 100
        &FOL::Binding(
            Quantifier::Exists,
            vec!((ident::make("x"), Sort::Symbol(integer::SortSymbol::Int))),
            box FOL::Apply(
                Function::Symbol(integer::FunctionSymbol::Lt),
                vec!(
                    FOL::Const(Const::Var(ident::make("x"))),
                    FOL::Const(Const::Symbol(integer::ConstSymbol::Number(100)))))),
        &vec!(
            Sexp::List(vec!(
                    Sexp::Atom(Atom::S("assert".to_string())),
                    Sexp::List(vec!(
                            Sexp::Atom(Atom::S("exists".to_string())),
                            Sexp::List(vec!(Sexp::List(vec!(
                                Sexp::Atom(Atom::S("x".to_string())),
                                Sexp::Atom(Atom::S("Int".to_string())))))),
                            Sexp::List(vec!(
                                Sexp::Atom(Atom::S("<".to_string())),
                                Sexp::Atom(Atom::S("x".to_string())),
                                Sexp::Atom(Atom::I(100)))))))),
            Sexp::List(vec!(Sexp::Atom(Atom::S("check-sat".to_string()))))),
        Z3Status::Sat);
}

