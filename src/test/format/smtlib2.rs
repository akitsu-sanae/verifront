use std::str::{self, FromStr};
use sexp::{Sexp, Atom};
use crate::logic::{*, Propos};
use crate::theory::boolean;
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
    use boolean::Const;
    check_with_z3( // true
        &Propos::Const(Const::True),
        &vec!(
            Sexp::List(vec!(
                Sexp::Atom(Atom::S("assert".to_string())),
                Sexp::Atom(Atom::S("true".to_string())))),
            Sexp::List(vec!(Sexp::Atom(Atom::S("check-sat".to_string()))))),
        Z3Status::Sat);
}

