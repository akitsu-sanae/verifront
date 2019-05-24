use std::str::{self, FromStr};
use sexp::Sexp;
use crate::ident;
use crate::util;
use crate::logic::{expr::*, theory::*, binder::*};
use crate::format::{Format, smtlib2::*};

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

fn check_with_z3<T: Smtlib2Theory, B: Smtlib2Binder>(smtlib2: Smtlib2<T, B>, sexpr: Vec<Sexp>, status: Z3Status) {
    assert_eq!(
        Smtlib2::<T, B>::print(&smtlib2).unwrap(),
        sexpr);

    assert_eq!(
        smtlib2,
        Smtlib2::<T, B>::parse(&sexpr).unwrap());

    use std::io::Write;

    let mut file = tempfile::NamedTempFile::new().unwrap();
    for sexpr in sexpr {
        writeln!(file, "{}\n", sexpr).unwrap();
    }

    let result = ::std::process::Command::new("z3")
        .arg("-smt2")
        .arg(file.path())
        .output()
        .expect("failed to execute z3");
    let output = Z3Status::from_str(str::from_utf8(&result.stdout).expect("unrecognazed output")).unwrap();
    assert_eq!(output, status);
}


#[test]
fn print_parse() {
    check_with_z3( // true
        Smtlib2::new(vec!(
                Command::Assert(Propos::Const(Const::Symbol(boolean::ConstSymbol::True))),
                Command::CheckSat)),
        vec!(
            Sexp::List(vec!(
                util::make_str_atom("assert"),
                util::make_str_atom("true"))),
            Sexp::List(vec!(util::make_str_atom("check-sat")))),
        Z3Status::Sat);

    type FOL = FOLWithTheory<integer::Integer>;
    fn int_literal(n: i64) -> FOL {
        FOL::Const(Const::Symbol(integer::ConstSymbol::Number(n)))
    }
    check_with_z3( // exists x:Int. x < 100
        Smtlib2::new(vec!(
                Command::Assert(FOL::Binding(
                    Quantifier::Exists,
                    vec!((ident::make("x"), Sort::Symbol(integer::SortSymbol::Int))),
                    box FOL::Apply(
                        Function::Symbol(integer::FunctionSymbol::Lt),
                        vec!(
                            FOL::Const(Const::Var(ident::make("x"))),
                            int_literal(100))))),
                Command::CheckSat)),
        vec!(
            Sexp::List(vec!(
                    util::make_str_atom("assert"),
                    Sexp::List(vec!(
                            util::make_str_atom("exists"),
                            Sexp::List(vec!(Sexp::List(vec!(
                                util::make_str_atom("x"),
                                util::make_str_atom("Int"))))),
                            Sexp::List(vec!(
                                    util::make_str_atom("<"),
                                    util::make_str_atom("x"),
                                    util::make_int_atom(100))))))),
            Sexp::List(vec!(util::make_str_atom("check-sat")))),
        Z3Status::Sat);

    check_with_z3( // define-fun plus4 (x: Int): Int = x + 4; assert (plus4(2) = 6)
        Smtlib2::new(vec!(
                Command::DefineFun(FunDef {
                    name: ident::make("plus4"),
                    params: vec!((ident::make("x"), Sort::Symbol(integer::SortSymbol::Int))),
                    ret: Sort::Symbol(integer::SortSymbol::Int),
                    body: FOL::Apply(
                        Function::Symbol(integer::FunctionSymbol::Add),
                        vec!(
                            FOL::Const(Const::Var(ident::make("x"))),
                            int_literal(4)))
                }),
                Command::Assert(FOL::Apply(
                        Function::Symbol(integer::FunctionSymbol::Boolean(boolean::FunctionSymbol::Equal)),
                        vec!(
                            FOL::Apply(
                                Function::Var(ident::make("plus4")),
                                vec!(int_literal(2))),
                            int_literal(6)))),
                Command::CheckSat)),
        vec!(
            Sexp::List(vec!(
                    util::make_str_atom("define-fun"),
                    util::make_str_atom("plus4"),
                    Sexp::List(vec!(Sexp::List(vec!(
                            util::make_str_atom("x"),
                            util::make_str_atom("Int"))))),
                    util::make_str_atom("Int"),
                    Sexp::List(vec!(
                            util::make_str_atom("+"),
                            util::make_str_atom("x"),
                            util::make_int_atom(4))))),
            Sexp::List(vec!(
                    util::make_str_atom("assert"),
                    Sexp::List(vec!(
                            util::make_str_atom("="),
                            Sexp::List(vec!(
                                    util::make_str_atom("plus4"),
                                    util::make_int_atom(2))),
                            util::make_int_atom(6))))),
            Sexp::List(vec!(util::make_str_atom("check-sat")))),
        Z3Status::Sat);
}

