use sexp::Sexp;
use crate::ident;
use crate::util;
use crate::logic::{expr::*, theory::*, binder::*};
use crate::format::{Format, smtlib2::*};

type FOL = FOLWithTheory<integer::Integer>;

fn check_with_z3<T: Smtlib2Theory, B: Smtlib2Binder>(smtlib2: Smtlib2<T, B>, _sexpr: Vec<Sexp>, expected: &str) {
    /*
    assert_eq!(
        Smtlib2::<T, B>::print(&smtlib2).unwrap(),
        sexpr);

    assert_eq!(
        smtlib2,
        Smtlib2::<T, B>::parse(&sexpr).unwrap()); */

    let sexpr = Smtlib2::<T, B>::print(&smtlib2).unwrap();
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
    assert_eq!(expected, ::std::str::from_utf8(&result.stdout).expect("unrecognazed output"));
}


#[test]
fn primitive() {
    check_with_z3( // assert(true)
        Smtlib2::new(vec!(
                Command::Assert(Propos::Const(Const::Symbol(boolean::ConstSymbol::True))),
                Command::CheckSat)),
        vec!(
            Sexp::List(vec!(
                util::make_str_atom("assert"),
                util::make_str_atom("true"))),
            Sexp::List(vec!(util::make_str_atom("check-sat")))),
        "sat\n");
}

fn int_literal(n: i64) -> FOL {
    FOL::Const(Const::Symbol(integer::ConstSymbol::Number(n)))
}

fn int_sort() -> Sort<integer::SortSymbol> {
    Sort::Symbol(integer::SortSymbol::Int)
}

fn bool_sort() -> Sort<integer::SortSymbol> {
    Sort::Symbol(integer::SortSymbol::Bool)
}


#[test]
fn fol() { // assert(exists x:Int. x < 100)
    check_with_z3(
        Smtlib2::new(vec!(
                Command::Assert(FOL::Binding(
                    Quantifier::Exists,
                    vec!((ident::make("x"), int_sort())),
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
        "sat\n");
}

#[test]
fn define_fun() { // define-fun plus4 (x: Int): Int = x + 4; assert (plus4(2) = 6)
    check_with_z3(
        Smtlib2::new(vec!(
                Command::DefineFun(FunDef {
                    name: ident::make("plus4"),
                    params: vec!((ident::make("x"), int_sort())),
                    ret: int_sort(),
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
        "sat\n");
}

#[test]
fn commands() {
    check_with_z3(
        Smtlib2::new(vec!(
            Command::DeclareConst((ident::make("A"), int_sort())),
            Command::DeclareConst((ident::make("B"), bool_sort())),
            Command::Assert(FOL::Const(Const::Symbol(integer::ConstSymbol::Boolean(boolean::ConstSymbol::True)))),
            Command::CheckSat,
            Command::CheckSatAssuming(vec!(ident::make("B")), vec!(ident::make("B"))),
            // Command::DeclareDatatype(ident::make("foo"), DatatypeDec {
            //    // type foo<'a> = bar of {hoge: int} | baz of {fuga: 'a}
            //    param: vec!(ident::make("'a")),
            //    ctors: vec!(
            //        (ident::make("bar"), vec!((ident::make("hoge"), int_sort()))),
            //        (ident::make("baz"), vec!((ident::make("fuga"), Sort::Var(ident::make("'a")))))),
           //  }),
            Command::DeclareDatatypes(vec!()), // TODO
            Command::DeclareFun(FunDec { // id(x: Int) = x
                name: ident::make("id"),
                params: vec!(int_sort()),
                ret: int_sort(),
            }),
            Command::DeclareSort(ident::make("sort1"), 1),
            Command::DefineFun(FunDef { // incr(x: Int) = x+1
                name: ident::make("incr"),
                params: vec!((ident::make("x"), int_sort())),
                ret: int_sort(),
                body: FOL::Apply(
                    Function::Symbol(integer::FunctionSymbol::Add),
                    vec!(
                        FOL::Const(Const::Var(ident::make("x"))),
                        int_literal(1)))
            }),
            Command::DefineFunRec(FunDef { // incr(x: Int) = x+1
                name: ident::make("incr_rec"),
                params: vec!((ident::make("x"), int_sort())),
                ret: int_sort(),
                body: FOL::Apply(
                    Function::Symbol(integer::FunctionSymbol::Add),
                    vec!(
                        FOL::Const(Const::Var(ident::make("x"))),
                        int_literal(1)))
            }),
            Command::DefineFunsRec(vec!(
                    // even(x: Int) = if x == 0 then 0 else odd(x-1)
                    FunDef {
                        name: ident::make("even"),
                        params: vec!((ident::make("x"), int_sort())),
                        ret: int_sort(),
                        body: FOL::Apply(
                            Function::Symbol(integer::FunctionSymbol::from(boolean::FunctionSymbol::IfThenElse)),
                            vec!(
                                FOL::Apply(
                                    Function::Symbol(integer::FunctionSymbol::from(boolean::FunctionSymbol::Equal)),
                                    vec!(
                                        FOL::Const(Const::Var(ident::make("x"))),
                                        int_literal(1))),
                                int_literal(0),
                                FOL::Apply(
                                    Function::Var(ident::make("odd")),
                                    vec!(FOL::Apply(
                                            Function::Symbol(integer::FunctionSymbol::Sub),
                                            vec!(
                                                FOL::Const(Const::Var(ident::make("x"))),
                                                int_literal(1))))))),
                    },
                    // odd(x: Int) = even(x-1)
                    FunDef {
                        name: ident::make("odd"),
                        params: vec!((ident::make("x"), int_sort())),
                        ret: int_sort(),
                        body: FOL::Apply(
                            Function::Var(ident::make("even")),
                            vec!(FOL::Apply(
                                    Function::Symbol(integer::FunctionSymbol::Sub),
                                    vec!(
                                        FOL::Const(Const::Var(ident::make("x"))),
                                        int_literal(1))))),
                    })),
            Command::DefineSort(ident::make("sort_id"), vec!(ident::make("a")), Sort::Var(ident::make("a"))),
            Command::Echo("some message here!".to_string()),
            Command::Exit)),
        vec!(
            Sexp::List(vec!(
                util::make_str_atom("assert"),
                util::make_str_atom("true"))),
            Sexp::List(vec!(util::make_str_atom("check-sat")))),
        "sat\nunsat\nsome message here!\n");
}

