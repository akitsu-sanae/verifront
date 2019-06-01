use sexp::Sexp;
use crate::ident;
use crate::util;
use crate::logic::{expr::*, theory::*, binder::*};
use crate::format::{Format, smtlib2::*};

type FOL = FOLWithTheory<integer::Integer>;

fn check_with_z3<T: Smtlib2Theory, B: Smtlib2Binder>(smtlib2: Smtlib2<T, B>, sexpr: Vec<Sexp>, expected: &str) {

    assert_eq!(
        Smtlib2::<T, B>::print(&smtlib2).unwrap(),
        sexpr);

    /*
    assert_eq!(
        smtlib2,
        Smtlib2::<T, B>::parse(&sexpr).unwrap()); */

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
fn datatype() {
    // constructor example:
    // (declare-datatype Color ((red) (green) (blue)))
    check_with_z3::<integer::Integer, Quantifier>(
        Smtlib2::new(vec!(
            Command::DeclareDatatype(DatatypeDec {
                name: ident::make("Color"),
                params: vec!(),
                constructor_decs: vec!(
                    ConstructorDec {
                        name: ident::make("red"),
                        selector_decs: vec!(),
                    },
                    ConstructorDec {
                        name: ident::make("green"),
                        selector_decs: vec!(),
                    },
                    ConstructorDec {
                        name: ident::make("blue"),
                        selector_decs: vec!(),
                    }),
            }))),
        vec!(Sexp::List(vec!(
            util::make_str_atom("declare-datatype"),
            util::make_str_atom("Color"),
            Sexp::List(vec!(
                    Sexp::List(vec!(util::make_str_atom("red"))),
                    Sexp::List(vec!(util::make_str_atom("green"))),
                    Sexp::List(vec!(util::make_str_atom("blue")))))))),
        "");

    // selector example
    // (declare-datatyoe IntList (
    //     (empty)
    //     (insert (head Int) (tail Intlist))))
    check_with_z3::<integer::Integer, Quantifier>(
        Smtlib2::new(vec!(
            Command::DeclareDatatype(DatatypeDec {
                name: ident::make("IntList"),
                params: vec!(),
                constructor_decs: vec!(
                    ConstructorDec {
                        name: ident::make("empty"),
                        selector_decs: vec!(),
                    },
                    ConstructorDec {
                        name: ident::make("insert"),
                        selector_decs: vec!(
                            SelectorDec {
                                name: ident::make("head"),
                                sort: int_sort(),
                            },
                            SelectorDec {
                                name: ident::make("tail"),
                                sort: Sort::Var(ident::make("IntList")),
                            }),
                    })}))),
        vec!(Sexp::List(vec!(
            util::make_str_atom("declare-datatype"),
            util::make_str_atom("IntList"),
            Sexp::List(vec!(
                Sexp::List(vec!(util::make_str_atom("empty"))),
                Sexp::List(vec!(
                    util::make_str_atom("insert"),
                    Sexp::List(vec!(
                        util::make_str_atom("head"),
                        util::make_str_atom("Int"))),
                    Sexp::List(vec!(
                        util::make_str_atom("tail"),
                        util::make_str_atom("IntList")))))))))),
        "");

    /* TODO
    // parameter example
    // (declare-datatype List (par (T) (
    //     (nil)
    //     (cons (car T) (cdr (List T))))))
    check_with_z3::<integer::Integer, Quantifier>(
        Smtlib2::new(vec!(
            Command::DeclareDatatype(DatatypeDec {
                name: ident::make("List_"),
                params: vec!(ident::make("T")),
                constructor_decs: vec!(
                    ConstructorDec {
                        name: ident::make("nil"),
                        selector_decs: vec!(),
                    },
                    ConstructorDec {
                        name: ident::make("cons"),
                        selector_decs: vec!(
                            SelectorDec {
                                name: ident::make("car"),
                                sort: Sort::Var(ident::make("T")),
                            },
                            SelectorDec {
                                name: ident::make("cdr"),
                                sort: Sort::Var(ident::make("List_"), Sort::Var(ident::make("T"))),
                            }),
                    })}))),
        vec!(
            Sexp::List(vec!(
                util::make_str_atom("assert"),
                util::make_str_atom("true"))),
            Sexp::List(vec!(util::make_str_atom("check-sat")))),
        ""); */
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
            Command::DeclareDatatype(DatatypeDec {
                name: ident::make("Color"),
                params: vec!(),
                constructor_decs: vec!(
                    ConstructorDec {
                        name: ident::make("red"),
                        selector_decs: vec!(),
                    },
                    ConstructorDec {
                        name: ident::make("green"),
                        selector_decs: vec!(),
                    },
                    ConstructorDec {
                        name: ident::make("blue"),
                        selector_decs: vec!(),
                    }),
            }),
            // (declare-datatypes (T) (
            //     (Tree leaf (node (value T) (children TreeList)))
            //     (TreeList nil (cons (car Tree) (cdr TreeList)))))
            Command::DeclareDatatypes(
                vec!(ident::make("T")),
                vec!(
                    DatatypeDec {
                        name: ident::make("Tree"),
                        params: vec!(),
                        constructor_decs: vec!(
                            ConstructorDec {
                                name: ident::make("leaf"),
                                selector_decs: vec!(),
                            },
                            ConstructorDec {
                                name: ident::make("node"),
                                selector_decs: vec!(
                                    SelectorDec {
                                        name: ident::make("value"),
                                        sort: Sort::Var(ident::make("T")),
                                    },
                                    SelectorDec {
                                        name: ident::make("children"),
                                        sort: Sort::Var(ident::make("TreeList")),
                                    }),
                            }),
                    },
                    DatatypeDec {
                        name: ident::make("TreeList"),
                        params: vec!(),
                        constructor_decs: vec!(
                            ConstructorDec {
                                name: ident::make("nil"),
                                selector_decs: vec!(),
                            },
                            ConstructorDec {
                                name: ident::make("cons"),
                                selector_decs: vec!(
                                    SelectorDec {
                                        name: ident::make("car"),
                                        sort: Sort::Var(ident::make("Tree")),
                                    },
                                    SelectorDec {
                                        name: ident::make("cdr"),
                                        sort: Sort::Var(ident::make("TreeList")),
                                    }),
                            }),
                    })),
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
            Command::DefineFunRec(FunDef { // incr_rec(x: Int) = x+1
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
                util::make_str_atom("declare-const"),
                util::make_str_atom("A"),
                util::make_str_atom("Int"))),
            Sexp::List(vec!(
                util::make_str_atom("declare-const"),
                util::make_str_atom("B"),
                util::make_str_atom("Bool"))),
            Sexp::List(vec!(
                util::make_str_atom("assert"),
                util::make_str_atom("true"))),
            Sexp::List(vec!(util::make_str_atom("check-sat"))),
            Sexp::List(vec!(
                util::make_str_atom("check-sat-assuming"),
                Sexp::List(vec!(
                    util::make_str_atom("B"),
                    Sexp::List(vec!(util::make_str_atom("not"), util::make_str_atom("B"))))))),
            Sexp::List(vec!(
                util::make_str_atom("declare-datatype"),
                util::make_str_atom("Color"),
                Sexp::List(vec!(
                    Sexp::List(vec!(util::make_str_atom("red"))),
                    Sexp::List(vec!(util::make_str_atom("green"))),
                    Sexp::List(vec!(util::make_str_atom("blue"))))))),
            Sexp::List(vec!(
                util::make_str_atom("declare-datatypes"),
                Sexp::List(vec!(util::make_str_atom("T"))),
                Sexp::List(vec!(
                    Sexp::List(vec!(
                        util::make_str_atom("Tree"),
                        Sexp::List(vec!(util::make_str_atom("leaf"))),
                        Sexp::List(vec!(
                            util::make_str_atom("node"),
                            Sexp::List(vec!(
                                util::make_str_atom("value"),
                                util::make_str_atom("T"))),
                            Sexp::List(vec!(
                                util::make_str_atom("children"),
                                util::make_str_atom("TreeList"))))))),
                    Sexp::List(vec!(
                        util::make_str_atom("TreeList"),
                        Sexp::List(vec!(util::make_str_atom("nil"))),
                        Sexp::List(vec!(
                            util::make_str_atom("cons"),
                            Sexp::List(vec!(
                                util::make_str_atom("car"),
                                util::make_str_atom("Tree"))),
                            Sexp::List(vec!(
                                util::make_str_atom("cdr"),
                                util::make_str_atom("TreeList"))))))))))),
            Sexp::List(vec!(
                util::make_str_atom("declare-fun"),
                util::make_str_atom("id"),
                Sexp::List(vec!(util::make_str_atom("Int"))),
                util::make_str_atom("Int"))),
            Sexp::List(vec!(
                util::make_str_atom("declare-sort"),
                util::make_str_atom("sort1"),
                util::make_int_atom(1))),
            Sexp::List(vec!(
                util::make_str_atom("define-fun"),
                util::make_str_atom("incr"),
                Sexp::List(vec!(Sexp::List(vec!(
                    util::make_str_atom("x"),
                    util::make_str_atom("Int"))))),
                util::make_str_atom("Int"),
                Sexp::List(vec!(
                    util::make_str_atom("+"),
                    util::make_str_atom("x"),
                    util::make_int_atom(1))))),
            Sexp::List(vec!(
                util::make_str_atom("define-fun-rec"),
                util::make_str_atom("incr_rec"),
                Sexp::List(vec!(Sexp::List(vec!(
                    util::make_str_atom("x"),
                    util::make_str_atom("Int"))))),
                util::make_str_atom("Int"),
                Sexp::List(vec!(
                    util::make_str_atom("+"),
                    util::make_str_atom("x"),
                    util::make_int_atom(1))))),
            Sexp::List(vec!(
                util::make_str_atom("define-funs-rec"),
                Sexp::List(vec!(
                    Sexp::List(vec!(
                        util::make_str_atom("even"),
                        Sexp::List(vec!(Sexp::List(vec!(
                            util::make_str_atom("x"),
                            util::make_str_atom("Int"))))),
                        util::make_str_atom("Int"))),
                    Sexp::List(vec!(
                        util::make_str_atom("odd"),
                        Sexp::List(vec!(Sexp::List(vec!(
                            util::make_str_atom("x"),
                            util::make_str_atom("Int"))))),
                        util::make_str_atom("Int"))))),
                Sexp::List(vec!(
                    Sexp::List(vec!(
                        util::make_str_atom("ite"),
                        Sexp::List(vec!(
                            util::make_str_atom("="),
                            util::make_str_atom("x"),
                            util::make_int_atom(1))),
                        util::make_int_atom(0),
                        Sexp::List(vec!(
                            util::make_str_atom("odd"),
                            Sexp::List(vec!(
                                util::make_str_atom("-"),
                                util::make_str_atom("x"),
                                util::make_int_atom(1))))))),
                    Sexp::List(vec!(
                        util::make_str_atom("even"),
                        Sexp::List(vec!(
                            util::make_str_atom("-"),
                            util::make_str_atom("x"),
                            util::make_int_atom(1))))))))),
            Sexp::List(vec!(
                util::make_str_atom("define-sort"),
                util::make_str_atom("sort_id"),
                Sexp::List(vec!(util::make_str_atom("a"))),
                util::make_str_atom("a"))),
            Sexp::List(vec!(
                util::make_str_atom("echo"),
                util::make_str_atom(r"some message here!"))),
            Sexp::List(vec!(util::make_str_atom("exit")))),
        "sat\nunsat\nsome message here!\n");
}

