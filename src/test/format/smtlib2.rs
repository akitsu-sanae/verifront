use crate::format::{smtlib2::*, Format};
use crate::logic::{binder::*, expr::*, symbol, theory::*};
use crate::util;
use sexp::Sexp;

type FOL = FOLWithTheory<integer::Integer>;

fn check_with_z3<T: Smtlib2Theory, B: Smtlib2Binder>(
    smtlib2: Smtlib2<T, B>,
    sexpr: Vec<Sexp>,
    expected: &str,
) {
    assert_eq!(Smtlib2::<T, B>::print(&smtlib2).unwrap(), sexpr);

    assert_eq!(smtlib2, Smtlib2::<T, B>::parse(&sexpr).unwrap());

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
    assert_eq!(
        expected,
        ::std::str::from_utf8(&result.stdout).expect("unrecognazed output")
    );
}

#[test]
fn primitive() {
    check_with_z3(
        // assert(true)
        Smtlib2::new(vec![
            Command::Assert(Propos::Const(Const::Symbol(boolean::ConstSymbol::True))),
            Command::CheckSat,
        ]),
        vec![
            Sexp::List(vec![
                util::make_str_atom("assert"),
                util::make_str_atom("true"),
            ]),
            Sexp::List(vec![util::make_str_atom("check-sat")]),
        ],
        "sat\n",
    );
}

fn int_literal(n: i64) -> FOL {
    FOL::Const(Const::Symbol(integer::ConstSymbol::Number(n)))
}

fn int_sort() -> Sort<integer::Integer> {
    Sort::Symbol(integer::SortSymbol::Int)
}

fn bool_sort() -> Sort<integer::Integer> {
    Sort::Symbol(integer::SortSymbol::Bool)
}

#[test]
fn fol() {
    // assert(exists x:Int. x < 100)
    check_with_z3(
        Smtlib2::new(vec![
            Command::Assert(FOL::Binding(
                Quantifier::Exists,
                vec![(symbol::make("x"), int_sort())],
                box FOL::Apply(
                    Function::Symbol(integer::FunctionSymbol::Lt),
                    vec![FOL::Const(Const::Var(symbol::make("x"))), int_literal(100)],
                ),
            )),
            Command::CheckSat,
        ]),
        vec![
            Sexp::List(vec![
                util::make_str_atom("assert"),
                Sexp::List(vec![
                    util::make_str_atom("exists"),
                    Sexp::List(vec![Sexp::List(vec![
                        util::make_str_atom("x"),
                        util::make_str_atom("Int"),
                    ])]),
                    Sexp::List(vec![
                        util::make_str_atom("<"),
                        util::make_str_atom("x"),
                        util::make_int_atom(100),
                    ]),
                ]),
            ]),
            Sexp::List(vec![util::make_str_atom("check-sat")]),
        ],
        "sat\n",
    );
}

#[test]
fn define_fun() {
    // define-fun plus4 (x: Int): Int = x + 4; assert (plus4(2) = 6)
    check_with_z3(
        Smtlib2::new(vec![
            Command::DefineFun(FunDef {
                name: symbol::make("plus4"),
                params: vec![(symbol::make("x"), int_sort())],
                ret: int_sort(),
                body: FOL::Apply(
                    Function::Symbol(integer::FunctionSymbol::Add),
                    vec![FOL::Const(Const::Var(symbol::make("x"))), int_literal(4)],
                ),
            }),
            Command::Assert(FOL::Apply(
                Function::Symbol(integer::FunctionSymbol::Boolean(
                    boolean::FunctionSymbol::Equal,
                )),
                vec![
                    FOL::Apply(Function::Var(symbol::make("plus4")), vec![int_literal(2)]),
                    int_literal(6),
                ],
            )),
            Command::CheckSat,
        ]),
        vec![
            Sexp::List(vec![
                util::make_str_atom("define-fun"),
                util::make_str_atom("plus4"),
                Sexp::List(vec![Sexp::List(vec![
                    util::make_str_atom("x"),
                    util::make_str_atom("Int"),
                ])]),
                util::make_str_atom("Int"),
                Sexp::List(vec![
                    util::make_str_atom("+"),
                    util::make_str_atom("x"),
                    util::make_int_atom(4),
                ]),
            ]),
            Sexp::List(vec![
                util::make_str_atom("assert"),
                Sexp::List(vec![
                    util::make_str_atom("="),
                    Sexp::List(vec![util::make_str_atom("plus4"), util::make_int_atom(2)]),
                    util::make_int_atom(6),
                ]),
            ]),
            Sexp::List(vec![util::make_str_atom("check-sat")]),
        ],
        "sat\n",
    );
}

#[test]
fn datatype() {
    // constructor example:
    // (declare-datatype Color ((red) (green) (blue)))
    check_with_z3::<integer::Integer, Quantifier>(
        Smtlib2::new(vec![Command::DeclareDatatype(DatatypeDec {
            name: symbol::make("Color"),
            params: vec![],
            constructor_decs: vec![
                ConstructorDec {
                    name: symbol::make("red"),
                    selector_decs: vec![],
                },
                ConstructorDec {
                    name: symbol::make("green"),
                    selector_decs: vec![],
                },
                ConstructorDec {
                    name: symbol::make("blue"),
                    selector_decs: vec![],
                },
            ],
        })]),
        vec![Sexp::List(vec![
            util::make_str_atom("declare-datatype"),
            util::make_str_atom("Color"),
            Sexp::List(vec![
                Sexp::List(vec![util::make_str_atom("red")]),
                Sexp::List(vec![util::make_str_atom("green")]),
                Sexp::List(vec![util::make_str_atom("blue")]),
            ]),
        ])],
        "",
    );

    // selector example
    // (declare-datatyoe IntList (
    //     (empty)
    //     (insert (head Int) (tail Intlist))))
    check_with_z3::<integer::Integer, Quantifier>(
        Smtlib2::new(vec![Command::DeclareDatatype(DatatypeDec {
            name: symbol::make("IntList"),
            params: vec![],
            constructor_decs: vec![
                ConstructorDec {
                    name: symbol::make("empty"),
                    selector_decs: vec![],
                },
                ConstructorDec {
                    name: symbol::make("insert"),
                    selector_decs: vec![
                        SelectorDec {
                            name: symbol::make("head"),
                            sort: int_sort(),
                        },
                        SelectorDec {
                            name: symbol::make("tail"),
                            sort: Sort::Var(symbol::make("IntList")),
                        },
                    ],
                },
            ],
        })]),
        vec![Sexp::List(vec![
            util::make_str_atom("declare-datatype"),
            util::make_str_atom("IntList"),
            Sexp::List(vec![
                Sexp::List(vec![util::make_str_atom("empty")]),
                Sexp::List(vec![
                    util::make_str_atom("insert"),
                    Sexp::List(vec![
                        util::make_str_atom("head"),
                        util::make_str_atom("Int"),
                    ]),
                    Sexp::List(vec![
                        util::make_str_atom("tail"),
                        util::make_str_atom("IntList"),
                    ]),
                ]),
            ]),
        ])],
        "",
    );

    /* TODO
    // parameter example
    // (declare-datatype List (par (T) (
    //     (nil)
    //     (cons (car T) (cdr (List T))))))
    check_with_z3::<integer::Integer, Quantifier>(
        Smtlib2::new(vec!(
            Command::DeclareDatatype(DatatypeDec {
                name: symbol::make("List_"),
                params: vec!(symbol::make("T")),
                constructor_decs: vec!(
                    ConstructorDec {
                        name: symbol::make("nil"),
                        selector_decs: vec!(),
                    },
                    ConstructorDec {
                        name: symbol::make("cons"),
                        selector_decs: vec!(
                            SelectorDec {
                                name: symbol::make("car"),
                                sort: Sort::Var(symbol::make("T")),
                            },
                            SelectorDec {
                                name: symbol::make("cdr"),
                                sort: Sort::Var(symbol::make("List_"), Sort::Var(symbol::make("T"))),
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
        Smtlib2::new(vec![
            Command::SetOption(Option::InteractiveMode(true)),
            Command::SetOption(Option::ProduceProofs(true)),
            Command::SetOption(Option::ProduceUnsatAssumptions(true)),
            Command::SetOption(Option::ProduceUnsatCores(true)),
            Command::DeclareConst((symbol::make("A"), int_sort())),
            Command::DeclareConst((symbol::make("B"), bool_sort())),
            Command::Assert(FOL::Const(Const::Symbol(integer::ConstSymbol::Boolean(
                boolean::ConstSymbol::True,
            )))),
            Command::CheckSat,
            Command::CheckSatAssuming(vec![symbol::make("B")], vec![symbol::make("B")]),
            Command::DeclareDatatype(DatatypeDec {
                name: symbol::make("Color"),
                params: vec![],
                constructor_decs: vec![
                    ConstructorDec {
                        name: symbol::make("red"),
                        selector_decs: vec![],
                    },
                    ConstructorDec {
                        name: symbol::make("green"),
                        selector_decs: vec![],
                    },
                    ConstructorDec {
                        name: symbol::make("blue"),
                        selector_decs: vec![],
                    },
                ],
            }),
            // (declare-datatypes (T) (
            //     (Tree leaf (node (value T) (children TreeList)))
            //     (TreeList nil (cons (car Tree) (cdr TreeList)))))
            Command::DeclareDatatypes(
                vec![symbol::make("T")],
                vec![
                    DatatypeDec {
                        name: symbol::make("Tree"),
                        params: vec![],
                        constructor_decs: vec![
                            ConstructorDec {
                                name: symbol::make("leaf"),
                                selector_decs: vec![],
                            },
                            ConstructorDec {
                                name: symbol::make("node"),
                                selector_decs: vec![
                                    SelectorDec {
                                        name: symbol::make("value"),
                                        sort: Sort::Var(symbol::make("T")),
                                    },
                                    SelectorDec {
                                        name: symbol::make("children"),
                                        sort: Sort::Var(symbol::make("TreeList")),
                                    },
                                ],
                            },
                        ],
                    },
                    DatatypeDec {
                        name: symbol::make("TreeList"),
                        params: vec![],
                        constructor_decs: vec![
                            ConstructorDec {
                                name: symbol::make("nil"),
                                selector_decs: vec![],
                            },
                            ConstructorDec {
                                name: symbol::make("cons"),
                                selector_decs: vec![
                                    SelectorDec {
                                        name: symbol::make("car"),
                                        sort: Sort::Var(symbol::make("Tree")),
                                    },
                                    SelectorDec {
                                        name: symbol::make("cdr"),
                                        sort: Sort::Var(symbol::make("TreeList")),
                                    },
                                ],
                            },
                        ],
                    },
                ],
            ),
            Command::DeclareFun(FunDec {
                // id(x: Int) = x
                name: symbol::make("id"),
                params: vec![int_sort()],
                ret: int_sort(),
            }),
            Command::DeclareSort(symbol::make("sort1"), 1),
            Command::DefineFun(FunDef {
                // incr(x: Int) = x+1
                name: symbol::make("incr"),
                params: vec![(symbol::make("x"), int_sort())],
                ret: int_sort(),
                body: FOL::Apply(
                    Function::Symbol(integer::FunctionSymbol::Add),
                    vec![FOL::Const(Const::Var(symbol::make("x"))), int_literal(1)],
                ),
            }),
            Command::DefineFunRec(FunDef {
                // incr_rec(x: Int) = x+1
                name: symbol::make("incr_rec"),
                params: vec![(symbol::make("x"), int_sort())],
                ret: int_sort(),
                body: FOL::Apply(
                    Function::Symbol(integer::FunctionSymbol::Add),
                    vec![FOL::Const(Const::Var(symbol::make("x"))), int_literal(1)],
                ),
            }),
            Command::DefineFunsRec(vec![
                // even(x: Int) = if x == 0 then 0 else odd(x-1)
                FunDef {
                    name: symbol::make("even"),
                    params: vec![(symbol::make("x"), int_sort())],
                    ret: int_sort(),
                    body: FOL::Apply(
                        Function::Symbol(integer::FunctionSymbol::from(
                            boolean::FunctionSymbol::IfThenElse,
                        )),
                        vec![
                            FOL::Apply(
                                Function::Symbol(integer::FunctionSymbol::from(
                                    boolean::FunctionSymbol::Equal,
                                )),
                                vec![FOL::Const(Const::Var(symbol::make("x"))), int_literal(1)],
                            ),
                            int_literal(0),
                            FOL::Apply(
                                Function::Var(symbol::make("odd")),
                                vec![FOL::Apply(
                                    Function::Symbol(integer::FunctionSymbol::Sub),
                                    vec![FOL::Const(Const::Var(symbol::make("x"))), int_literal(1)],
                                )],
                            ),
                        ],
                    ),
                },
                // odd(x: Int) = even(x-1)
                FunDef {
                    name: symbol::make("odd"),
                    params: vec![(symbol::make("x"), int_sort())],
                    ret: int_sort(),
                    body: FOL::Apply(
                        Function::Var(symbol::make("even")),
                        vec![FOL::Apply(
                            Function::Symbol(integer::FunctionSymbol::Sub),
                            vec![FOL::Const(Const::Var(symbol::make("x"))), int_literal(1)],
                        )],
                    ),
                },
            ]),
            Command::DefineSort(
                symbol::make("sort_id"),
                vec![symbol::make("a")],
                Sort::Var(symbol::make("a")),
            ),
            Command::Echo("some message here!".to_string()),
            Command::GetAssertions,
            Command::GetInfo(InfoFlag::Version),
            Command::GetProof,
            Command::GetUnsatAssumptions,
            Command::GetUnsatCore,
            Command::Push(1),
            Command::Pop(1),
            Command::Reset,
            Command::ResetAssertions,
            Command::Exit,
        ]),
        vec![
            Sexp::List(vec![
                util::make_str_atom("set-option"),
                util::make_str_atom(":interactive-mode"),
                util::make_str_atom("true"),
            ]),
            Sexp::List(vec![
                util::make_str_atom("set-option"),
                util::make_str_atom(":produce-proofs"),
                util::make_str_atom("true"),
            ]),
            Sexp::List(vec![
                util::make_str_atom("set-option"),
                util::make_str_atom(":produce-unsat-assumptions"),
                util::make_str_atom("true"),
            ]),
            Sexp::List(vec![
                util::make_str_atom("set-option"),
                util::make_str_atom(":produce-unsat-cores"),
                util::make_str_atom("true"),
            ]),
            Sexp::List(vec![
                util::make_str_atom("declare-const"),
                util::make_str_atom("A"),
                util::make_str_atom("Int"),
            ]),
            Sexp::List(vec![
                util::make_str_atom("declare-const"),
                util::make_str_atom("B"),
                util::make_str_atom("Bool"),
            ]),
            Sexp::List(vec![
                util::make_str_atom("assert"),
                util::make_str_atom("true"),
            ]),
            Sexp::List(vec![util::make_str_atom("check-sat")]),
            Sexp::List(vec![
                util::make_str_atom("check-sat-assuming"),
                Sexp::List(vec![
                    util::make_str_atom("B"),
                    Sexp::List(vec![util::make_str_atom("not"), util::make_str_atom("B")]),
                ]),
            ]),
            Sexp::List(vec![
                util::make_str_atom("declare-datatype"),
                util::make_str_atom("Color"),
                Sexp::List(vec![
                    Sexp::List(vec![util::make_str_atom("red")]),
                    Sexp::List(vec![util::make_str_atom("green")]),
                    Sexp::List(vec![util::make_str_atom("blue")]),
                ]),
            ]),
            Sexp::List(vec![
                util::make_str_atom("declare-datatypes"),
                Sexp::List(vec![util::make_str_atom("T")]),
                Sexp::List(vec![
                    Sexp::List(vec![
                        util::make_str_atom("Tree"),
                        Sexp::List(vec![util::make_str_atom("leaf")]),
                        Sexp::List(vec![
                            util::make_str_atom("node"),
                            Sexp::List(vec![
                                util::make_str_atom("value"),
                                util::make_str_atom("T"),
                            ]),
                            Sexp::List(vec![
                                util::make_str_atom("children"),
                                util::make_str_atom("TreeList"),
                            ]),
                        ]),
                    ]),
                    Sexp::List(vec![
                        util::make_str_atom("TreeList"),
                        Sexp::List(vec![util::make_str_atom("nil")]),
                        Sexp::List(vec![
                            util::make_str_atom("cons"),
                            Sexp::List(vec![
                                util::make_str_atom("car"),
                                util::make_str_atom("Tree"),
                            ]),
                            Sexp::List(vec![
                                util::make_str_atom("cdr"),
                                util::make_str_atom("TreeList"),
                            ]),
                        ]),
                    ]),
                ]),
            ]),
            Sexp::List(vec![
                util::make_str_atom("declare-fun"),
                util::make_str_atom("id"),
                Sexp::List(vec![util::make_str_atom("Int")]),
                util::make_str_atom("Int"),
            ]),
            Sexp::List(vec![
                util::make_str_atom("declare-sort"),
                util::make_str_atom("sort1"),
                util::make_int_atom(1),
            ]),
            Sexp::List(vec![
                util::make_str_atom("define-fun"),
                util::make_str_atom("incr"),
                Sexp::List(vec![Sexp::List(vec![
                    util::make_str_atom("x"),
                    util::make_str_atom("Int"),
                ])]),
                util::make_str_atom("Int"),
                Sexp::List(vec![
                    util::make_str_atom("+"),
                    util::make_str_atom("x"),
                    util::make_int_atom(1),
                ]),
            ]),
            Sexp::List(vec![
                util::make_str_atom("define-fun-rec"),
                util::make_str_atom("incr_rec"),
                Sexp::List(vec![Sexp::List(vec![
                    util::make_str_atom("x"),
                    util::make_str_atom("Int"),
                ])]),
                util::make_str_atom("Int"),
                Sexp::List(vec![
                    util::make_str_atom("+"),
                    util::make_str_atom("x"),
                    util::make_int_atom(1),
                ]),
            ]),
            Sexp::List(vec![
                util::make_str_atom("define-funs-rec"),
                Sexp::List(vec![
                    Sexp::List(vec![
                        util::make_str_atom("even"),
                        Sexp::List(vec![Sexp::List(vec![
                            util::make_str_atom("x"),
                            util::make_str_atom("Int"),
                        ])]),
                        util::make_str_atom("Int"),
                    ]),
                    Sexp::List(vec![
                        util::make_str_atom("odd"),
                        Sexp::List(vec![Sexp::List(vec![
                            util::make_str_atom("x"),
                            util::make_str_atom("Int"),
                        ])]),
                        util::make_str_atom("Int"),
                    ]),
                ]),
                Sexp::List(vec![
                    Sexp::List(vec![
                        util::make_str_atom("ite"),
                        Sexp::List(vec![
                            util::make_str_atom("="),
                            util::make_str_atom("x"),
                            util::make_int_atom(1),
                        ]),
                        util::make_int_atom(0),
                        Sexp::List(vec![
                            util::make_str_atom("odd"),
                            Sexp::List(vec![
                                util::make_str_atom("-"),
                                util::make_str_atom("x"),
                                util::make_int_atom(1),
                            ]),
                        ]),
                    ]),
                    Sexp::List(vec![
                        util::make_str_atom("even"),
                        Sexp::List(vec![
                            util::make_str_atom("-"),
                            util::make_str_atom("x"),
                            util::make_int_atom(1),
                        ]),
                    ]),
                ]),
            ]),
            Sexp::List(vec![
                util::make_str_atom("define-sort"),
                util::make_str_atom("sort_id"),
                Sexp::List(vec![util::make_str_atom("a")]),
                util::make_str_atom("a"),
            ]),
            Sexp::List(vec![
                util::make_str_atom("echo"),
                util::make_str_atom(r"some message here!"),
            ]),
            Sexp::List(vec![util::make_str_atom("get-assertions")]),
            Sexp::List(vec![
                util::make_str_atom("get-info"),
                util::make_str_atom(":version"),
            ]),
            Sexp::List(vec![util::make_str_atom("get-proof")]),
            Sexp::List(vec![util::make_str_atom("get-unsat-assumptions")]),
            Sexp::List(vec![util::make_str_atom("get-unsat-core")]),
            Sexp::List(vec![util::make_str_atom("push"), util::make_int_atom(1)]),
            Sexp::List(vec![util::make_str_atom("pop"), util::make_int_atom(1)]),
            Sexp::List(vec![util::make_str_atom("reset")]),
            Sexp::List(vec![util::make_str_atom("reset-assertions")]),
            Sexp::List(vec![util::make_str_atom("exit")]),
        ],
        r#"sat
unsat
some message here!
(true)
(:version "4.8.5")
((proof
(unit-resolution (asserted B) (asserted (not B)) false)))

(B (not B))
(B (not B))
"#,
    );
}
