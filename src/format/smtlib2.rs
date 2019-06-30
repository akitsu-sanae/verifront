use super::Format;
use crate::ident::Ident;
use crate::logic::{binder::*, expr::*, theory::*};
use sexp::Sexp;

mod print;
pub use print::PrintError;

mod parse;
pub use parse::ParseError;

pub trait Smtlib2Theory: Theory {
    fn sexp_of_sort_symbol(ss: &Self::SortSymbol) -> Result<Sexp, PrintError>;
    fn sexp_of_function_symbol(fs: &Self::FunctionSymbol) -> Result<Sexp, PrintError>;
    fn sexp_of_const_symbol(c: &Self::ConstSymbol) -> Result<Sexp, PrintError>;

    fn sort_symbol_of_sexp(expr: &Sexp) -> Result<Self::SortSymbol, ParseError>;
    fn function_symbol_of_sexp(expr: &Sexp) -> Result<Self::FunctionSymbol, ParseError>;
    fn const_symbol_of_sexp(expr: &Sexp) -> Result<Self::ConstSymbol, ParseError>;
}

pub trait Smtlib2Binder: IsBinder + Sized {
    fn sexp_of_binder(&self) -> Result<Sexp, PrintError>;
    fn binder_of_sexp(expr: &Sexp) -> Result<Self, ParseError>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorDec<SS: IsSortSymbol> {
    pub name: Ident,
    pub sort: Sort<SS>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstructorDec<SS: IsSortSymbol> {
    pub name: Ident,
    pub selector_decs: Vec<SelectorDec<SS>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DatatypeDec<SS: IsSortSymbol> {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub constructor_decs: Vec<ConstructorDec<SS>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunDec<T: Smtlib2Theory> {
    pub name: Ident,
    pub params: Vec<Sort<T::SortSymbol>>,
    pub ret: Sort<T::SortSymbol>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunDef<T: Smtlib2Theory, B: Smtlib2Binder> {
    pub name: Ident,
    pub params: Vec<SortedSymbol<T::SortSymbol>>,
    pub ret: Sort<T::SortSymbol>,
    pub body: Expr<T, B>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttributeValue {
    SpecConstant(String), // FIXME
    Symbol(Ident),
    // Sexp(Sexp), NOTE: no Eq implemention for Sexp
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute {
    Keyword(String),
    KeywordWithAttributeValue(String, AttributeValue),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Option {
    DiagnosticOutputChannel(String),
    GlobalDeclarations(bool),
    InteractiveMode(bool),
    PrintSuccess(bool),
    ProduceAssertions(bool),
    ProduceAssignments(bool),
    ProduceModels(bool),
    ProduceProofs(bool),
    ProduceUnsatAssumptions(bool),
    ProduceUnsatCores(bool),
    RandomSeed(i64),
    RegularOutputChannel(String),
    ReproducibleResourceLimit(i64),
    Verbosity(i64),
    Attribute(Attribute),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InfoFlag {
    AllStatistics,
    AssertionStackLevels,
    Authors,
    ErrorBehavior,
    Name,
    ReasonUnknown,
    Version,
    Keyword(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command<T: Smtlib2Theory, B: Smtlib2Binder> {
    // version 2.6
    Assert(Expr<T, B>),
    CheckSat,
    CheckSatAssuming(Vec<Ident>, Vec<Ident>), // positives and negativs
    DeclareConst(SortedSymbol<T::SortSymbol>),
    DeclareDatatype(DatatypeDec<T::SortSymbol>),
    DeclareDatatypes(Vec<Ident>, Vec<DatatypeDec<T::SortSymbol>>),
    DeclareFun(FunDec<T>),
    DeclareSort(Ident, i64),
    DefineFun(FunDef<T, B>),
    DefineFunRec(FunDef<T, B>),
    DefineFunsRec(Vec<FunDef<T, B>>),
    DefineSort(Ident, Vec<Ident>, Sort<T::SortSymbol>),
    Echo(String),
    Exit,
    GetAssertions,
    GetAssignment,
    GetInfo(InfoFlag),
    GetModel,
    GetOption(String),
    GetProof,
    GetUnsatAssumptions,
    GetUnsatCore,
    GetValue(Vec<Expr<T, B>>),
    Pop(i64),
    Push(i64),
    Reset,
    ResetAssertions,
    SetInfo(Attribute),
    SetLogic(String),
    SetOption(Option),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Smtlib2<T: Smtlib2Theory, B: Smtlib2Binder> {
    pub commands: Vec<Command<T, B>>,
}

impl<T: Smtlib2Theory, B: Smtlib2Binder> Smtlib2<T, B> {
    pub fn new(cmds: Vec<Command<T, B>>) -> Self {
        Self { commands: cmds }
    }
}

impl<T, B> Format<Vec<Sexp>> for Smtlib2<T, B>
where
    T: Smtlib2Theory,
    B: Smtlib2Binder,
{
    type PrintError = PrintError;
    type ParseError = ParseError;

    fn print(expr: &Smtlib2<T, B>) -> Result<Vec<Sexp>, PrintError> {
        print::toplevels(expr)
    }

    fn parse(toplevels: &Vec<Sexp>) -> Result<Smtlib2<T, B>, ParseError> {
        parse::toplevels(toplevels)
    }
}
