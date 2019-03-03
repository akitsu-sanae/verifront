 #![feature(box_syntax)]
#![feature(type_alias_enum_variants)]

mod logic;
mod ident;
mod theory;

#[cfg(test)]
mod tests {
    #[test]
    fn theory() {
        use crate::logic::*;
        use crate::ident;
        fn debug_print_check<T: std::fmt::Debug>(x: T, s: &str) {
            assert_eq!(format!("{:?}", x), s.to_string());
        }
        debug_print_check(
            Propos::Atom(ident::make("A")),
            r#"Atom("A")"#);
        debug_print_check(
            Propos::make_neg(
                box Propos::make_and(
                    box Propos::make_atom("A"),
                    box Propos::make_or(
                        box Propos::make_atom("B"),
                        box Propos::make_atom("C")))),
            r#"UnaryOp(Neg, BinaryOp(And, Atom("A"), BinaryOp(Or, Atom("B"), Atom("C"))))"#);

        use crate::theory::boolean::{Function, Predicate, Boolean};
        type BFOL = FOL<Boolean>;
        debug_print_check( // forall a. a = true
            BFOL::Quantified(
                Quantifier::Forall,
                vec![ident::make("a")],
                box BFOL::Apply(
                    Predicate::Eq,
                    vec![
                        Term::Var (ident::make("a")),
                        Term::Apply (Function::True, vec![])
                    ])),
            r#"Quantified(Forall, ["a"], Apply(Eq, [Var("a"), Apply(True, [])]))"#);
    }
}
