#![feature(box_syntax)]
#![feature(type_alias_enum_variants)]

pub mod logic;
pub mod theory;
pub mod ident;

#[cfg(test)]
mod tests {
    #[test]
    fn theory() {
        use crate::logic::{DefaultPropos, DefaultFOL};
        use crate::ident;
        type Propos = DefaultPropos;
        fn debug_print_check<T: std::fmt::Debug>(x: T, s: &str) {
            assert_eq!(format!("{:?}", x), s.to_string());
        }
        debug_print_check(
            Propos::Var(ident::make("A")),
            r#"Var("A")"#);

        use crate::theory::core::FunctionSymbol::*;
        debug_print_check( // A and (B and C)
            Propos::Apply(
                Not,
                vec![
                Propos::Apply(
                    And,
                    vec![
                    Propos::Var(ident::make("A")),
                    Propos::Apply(
                        And,
                        vec![
                        Propos::Var(ident::make("B")),
                        Propos::Var(ident::make("C")),
                        ]),
                    ])
                ]),
            r#"Apply(Not, [Apply(And, [Var("A"), Apply(And, [Var("B"), Var("C")])])])"#);

        type FOL = DefaultFOL;
        use crate::logic::Quantifier;
        debug_print_check( // forall a. a = true
            FOL::Binding(
                Quantifier::Forall,
                vec![ident::make("a")],
                box FOL::Apply(
                    Equal,
                    vec![
                    FOL::Var(ident::make("a")),
                    FOL::Apply(True, vec![])
                    ])),
            r#"Binding(Forall, ["a"], Apply(Equal, [Var("a"), Apply(True, [])]))"#);
    }
}
