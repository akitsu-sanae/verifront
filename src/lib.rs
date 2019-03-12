#![feature(box_syntax)]
#![feature(type_alias_enum_variants)]

mod logic;
mod theory;

#[cfg(test)]
mod tests {
    #[test]
    fn theory() {
        use crate::logic::{self, DefaultPropos, DefaultFOL};
        type Propos = DefaultPropos;
        use crate::theory;
        fn debug_print_check<T: std::fmt::Debug>(x: T, s: &str) {
            assert_eq!(format!("{:?}", x), s.to_string());
        }
        debug_print_check(
            Propos::Var(logic::make_ident("A")),
            r#"Var("A")"#);

        use crate::theory::core::FunctionSymbol::*;
        debug_print_check( // A and (B and C)
            Propos::Apply(
                Not,
                vec![
                Propos::Apply(
                    And,
                    vec![
                    Propos::Var(logic::make_ident("A")),
                    Propos::Apply(
                        And,
                        vec![
                        Propos::Var(logic::make_ident("B")),
                        Propos::Var(logic::make_ident("C")),
                        ]),
                    ])
                ]),
            r#"Apply(Not, [Apply(And, [Var("A"), Apply(And, [Var("B"), Var("C")])])])"#);

        type FOL = DefaultFOL;
        use crate::logic::Quantifier;
        debug_print_check( // forall a. a = true
            FOL::Binding(
                Quantifier::Forall,
                vec![logic::make_ident("a")],
                box FOL::Apply(
                    Equal,
                    vec![
                    FOL::Var(logic::make_ident("a")),
                    FOL::Apply(True, vec![])
                    ])),
            r#"Binding(Forall, ["a"], Apply(Equal, [Var("a"), Apply(True, [])]))"#);
    }
}
