mod format;
mod logic;
mod sortcheck;
mod program;

fn debug_print_check<T: std::fmt::Debug>(x: T, s: &str) {
    assert_eq!(format!("{:?}", x), s.to_string());
}
