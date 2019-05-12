mod logic;
mod program;
mod format;

fn debug_print_check<T: std::fmt::Debug>(x: T, s: &str) {
    assert_eq!(format!("{:?}", x), s.to_string());
}



