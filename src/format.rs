pub mod smtlib2;

pub trait Format<T1, T2> {
    type PrintError: std::error::Error;
    type ParseError: std::error::Error;

    fn print(t: &T1) -> Result<T2, Self::PrintError>;
    fn parse(t: &T2) -> Result<T1, Self::ParseError>;
}


