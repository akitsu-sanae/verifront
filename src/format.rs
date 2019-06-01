pub mod smtlib2;

pub trait Format<T>: Sized {
    type PrintError: std::error::Error;
    type ParseError: std::error::Error;

    fn print(t: &Self) -> Result<T, Self::PrintError>;
    fn parse(t: &T) -> Result<Self, Self::ParseError>;
}
