use std::error;
use std::fmt;
use std::io;
use std::path::Path;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnexpectedError {
    pub line: i32,
    pub column: i32,
    pub expected: Vec<String>,
    pub found: String,
}
impl fmt::Display for UnexpectedError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Unexpected Token error at ({}, {}): expected one of {:?}, found {}", self.line, self.column, self.expected, self.found)
    }
}

impl error::Error for UnexpectedError {
    fn description(&self) -> &str { "unexpected token error in parsing" }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncompleteError {
    msg: String,
}

impl fmt::Display for IncompleteError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "incomplete error: {}", self.msg)
    }
}

impl error::Error for IncompleteError {
    fn description(&self) -> &str { "incomplete error in parsing" }
}

#[derive(Debug)]
pub enum ParseError {
    Io(io::Error),
    Unexpected(UnexpectedError),
    Incomplete(IncompleteError),
    Other(Box<dyn error::Error>),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseError::*;
        match *self {
            Io(ref err) => write!(f, "io error: {}", err),
            Unexpected(ref err) => write!(f, "{}", err),
            Incomplete(ref err) => write!(f, "{}", err),
            Other(ref err) => write!(f, "{}", err),
        }
    }
}

impl error::Error for ParseError {
    fn description(&self) -> &str {
        use ParseError::*;
        match *self {
            Io(ref err) => err.description(),
            Unexpected(ref err) => err.description(),
            Incomplete(ref err) => err.description(),
            Other(ref err) => err.description(),
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        use ParseError::*;
        match *self {
            Io(ref err) => Some(err),
            Unexpected(ref err) => Some(err),
            Incomplete(ref err) => Some(err),
            Other(ref err) => Some(err.as_ref()),
        }
    }
}

impl From<io::Error> for ParseError {
    fn from(err: io::Error) -> Self { ParseError::Io(err) }
}
impl From<UnexpectedError> for ParseError {
    fn from(err: UnexpectedError) -> Self { ParseError::Unexpected(err) }
}
impl From<IncompleteError> for ParseError {
    fn from(err: IncompleteError) -> Self { ParseError::Incomplete(err) }
}

pub trait Parsable {
    fn parse(content: &str) -> Result<Box<Self>, ParseError>;
    fn from_file(path: &Path) -> Result<Box<Self>, ParseError> {
        match std::fs::read_to_string(path) {
            Ok(content) => Self::parse(&content),
            Err(err) => Err(ParseError::Io(err)),
        }
    }
}


