#[derive(Copy, Clone, Debug)]
pub enum ErrorKind {
    UnexpectedEndOfName,
    UnexpectedCharacter,
    InvalidIdentifier,
    InvalidWordSubstIndex,
    InvalidRepeatCountNumber,
    InvalidOperator,
    IntegerOverflow,
    InvalidIndexMangling,
    UnexpectedNodeKind,
    MissingNode,
    MissingChildNode,
    InvalidStandardSubst,
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    message: String,
}

impl Error {
    pub fn new(kind: ErrorKind, message: String) -> Error {
        Error {
            kind,
            message
        }
    }
}
