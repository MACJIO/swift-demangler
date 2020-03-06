#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    UnexpectedEndOfName,
    UnexpectedCharacter,
    InvalidIdentifier,
    InvalidWordSubstIndex,
    InvalidSubstIndex,
    InvalidRepeatCountNumber,
    InvalidOperator,
    IntegerOverflow,
    InvalidIndexMangling,
    UnexpectedNodeKind,
    UnexpectedNodePayload,
    MissingNode,
    MissingChildNode,
    InvalidStandardSubst,
    InvalidBuiltinTypeSize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    message: String,
}

impl Error {
    pub fn new(kind: ErrorKind, message: String) -> Error {
        Error {
            kind,
            message
        }
    }

    pub fn kind(&self) -> ErrorKind {
        self.kind
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}
