use std::rc::Rc;

use crate::node::{self, kind, Node, Kind, Payload};
use crate::punycode;
use crate::util;

#[derive(Copy, Clone, Debug)]
pub enum ErrorKind {
    UnexpectedEndOfName,
    UnexpectedCharacter,
    InvalidIdentifier,
    InvalidWordSubstIndex,
    InvalidOperator,
    IntegerOverflow,
    InvalidIndexMangling,
    UnexpectedNodeKind,
    MissingNode,
    MissingChildNode,
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    message: String,
    position: usize,
}

impl Error {
    pub fn new(kind: ErrorKind, message: String, position: usize) -> Error {
        Error {
            kind,
            message,
            position
        }
    }
}

pub struct Demangler<'a> {
    buffer: &'a [u8],
    position: usize,
    address: usize,
    node_stack: Vec<Rc<Node>>,
    words: Vec<String>,
    substitutions: Vec<Rc<Node>>,
}

impl Demangler<'_> {
    const MAX_WORD_SUBSTS: usize = 26;

    pub fn new(buffer: &[u8], address: usize) -> Demangler {
        Demangler {
            buffer,
            position: 0,
            address,
            node_stack: Vec::new(),
            words: Vec::new(),
            substitutions: Vec::new()
        }
    }

    pub fn next_char(&mut self) -> Option<u8> {
        let buffer = self.buffer;
        let position = self.position;
        if position < buffer.len() {
            self.position = position + 1;
            Some(buffer[position])
        } else {
            None
        }
    }

    pub fn peek_char(&self) -> Option<u8> {
        let buffer = self.buffer;
        let position = self.position;
        if position < buffer.len() {
            Some(buffer[position])
        } else {
            None
        }
    }

    pub fn next_char_skip_padding(&mut self) -> Option<u8> {
        let mut c = self.next_char();
        while match c {
            Some(v) => v == 0xff,
            None => false,
        } {
            c = self.next_char()
        }
        c
    }

    pub fn next_if(&mut self, c: u8) -> Option<bool> {
        if let Some(v) = self.peek_char() {
            let eq = v == c;
            if eq {
                self.position += 1;
            }
            Some(eq)
        } else {
            None
        }
    }

    /// Moves current position one character back.
    ///
    /// # Panics
    /// In case current position is 0 this method will panic.
    pub fn push_back(&mut self) {
        // THIS IS AN EXPECTED PANIC
        assert!(self.position > 0);
        self.position -= 1;
    }

    /// Pushes a node onto the node stack.
    pub fn push_node(&mut self, node: Rc<Node>) {
        self.node_stack.push(node)
    }

    /// Pops a node from the node stack.
    pub fn pop_node(&mut self) -> Option<Rc<Node>> {
        self.node_stack.pop()
    }

    /// Pops a node from the node stack if the last node has a specific kind.
    pub fn pop_node_of_kind(&mut self, kind: Kind) -> Result<Rc<Node>, Error> {
        if let Some(node) = self.node_stack.last() {
            if kind == node.kind() {
                Ok(self.node_stack.pop().unwrap())
            } else {
                Err(Error::new(
                    ErrorKind::UnexpectedNodeKind,
                    format!("Unexpected node kind."),
                    self.position)
                )
            }
        } else {
            Err(Error::new(
                ErrorKind::MissingNode,
                format!("Expected a node."),
                self.position
            ))
        }
    }

    pub fn pop_type_and_get_child(&mut self) -> Result<Rc<Node>, Error> {
        let ty = self.pop_node_of_kind(Kind::Type)?;

        if let Some(child) = ty.get_child(0) {
            Ok(child)
        } else {
            Err(Error::new(
                ErrorKind::MissingChildNode,
                "A Type node must have a child.".to_string(),
                self.position
            ))
        }
    }

    pub fn pop_type_and_get_any_generic(&mut self) -> Result<Rc<Node>, Error> {
        let child = self.pop_type_and_get_child()?;

        if kind::is_any_generic(child.kind()) {
            Ok(child)
        } else {
            Err(Error::new(
                ErrorKind::UnexpectedNodeKind,
                "Expected a node of any generic type.".to_string(),
                self.position
            ))
        }
    }

    pub fn char_or_err(&self, c: Option<u8>) -> Result<u8, Error> {
        if let Some(c) = c {
            Ok(c)
        } else {
            Err(Error::new(
                ErrorKind::UnexpectedEndOfName,
                format!("Expected a character at position {}.", self.position),
                self.position
            ))
        }
    }

    pub fn check_is_digit(&self, c: u8) -> Result<(), Error> {
        if util::is_digit(c as char) {
            Ok(())
        } else {
            Err(Error::new(
                ErrorKind::UnexpectedCharacter,
                format!(
                    "Unexpected character {} at position {}, expected a digit.",
                    c as char, self.position
                ),
                self.position
            ))
        }
    }

    pub fn demangle_natural(&mut self) -> Result<u32, Error> {
        let c = self.char_or_err(self.peek_char())?;
        self.check_is_digit(c)?;

        let mut num = 0u32;
        loop {
            if let Some(c) = self.peek_char() {
                if util::is_digit(c as char) {
                    self.position += 1;

                    // An integer overflow will cause a panic, so better check for it.
                    num = num.checked_mul(10)
                        .and_then(|n| n.checked_add(c as u32 - '0' as u32))
                        .ok_or_else(|| Error::new(
                            ErrorKind::IntegerOverflow,
                            format!(
                                "An integer overflow occurred while reading natural at position {}.",
                                self.position
                            ),
                            self.position
                        ))?;
                    continue
                }
            }

            break Ok(num)
        }
    }

    /// Demangles an index.
    /// TODO: proper docs
    pub fn demangle_index(&mut self) -> Result<u32, Error> {
        if let Some(b) = self.next_if('_' as u8) {
            if b {
                return Ok(0);
            }

            let num = self.demangle_natural()?;

            if let Some(true) = self.next_if('_' as u8) {
                Ok(num.checked_add(1).ok_or_else(|| {
                    Error::new(
                        ErrorKind::IntegerOverflow,
                        format!("An integer overflow occurred while demangling an index."),
                        self.position
                    )
                })?)
            } else {
                Err(Error::new(
                    ErrorKind::InvalidIndexMangling,
                    format!("Expected an underscore at the end of an index mangling."),
                    self.position
                ))
            }
        } else {
            return Err(Error::new(
                ErrorKind::UnexpectedEndOfName,
                format!("Expected an index at position {}.", self.position),
                self.position
            ))
        }
    }

    /// Demangles a word substitution at current position. Returns a tuple, where the first element
    /// is a substituted word and the second is a boolean indicating whether this substitution was
    /// the last one. This method expects that next character is available and is an ASCII letter.
    ///
    /// # Panics
    /// In case no more characters are available or the next character is not an ASCII letter this
    /// method will panic.
    pub fn demangle_word_subst(&mut self) -> Result<(&str, bool), Error> {
        let c = self.next_char().unwrap();
        let mut has_word_subst = true;

        let word_idx = if util::is_lower_letter(c as char) {
            c - 'a' as u8
        } else {
            assert!(util::is_upper_letter(c as char));
            has_word_subst = false;
            c - 'A' as u8
        } as usize;

        if word_idx < self.words.len() && word_idx <= Demangler::MAX_WORD_SUBSTS {
            Ok((&self.words[word_idx], has_word_subst))
        } else {
            Err(Error::new(
                ErrorKind::InvalidWordSubstIndex,
                format!("Invalid word index {} at position {}.", word_idx, self.position),
                self.position
            ))
        }
    }

    /// Demangles an identifier at current position and returns an identifier node.
    pub fn demangle_identifier(&mut self) -> Result<Rc<Node>, Error> {
        let c = self.char_or_err(self.peek_char())?;
        self.check_is_digit(c)?;

        let mut has_word_subst = false;
        let mut is_punycoded = false;

        // check if identifier has word substitutions or is punycoded
        if c as char == '0' {
            // if we're able to peek a char, we should be able to advance forward
            self.next_char().unwrap();

            if self.char_or_err(self.peek_char())? as char == '0' {
                self.next_char().unwrap();
                is_punycoded = true;
            } else {
                has_word_subst = true;
            }
        }

        let mut identifier = String::new();
        loop {
            while has_word_subst {
                if util::is_letter(self.char_or_err(self.peek_char())? as char) {
                    let (part, has_more) = self.demangle_word_subst()?;
                    has_word_subst = has_more;
                    identifier += part;
                } else {
                    break;
                }
            }

            if let Some(true) = self.next_if('0' as u8) {
                break;
            }

            let num_chars = self.demangle_natural()? as usize;
            if is_punycoded {
                self.next_if('_' as u8);
            }

            let chars_avail = self.buffer.len() - self.position;
            if chars_avail < num_chars {
                return Err(Error::new(
                    ErrorKind::UnexpectedEndOfName,
                    format!(
                        "Expected {} characters at position {}, found {}.",
                        num_chars, self.position, chars_avail
                    ),
                    self.position
                ))
            }

            // read raw chars
            let slice = Vec::from(&self.buffer[self.position..self.position + num_chars]);
            let string = String::from_utf8(slice).or_else(|_| {
                Err(Error::new(
                    ErrorKind::InvalidIdentifier,
                    format!("Invalid identifier at position {}.", self.position),
                    self.position
                ))
            })?;

            if is_punycoded {
                identifier += &punycode::decode(&string).or_else(|_| {
                    Err(Error::new(
                        ErrorKind::InvalidIdentifier,
                        format!("Failed to decode punycode at position {}.", self.position),
                        self.position
                    ))
                })?;
            } else {
                identifier += &string;

                // Find all words in current part and add them to the word substitutions table.
                let mut in_word = false;
                let mut prev_char = '\x00';
                let mut word = String::new(); // init word as an empty string
                for c in string.chars() {
                    if in_word {
                        word.push(c);

                        if util::is_word_end(c, prev_char) {
                            if word.len() >= 2 && self.words.len() < Demangler::MAX_WORD_SUBSTS {
                                self.words.push(word);
                                word = String::new();
                            }
                            in_word = false;
                        }
                    } else if util::is_word_start(c) {
                        in_word = true;
                        word.push(c);
                    }

                    prev_char = c;
                }

                // handle the last word if any
                if in_word && word.len() >= 2 && self.words.len() < Demangler::MAX_WORD_SUBSTS {
                    self.words.push(word);
                }
            }

            self.position += num_chars;

            if !has_word_subst {
                break;
            }
        }

        if identifier.is_empty() {
            Err(Error::new(
                ErrorKind::InvalidIdentifier,
                format!("Empty identifier at position {}.", self.position),
                self.position
            ))
        } else {
            let node_ref = Rc::new(Node::new(
                Kind::Identifier,
                Payload::Text(identifier)
            ));

            // add a substitution
            self.substitutions.push(node_ref.clone());

            Ok(node_ref)
        }
    }

    #[cfg(test)]
    pub fn add_word_subst(&mut self, word: String) {
        self.words.push(word)
    }

    #[cfg(test)]
    pub fn words(&mut self) -> &Vec<String> {
        &self.words
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::node::Payload;

    fn make_demangler(buffer: &[u8]) -> Demangler {
        Demangler::new(buffer, 0)
    }

    #[test]
    fn test_next_char() {
        let mut dem = make_demangler(b"abc");

        assert_eq!(dem.next_char().unwrap(), 'a' as u8);
        assert_eq!(dem.next_char().unwrap(), 'b' as u8);
        assert_eq!(dem.next_char().unwrap(), 'c' as u8);
        assert!(dem.next_char().is_none());
    }

    #[test]
    fn test_peek_char() {
        let mut dem = make_demangler(b"abc");

        assert_eq!(dem.peek_char().unwrap(), 'a' as u8);
        assert_eq!(dem.peek_char().unwrap(), 'a' as u8);

        let mut dem = make_demangler(b"");

        assert!(dem.peek_char().is_none());
    }

    #[test]
    fn test_next_char_skip_padding() {
        let mut dem = make_demangler(b"a\xFF\xFF\xFFb");

        assert_eq!(dem.next_char_skip_padding().unwrap(), 'a' as u8);
        assert_eq!(dem.next_char_skip_padding().unwrap(), 'b' as u8);
        assert!(dem.next_char_skip_padding().is_none());

        let mut dem = make_demangler(b"\xFF\xFF");

        assert!(dem.next_char_skip_padding().is_none());
    }

    #[test]
    fn test_next_if() {
        let mut dem = make_demangler(b"abc");

        assert_eq!(dem.next_if('a' as u8).unwrap(), true);
        assert_eq!(dem.next_if('g' as u8).unwrap(), false);
        assert_eq!(dem.next_if('b' as u8).unwrap(), true);
        assert_eq!(dem.next_if('c' as u8).unwrap(), true);
        assert!(dem.next_if('c' as u8).is_none());
    }

    #[test]
    #[should_panic]
    fn test_push_back_when_empty() {
        make_demangler(b"").push_back()
    }

    #[test]
    fn test_push_back() {
        let mut dem = make_demangler(b"abc");

        dem.next_char().unwrap();
        dem.next_char().unwrap();
        dem.push_back();
        dem.push_back();

        assert_eq!(dem.peek_char().unwrap(), 'a' as u8);
    }

    #[test]
    fn test_push_pop_node() {
        let mut dem = make_demangler(b"");
        let node = Rc::new(Node::new(Kind::Allocator, Payload::None));

        dem.push_node(node);
        dem.pop_node().unwrap();
    }

    #[test]
    fn test_pop_node_of_kind() {
        let mut dem = make_demangler(b"");
        let node = Rc::new(Node::new(Kind::Allocator, Payload::None));

        dem.push_node(node);
        assert!(dem.pop_node_of_kind(Kind::Allocator).is_ok());
    }

    #[test]
    fn test_pop_type_and_get_child() {
        let mut dem = make_demangler(b"");
        let child = Rc::new(Node::new(Kind::Type, Payload::None));
        let node = Rc::new(Node::new(Kind::Type, Payload::Children(vec![child])));

        dem.push_node(node);

        assert!(dem.pop_type_and_get_child().is_ok());
    }

    #[test]
    fn test_pop_type_and_get_any_generic() {
        let mut dem = make_demangler(b"");
        let child = Rc::new(Node::new(Kind::Structure, Payload::None));
        let node = Rc::new(Node::new(Kind::Type, Payload::Children(vec![child])));

        dem.push_node(node);

        assert!(dem.pop_type_and_get_child().is_ok());
    }

    #[test]
    #[should_panic]
    fn test_pop_type_and_get_any_generic_fail() {
        let mut dem = make_demangler(b"");
        let child = Rc::new(Node::new(Kind::Allocator, Payload::None));
        let node = Rc::new(Node::new(Kind::Type, Payload::Children(vec![child])));

        dem.push_node(node);

        assert!(dem.pop_type_and_get_any_generic().is_ok());
    }

    #[test]
    fn test_demangle_natural() {
        // check that demangle_number() doesn't advance after the number
        let mut dem = make_demangler(b"123a");
        assert_eq!(dem.demangle_natural().ok().unwrap(), 123);
        assert!(dem.peek_char().is_some());

        let mut dem = make_demangler(b"123abc");

        assert_eq!(dem.demangle_natural().ok().unwrap(), 123);
        assert!(dem.demangle_natural().is_err());

        let mut dem = make_demangler(b"abc123");

        assert!(dem.demangle_natural().is_err());
        dem.next_char();
        dem.next_char();
        dem.next_char();
        assert_eq!(dem.demangle_natural().ok().unwrap(), 123);
    }

    #[test]
    fn test_demangle_word_subst() {
        let mut dem = make_demangler(b"adbCF");

        dem.add_word_subst("Abc".to_string());
        dem.add_word_subst("Foo".to_string());
        dem.add_word_subst("Bar".to_string());
        dem.add_word_subst("Baz".to_string());

        let (part, more) = dem.demangle_word_subst().ok().unwrap();
        assert_eq!("Abc", part);
        assert!(more);

        let (part, more) = dem.demangle_word_subst().ok().unwrap();
        assert_eq!("Baz", part);
        assert!(more);

        let (part, more) = dem.demangle_word_subst().ok().unwrap();
        assert_eq!("Foo", part);
        assert!(more);

        let (part, more) = dem.demangle_word_subst().ok().unwrap();
        assert_eq!("Bar", part);
        assert!(!more);

        // F stands for index 5, which is OOB
        assert!(dem.demangle_word_subst().is_err());
    }

    #[test]
    #[should_panic]
    fn test_demangle_word_subst_panic_on_digit() {
        make_demangler(b"123").demangle_word_subst();
    }

    #[test]
    #[should_panic]
    fn test_demangle_word_subst_panic_on_eof() {
        make_demangler(b"").demangle_word_subst();
    }

    #[test]
    fn test_demangle_identifier() {
        let mut dem = make_demangler(b"1a");

        let node = dem.demangle_identifier().unwrap_or_else(|e| {
            panic!("{:?}", e);
        });

        assert_eq!(node.kind(), Kind::Identifier);

        if let Payload::Text(t) = node.payload() {
            assert_eq!(t, "a");
        } else {
            panic!("Invalid payload kind.")
        }
    }
}