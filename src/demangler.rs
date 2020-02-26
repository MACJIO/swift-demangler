use std::slice::Iter;

use super::node::{Node, Kind, Payload};

pub enum ErrorKind {
    UnexpectedEndOfName,
    InvalidOperator(char),
}

pub struct Error {
    kind: ErrorKind,
    position: usize,
}

pub struct Demangler<'a> {
    buffer: &'a [u8],
    position: usize,
    address: usize,
}

impl Demangler<'_> {
    fn next_char(&mut self) -> Option<u8> {
        let buffer = self.buffer;
        let position = self.position;
        if position < buffer.len() {
            self.position = position + 1;
            Some(buffer[position])
        } else {
            None
        }
    }

    fn peek_char(&self) -> Option<u8> {
        let buffer = self.buffer;
        let position = self.position;
        if position < buffer.len() {
            Some(buffer[position])
        } else {
            None
        }
    }

    fn next_char_skip_padding(&mut self) -> Option<u8> {
        let mut c = self.next_char();
        while match c {
            Some(v) => v == 0xff,
            None => false,
        } {
            c = self.next_char()
        }
        c
    }

    fn next_if(&mut self, c: u8) -> bool {
        if c != self.peek_char().unwrap() {
            false
        } else {
            self.position = self.position + 1;
            true
        }
    }

    /// Checks if c is an ASCII digit.
    fn is_digit(c: u8) -> bool {
        let c = c as char;
        ('0'..'9').contains(&c)
    }

    /// Checks if c is a lowercase ASCII letter.
    fn is_lower_letter(c: u8) -> bool {
        let c = c as char;
        ('a'..'z').contains(&c)
    }

    /// Checks if c is an uppercase ASCII letter.
    fn is_upper_letter(c: u8) -> bool {
        let c = c as char;
        ('A'..'Z').contains(&c)
    }

    /// Checks if c is an ASCII letter.
    fn is_letter(c: u8) -> bool {
        let c = c as char;
        ('a'..'z').contains(&c) || ('A'..'Z').contains(&c)
    }

    fn demangle_identifier(&mut self) -> Result<Node, Error> {
        unimplemented!()
    }
}