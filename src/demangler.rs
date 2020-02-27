use super::node::{Node, Kind};

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
    node_stack: Vec<Node>,
}

impl Demangler<'_> {
    pub fn new(buffer: &[u8], address: usize) -> Demangler {
        Demangler {
            buffer,
            position: 0,
            address,
            node_stack: Vec::new()
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

    /// Checks if c is an ASCII digit.
    pub fn is_digit(c: u8) -> bool {
        let c = c as char;
        ('0'..'9').contains(&c)
    }

    /// Checks if c is a lowercase ASCII letter.
    pub fn is_lower_letter(c: u8) -> bool {
        let c = c as char;
        ('a'..'z').contains(&c)
    }

    /// Checks if c is an uppercase ASCII letter.
    pub fn is_upper_letter(c: u8) -> bool {
        let c = c as char;
        ('A'..'Z').contains(&c)
    }

    /// Checks if c is an ASCII letter.
    pub fn is_letter(c: u8) -> bool {
        let c = c as char;
        ('a'..'z').contains(&c) || ('A'..'Z').contains(&c)
    }

    /// Pushes a node onto the node stack.
    pub fn push_node(&mut self, node: Node) {
        self.node_stack.push(node)
    }

    /// Pops a node from the node stack.
    pub fn pop_node(&mut self) -> Option<Node> {
        self.node_stack.pop()
    }

    /// Pops a node from the node stack if the last node has a specific kind.
    pub fn pop_node_of_kind(&mut self, kind: Kind) -> Option<Node> {
        if let Some(node) = self.node_stack.last() {
            if kind == node.kind() {
                self.node_stack.pop()
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn demangle_identifier(&mut self) -> Result<Node, Error> {
        unimplemented!()
    }
}