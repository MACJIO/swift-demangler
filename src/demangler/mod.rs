use std::rc::Rc;

use crate::node::{kind, Node, Kind, Payload};
use crate::punycode;
use crate::util;
use crate::error::{Error, ErrorKind};
use crate::node::kind::{is_context, is_decl_name};

pub mod cache;
use cache::NodeCache;

#[cfg(test)]
mod tests;

const STDLIB_NAME: &str = "Swift";
const MANGLING_MODULE_OBJC: &str = "__C";
const MANGLING_MODULE_CLANG_IMPORTER: &str = "__C_Synthesized";

const BUILTIN_TYPE_NAME_BRIDGEOBJECT: &str = "Builtin.BridgeObject";
const BUILTIN_TYPE_NAME_UNSAFEVALUEBUFFER: &str = "Builtin.UnsafeValueBuffer";
const BUILTIN_TYPE_NAME_FLOAT: &str = "Builtin.FPIEEE";
const BUILTIN_TYPE_NAME_INT: &str = "Builtin.Int";
const BUILTIN_TYPE_NAME_INTLITERAL: &str = "Builtin.IntLiteral";
const BUILTIN_TYPE_NAME_PREFIX: &str = "Builtin.";
const BUILTIN_TYPE_NAME_VEC: &str = "Builtin.Vec";
const BUILTIN_TYPE_NAME_UNKNOWNOBJECT: &str = "Builtin.UnknownObject";
const BUILTIN_TYPE_NAME_NATIVEOBJECT: &str = "Builtin.NativeObject";
const BUILTIN_TYPE_NAME_RAWPOINTER: &str = "Builtin.RawPointer";
const BUILTIN_TYPE_NAME_SILTOKEN: &str = "Builtin.SILToken";
const BUILTIN_TYPE_NAME_WORD: &str = "Builtin.Word";

fn create_standard_substitution(cache: &mut NodeCache, c: char) -> Option<Rc<Node>> {
    let node = match c {
        'A' => cache.create_swift_type(Kind::Structure, "AutoreleasingUnsafeMutablePointer".to_string()),
        'a' => cache.create_swift_type(Kind::Structure, "Array".to_string()),
        'b' => cache.create_swift_type(Kind::Structure, "Bool".to_string()),
        'D' => cache.create_swift_type(Kind::Structure, "Dictionary".to_string()),
        'd' => cache.create_swift_type(Kind::Structure, "Double".to_string()),
        'f' => cache.create_swift_type(Kind::Structure, "Float".to_string()),
        'h' => cache.create_swift_type(Kind::Structure, "Set".to_string()),
        'I' => cache.create_swift_type(Kind::Structure, "DefaultIndices".to_string()),
        'i' => cache.create_swift_type(Kind::Structure, "Int".to_string()),
        'J' => cache.create_swift_type(Kind::Structure, "Character".to_string()),
        'N' => cache.create_swift_type(Kind::Structure, "ClosedRange".to_string()),
        'n' => cache.create_swift_type(Kind::Structure, "Range".to_string()),
        'O' => cache.create_swift_type(Kind::Structure, "ObjectIdentifier".to_string()),
        'P' => cache.create_swift_type(Kind::Structure, "UnsafePointer".to_string()),
        'p' => cache.create_swift_type(Kind::Structure, "UnsafeMutablePointer".to_string()),
        'R' => cache.create_swift_type(Kind::Structure, "UnsafeBufferPointer".to_string()),
        'r' => cache.create_swift_type(Kind::Structure, "UnsafeMutableBufferPointer".to_string()),
        'S' => cache.create_swift_type(Kind::Structure, "String".to_string()),
        's' => cache.create_swift_type(Kind::Structure, "Substring".to_string()),
        'u' => cache.create_swift_type(Kind::Structure, "UInt".to_string()),
        'V' => cache.create_swift_type(Kind::Structure, "UnsafeRawPointer".to_string()),
        'v' => cache.create_swift_type(Kind::Structure, "UnsafeMutableRawPointer".to_string()),
        'W' => cache.create_swift_type(Kind::Structure, "UnsafeRawBufferPointer".to_string()),
        'w' => cache.create_swift_type(Kind::Structure, "UnsafeMutableRawBufferPointer".to_string()),

        'q' => cache.create_swift_type(Kind::Enum, "Optional".to_string()),

        'B' => cache.create_swift_type(Kind::Protocol, "BinaryFloatingPoint".to_string()),
        'E' => cache.create_swift_type(Kind::Protocol, "Encodable".to_string()),
        'e' => cache.create_swift_type(Kind::Protocol, "Decodable".to_string()),
        'F' => cache.create_swift_type(Kind::Protocol, "FloatingPoint".to_string()),
        'G' => cache.create_swift_type(Kind::Protocol, "RandomNumberGenerator".to_string()),
        'H' => cache.create_swift_type(Kind::Protocol, "Hashable".to_string()),
        'j' => cache.create_swift_type(Kind::Protocol, "Numeric".to_string()),
        'K' => cache.create_swift_type(Kind::Protocol, "BidirectionalCollection".to_string()),
        'k' => cache.create_swift_type(Kind::Protocol, "RandomAccessCollection".to_string()),
        'L' => cache.create_swift_type(Kind::Protocol, "Comparable".to_string()),
        'l' => cache.create_swift_type(Kind::Protocol, "Collection".to_string()),
        'M' => cache.create_swift_type(Kind::Protocol, "MutableCollection".to_string()),
        'm' => cache.create_swift_type(Kind::Protocol, "RangeReplaceableCollection".to_string()),
        'Q' => cache.create_swift_type(Kind::Protocol, "Equatable".to_string()),
        'T' => cache.create_swift_type(Kind::Protocol, "Sequence".to_string()),
        't' => cache.create_swift_type(Kind::Protocol, "IteratorProtocol".to_string()),
        'U' => cache.create_swift_type(Kind::Protocol, "UnsignedInteger".to_string()),
        'X' => cache.create_swift_type(Kind::Protocol, "RangeExpression".to_string()),
        'x' => cache.create_swift_type(Kind::Protocol, "Strideable".to_string()),
        'Y' => cache.create_swift_type(Kind::Protocol, "RawRepresentable".to_string()),
        'y' => cache.create_swift_type(Kind::Protocol, "StringProtocol".to_string()),
        'Z' => cache.create_swift_type(Kind::Protocol, "SignedInteger".to_string()),
        'z' => cache.create_swift_type(Kind::Protocol, "BinaryInteger".to_string()),

        _ => return None
    };

    Some(node)
}

pub struct Demangler<'a> {
    buffer: &'a [u8],
    position: usize,
    address: usize,
    node_stack: Vec<Rc<Node>>,
    cache: NodeCache,
    words: Vec<String>,
    substitutions: Vec<Rc<Node>>,
}

impl Demangler<'_> {
    const MAX_WORD_SUBSTS: usize = 26;
    const MAX_REPEAT_COUNT: u32 = 2048;
    const MAX_BUILTIN_TYPE_SIZE: u32 = 4096;

    pub fn new(buffer: &[u8], address: usize) -> Demangler {
        Demangler {
            buffer,
            position: 0,
            address,
            node_stack: Vec::new(),
            cache: NodeCache::new(),
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

    pub fn add_subst(&mut self, subst: Rc<Node>) {
        self.substitutions.push(subst);
    }

    pub fn get_subst(&mut self, idx: usize) -> Result<Rc<Node>, Error> {
        self.substitutions.get(idx)
            .and_then(|r| Some(r.clone()))
            .ok_or_else(|| {
            Error::new(
                ErrorKind::InvalidSubstIndex,
                format!("Invalid substitution index {}", idx)
            )
        })
    }

    /// Pushes a node onto the node stack.
    pub fn push_node(&mut self, node: Rc<Node>) {
        self.node_stack.push(node)
    }

    /// Pops a node from the node stack.
    pub fn pop_node(&mut self) -> Result<Rc<Node>, Error> {
        self.node_stack.pop().ok_or_else(|| {
            Error::new(
                ErrorKind::MissingNode,
                "Expected a node.".to_string()
            )
        })
    }

    /// Pops a node from the node stack if the last node has a specific kind.
    pub fn pop_node_of_kind(&mut self, kind: Kind) -> Result<Rc<Node>, Error> {
        self.pop_node().and_then(|node| {
            if kind == node.kind() {
                Ok(node)
            } else {
                Err(Error::new(
                    ErrorKind::UnexpectedNodeKind,
                    format!("Unexpected node kind.")
                ))
            }
        })
    }

    pub fn pop_type_and_get_child(&mut self) -> Result<Rc<Node>, Error> {
        let ty = self.pop_node_of_kind(Kind::Type)?;

        if ty.num_children() == 1 {
            Ok(ty.get_child(0).unwrap())
        } else {
            // put the node back
            self.push_node(ty);

            Err(Error::new(
                ErrorKind::MissingChildNode,
                "A Type node must have exactly one child.".to_string()
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
                "Expected a node of any generic type.".to_string()
            ))
        }
    }

    pub fn pop_module(&mut self) -> Result<Rc<Node>, Error> {
        self.pop_node().and_then(|node| {
            if let Kind::Identifier = node.kind() {
                Ok(self.cache.create_node(Kind::Module, node.payload().clone()))
            } else if let Kind::Module = node.kind() {
                Ok(node)
            } else {
                // put the node back where it was if we're not going to use it
                self.push_node(node);

                Err(Error::new(
                    ErrorKind::UnexpectedNodeKind,
                    "Expected an Identifier or a Module node.".to_string()
                ))
            }
        })
    }

    pub fn pop_context(&mut self) -> Result<Rc<Node>, Error> {
        self.pop_module().or_else(|e| {
            if e.kind() == ErrorKind::UnexpectedNodeKind {
                let child = self.pop_type_and_get_child()
                    .or_else(|_| self.pop_node())?;

                // check that the node is a context node
                if is_context(child.kind()) {
                    Ok(child)
                } else {
                    // put the node back if we didn't want it
                    self.push_node(child);

                    Err(Error::new(
                        ErrorKind::UnexpectedNodeKind,
                        "Expected a context child node.".to_string()
                    ))
                }
            } else {
                Err(e)
            }
        })
    }

    pub fn char_or_err(&self, c: Option<u8>) -> Result<u8, Error> {
        if let Some(c) = c {
            Ok(c)
        } else {
            Err(Error::new(
                ErrorKind::UnexpectedEndOfName,
                format!("Expected a character at position {}.", self.position)
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
                )
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
                            )
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
                        format!("An integer overflow occurred while demangling an index.")
                    )
                })?)
            } else {
                Err(Error::new(
                    ErrorKind::InvalidIndexMangling,
                    format!("Expected an underscore at the end of an index mangling.")
                ))
            }
        } else {
            return Err(Error::new(
                ErrorKind::UnexpectedEndOfName,
                format!("Expected an index at position {}.", self.position)
            ))
        }
    }

    pub fn demangle_index_as_node(&mut self) -> Result<Rc<Node>, Error> {
        let index = self.demangle_index()? as u64;
        Ok(self.cache.create_index_node(Kind::Index, index))
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
                format!("Invalid word index {} at position {}.", word_idx, self.position)
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
                    )
                ))
            }

            // read raw chars
            let slice = Vec::from(&self.buffer[self.position..self.position + num_chars]);
            let string = String::from_utf8(slice).or_else(|_| {
                Err(Error::new(
                    ErrorKind::InvalidIdentifier,
                    format!("Invalid identifier at position {}.", self.position)
                ))
            })?;

            if is_punycoded {
                identifier += &punycode::decode(&string).or_else(|_| {
                    Err(Error::new(
                        ErrorKind::InvalidIdentifier,
                        format!("Failed to decode punycode at position {}.", self.position)
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
                format!("Empty identifier at position {}.", self.position)
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

    pub fn push_multi_substitutions(&mut self, repeat_count: u32, subst_idx: usize) -> Result<Rc<Node>, Error> {
        let node = self.substitutions.get(subst_idx).ok_or_else(|| {
            Error::new(
                ErrorKind::InvalidSubstIndex,
                format!("Invalid word index {} at position {}.", subst_idx, self.position)
            )
        })?.clone();

        for _ in 1..repeat_count {
            self.push_node(node.clone());
        }

        Ok(node)
    }

    pub fn demangle_multi_substitutions(&mut self) -> Result<Rc<Node>, Error> {
        loop {
            let mut c = self.next_char().ok_or_else(|| {
                Error::new(
                    ErrorKind::UnexpectedEndOfName,
                    format!("Expected a character at position {}.", self.position)
                )
            })? as char;

            let mut repeat_count: u32 = 1;
            if util::is_digit(c) {
                self.push_back();
                repeat_count = self.demangle_repeat_count()?;
                c = self.next_char().ok_or_else(||
                    Error::new(
                        ErrorKind::UnexpectedEndOfName,
                        format!("Expected a character at position {}.", self.position)
                    )
                )? as char;
            }

            let subst_idx: usize;
            let is_last: bool;
            if util::is_lower_letter(c) {
                subst_idx = c as usize - 'a' as usize;
                is_last = false;
            } else if util::is_upper_letter(c as char) {
                subst_idx = c as usize - 'A' as usize;
                is_last = true;
            } else if c == '_' {
                subst_idx = repeat_count as usize + 27;
                repeat_count = 1;
                is_last = true;
            } else {
                return Err(Error::new(
                    ErrorKind::UnexpectedCharacter,
                    "Unexpected character in substitution.".to_string()
                ))
            }

            let node = self.push_multi_substitutions(repeat_count, subst_idx)?;
            if is_last {
                break Ok(node)
            }
        }
    }

    pub fn demangle_repeat_count(&mut self) -> Result<u32, Error> {
        let repeat_count = self.demangle_natural()?;

        if repeat_count > Demangler::MAX_REPEAT_COUNT || repeat_count == 0 {
            Err(Error::new(
                ErrorKind::InvalidRepeatCountNumber,
                format!("Invalid repeat count {} at position {}.", repeat_count, self.position)
            ))
        } else {
            Ok(repeat_count)
        }
    }

    pub fn demangle_standard_substitution(&mut self) -> Result<Rc<Node>, Error> {
        let c = self.next_char().ok_or_else(|| {
            Error::new(
                ErrorKind::UnexpectedEndOfName,
                format!("Expected a standard substitution at position {}.", self.position)
            )
        })? as char;

        match c {
            'o' => Ok(self.cache.create_module_node(MANGLING_MODULE_OBJC.to_string())),
            'C' => Ok(self.cache.create_module_node(MANGLING_MODULE_CLANG_IMPORTER.to_string())),
            'g' => {
                let child = self.pop_node_of_kind(Kind::Type)?;
                let type_list = self.cache.create_node_with_child(Kind::TypeList, child);
                let children = vec![
                    self.cache.create_swift_type(Kind::Enum, "Optional".to_string()),
                    type_list
                ];
                let inner_ty = self.cache.create_node_with_children(Kind::BoundGenericEnum, children);
                let optional_ty = self.cache.create_type_node(inner_ty);
                self.substitutions.push(optional_ty.clone());
                Ok(optional_ty)
            },
            _ => {
                self.push_back();

                let repeat_count = if util::is_digit(c) {
                    self.demangle_repeat_count()?
                } else {
                    1
                };

                // next_char borrows self mutably, while char_or_err borrows self immutably,
                // so we need to call them separately here.
                let c = self.next_char();
                let c = self.char_or_err(c)? as char;
                let subst_node = create_standard_substitution(&mut self.cache, c)
                    .ok_or_else(|| {
                        Error::new(
                            ErrorKind::InvalidStandardSubst,
                            format!(
                                "Invalid standard substitution character {} at position {}",
                                c, self.position
                            )
                        )
                    })?;

                for _ in 1..repeat_count {
                    self.push_node(subst_node.clone())
                }

                Ok(subst_node)
            }
        }
    }

    pub fn demangle_any_generic_type(&mut self, kind: Kind) -> Result<Rc<Node>, Error> {
        let name = self.pop_node()?;
        if !is_decl_name(name.kind()) {
            self.push_node(name);
            return Err(Error::new(
                ErrorKind::UnexpectedNodeKind,
                "Expected a decl name node.".to_string()
            ));
        }

        let ctx = self.pop_context().or_else(|err| {
            self.push_node(name.clone());
            Err(err)
        })?;

        let ty = self.cache.create_node_with_children(kind, vec![ctx, name]);
        let node = self.cache.create_type_node(ty);
        self.substitutions.push(node.clone());

        Ok(node)
    }

    pub fn demangle_extension_context(&mut self) -> Result<Rc<Node>, Error> {
        let gen_sig = self.pop_node_of_kind(Kind::DependentGenericSignature);
        let module = self.pop_module()?;
        let ty = self.pop_type_and_get_any_generic()?;

        let children = if let Err(Error {kind: ErrorKind::UnexpectedNodeKind, ..}) = gen_sig {
            vec![module, ty]
        } else if let Ok(gen_sig) = gen_sig {
            vec![module, ty, gen_sig]
        } else {
            return gen_sig
        };

        Ok(self.cache.create_node_with_children(Kind::Extension, children))
    }

    pub fn demangle_builtin_type_size(&mut self) -> Result<u32, Error> {
        let size = self.demangle_index()? - 1;
        if size > Demangler::MAX_BUILTIN_TYPE_SIZE {
            Err(Error::new(
                ErrorKind::InvalidBuiltinTypeSize,
                format!("Invalid size {} at position {}.", size, self.position)
            ))
        } else {
            Ok(size)
        }
    }

    pub fn demangle_builtin_type(&mut self) -> Result<Rc<Node>, Error> {
        if let Some(c) = self.next_char() {
            let node = match c as char {
                'b' => self.cache.create_text_node(Kind::BuiltinTypeName, BUILTIN_TYPE_NAME_BRIDGEOBJECT.to_string()),
                'B' => self.cache.create_text_node(Kind::BuiltinTypeName, BUILTIN_TYPE_NAME_UNSAFEVALUEBUFFER.to_string()),
                'f' => {
                    let size = self.demangle_builtin_type_size()?;
                    self.cache.create_text_node(
                        Kind::BuiltinTypeName,
                        BUILTIN_TYPE_NAME_FLOAT.to_string() + &format!("{}", size)
                    )
                },
                'i' => {
                    let size = self.demangle_builtin_type_size()?;
                    self.cache.create_text_node(
                        Kind::BuiltinTypeName,
                        BUILTIN_TYPE_NAME_INT.to_string() + &format!("{}", size)
                    )
                },
                'I' => self.cache.create_text_node(Kind::BuiltinTypeName, BUILTIN_TYPE_NAME_INTLITERAL.to_string()),
                'v' => {
                    let elts = self.demangle_builtin_type_size()?;
                    let elt_type = self.pop_type_and_get_child()?;
                    if let Payload::Text(text) = elt_type.payload() {
                        if elt_type.kind() != Kind::BuiltinTypeName || !text.starts_with(BUILTIN_TYPE_NAME_PREFIX) {
                            return Err(Error::new(
                                ErrorKind::UnexpectedNodeKind,
                                format!("Expected builtin type node at position {}.", self.position)
                            ))
                        }
                        let name = BUILTIN_TYPE_NAME_VEC.to_string() +
                            &format!("{}", elts) +
                            "x" +
                            &text[BUILTIN_TYPE_NAME_PREFIX.len()..];
                        self.cache.create_text_node(Kind::BuiltinTypeName, name)
                    } else {
                        return Err(Error::new(
                            ErrorKind::UnexpectedNodePayload,
                            format!("Expected payload at position {}.", self.position)
                        ))
                    }
                },
                'O' => self.cache.create_text_node(Kind::BuiltinTypeName, BUILTIN_TYPE_NAME_UNKNOWNOBJECT.to_string()),
                'o' => self.cache.create_text_node(Kind::BuiltinTypeName, BUILTIN_TYPE_NAME_NATIVEOBJECT.to_string()),
                'p' => self.cache.create_text_node(Kind::BuiltinTypeName, BUILTIN_TYPE_NAME_RAWPOINTER.to_string()),
                't' => self.cache.create_text_node(Kind::BuiltinTypeName, BUILTIN_TYPE_NAME_SILTOKEN.to_string()),
                'w' => self.cache.create_text_node(Kind::BuiltinTypeName, BUILTIN_TYPE_NAME_WORD.to_string()),
                _ => return Err(Error::new(
                    ErrorKind::UnexpectedCharacter,
                    format!("Unexpected builtin type {} at position {}.", c, self.position)
                )),
            };

            Ok(self.cache.create_type_node(node))
        } else {
            Err(Error::new(
                ErrorKind::UnexpectedEndOfName,
                format!("Expected char at position {}.", self.position)
            ))
        }

    }

    pub fn demangle_generic_type(&mut self) -> Result<Rc<Node>, Error> {
        let gen_sig = self.pop_node_of_kind(Kind::DependentGenericSignature)?;
        let ty = self.pop_node_of_kind(Kind::Type)?;
        let node = self.cache.create_node_with_children(Kind::DependentGenericType, vec![gen_sig, ty]);
        Ok(self.cache.create_type_node(node))
    }

    pub fn demangle_operator(&mut self) -> Result<Rc<Node>, Error> {
        if let Some(c) = self.next_char_skip_padding() {
            match c as char {
                '\x01'..='\x0C' => unimplemented!("Symbolic references are not implemented."),
                'A' => self.demangle_multi_substitutions(),
                'B' => self.demangle_builtin_type(),
                'C' => self.demangle_any_generic_type(Kind::Class),
                'D' => unimplemented!("Type mangling is not implemented."),
                'E' => self.demangle_extension_context(),
                'F' => unimplemented!("Plain functions are not supported."),
                'G' => unimplemented!("Bound generic types are not supported"),
                'H' => unimplemented!("H operator is not supported"),
                'I' => unimplemented!("demangleImplFunctionType() is not implemented."),
                'K' => Ok(self.cache.create_none_node(Kind::ThrowsAnnotation)),
                'L' => unimplemented!("demangleLocalIdentifier() is not implemented."),
                'M' => unimplemented!("demangleMetatype() is not implemented."),
                'N' => {
                    let node = self.pop_node()?;
                    Ok(self.cache.create_node_with_child(Kind::TypeMetadata, node))
                },
                'O' => self.demangle_any_generic_type(Kind::Enum),
                'P' => self.demangle_any_generic_type(Kind::Protocol),
                'Q' => unimplemented!("demangleArchetype() is not implemented."),
                'R' => unimplemented!("demangleGenericRequirement() is not implemented."),
                'S' => self.demangle_standard_substitution(),
                'T' => unimplemented!("demangleThunkOrSpecialization() is not implemented."),
                'V' => self.demangle_any_generic_type(Kind::Structure),
                'W' => unimplemented!("demangleWitness() is not implemented."),
                'X' => unimplemented!("demangleSpecialType() is not implemented."),
                'Z' => {
                    let node = self.pop_node()?;
                    if kind::is_entity(node.kind()) {
                        Ok(self.cache.create_node_with_child(Kind::Static, node))
                    } else {
                        Err(Error::new(
                            ErrorKind::UnexpectedNodeKind,
                            format!("Expected entity type at position {}.", self.position)
                        ))
                    }
                },
                'a' => self.demangle_any_generic_type(Kind::TypeAlias),
                'c' => unimplemented!("popFunctionType() is not implemented."),
                'd' => Ok(self.cache.create_none_node(Kind::VariadicMarker)),
                'f' => unimplemented!("demangleFunctionEntity() is not implemented."),
                'g' => unimplemented!("demangleRetroactiveConformance() is not implemented."),
                'h' => {
                    let child = self.pop_type_and_get_child()?;
                    let node = self.cache.create_node_with_child(Kind::Shared, child);
                    Ok(self.cache.create_type_node(node))
                },
                'i' => unimplemented!("demangleSubscript() is not implemented."),
                'l' => unimplemented!("demangleGenericSignature() is not implemented."),
                'm' => {
                    let child = self.pop_node_of_kind(Kind::Type)?;
                    let node = self.cache.create_node_with_child(Kind::Metatype, child);
                    Ok(self.cache.create_type_node(node))
                },
                'n' => {
                    let child = self.pop_type_and_get_child()?;
                    let node = self.cache.create_node_with_child(Kind::Owned, child);
                    Ok(self.cache.create_type_node(node))
                },
                'o' => unimplemented!("demangleOperatorIdentifier() is not implemented."),
                'p' => unimplemented!("demangleProtocolListType() is not implemented."),
                'q' => unimplemented!("createType(demangleGenericParamIndex()) is not implemented."),
                'r' => unimplemented!("demangleGenericSignature() is not implemented."),
                's' => Ok(self.cache.create_module_node(STDLIB_NAME.to_string())),
                't' => unimplemented!("popTuple() is not implemented."),
                'u' => unimplemented!("demangleGenericType() is not implemented."),
                'v' => unimplemented!("demangleVariable() is not implemented."),
                'w' => unimplemented!("demangleValueWitness() is not implemented."),
                'x' => unimplemented!("createType(getDependentGenericParamType(0, 0)) is not implemented."),
                'y' => Ok(self.cache.create_none_node(Kind::EmptyList)),
                'z' => {
                    let child = self.pop_type_and_get_child()?;
                    let node = self.cache.create_node_with_child(Kind::InOut, child);
                    Ok(self.cache.create_type_node(node))
                },
                '_' => Ok(self.cache.create_none_node(Kind::FirstElementMarker)),
                '.' => {
                    self.push_back();
                    let suffix = self.buffer[self.position..].to_vec();
                    let node = self.cache.create_text_node(Kind::Suffix, String::from_utf8(suffix).or_else(|_| {
                        Err(Error::new(
                            ErrorKind::UnexpectedCharacter,
                            "Invalid suffix".to_string()
                        ))
                    })?);
                    self.position = self.buffer.len();
                    Ok(node)
                }
                _ => {
                    self.push_back();
                    self.demangle_identifier()
                }
            }
        } else {
            Err(Error::new(ErrorKind::UnexpectedEndOfName, "".to_string()))
        }
    }

    pub fn parse_and_push_nodes(&mut self) -> Result<(), Error> {
        while self.position < self.buffer.len() {
            let node = self.demangle_operator()?;
            self.push_node(node);
        }
        Ok(())
    }

    #[cfg(test)]
    pub fn add_word_subst(&mut self, word: String) {
        self.words.push(word)
    }

    #[cfg(test)]
    pub fn words(&mut self) -> &Vec<String> {
        &self.words
    }

    #[cfg(test)]
    pub fn position(&mut self) -> usize {
        self.position
    }
}