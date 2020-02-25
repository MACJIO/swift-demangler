use std::rc::Rc;
use std::io::Result as IoResult;
use std::{u8, u16, u32, u64, i8, i16, i32, i64};
use bytes::BufMut;
pub use self::kind::Kind;

pub struct Node {
    kind: Kind,
    payload: Payload,
}

pub enum Payload {
    None,
    Text(String),
    Index(u64),
    Children(Vector<Rc<Node>>)
}

impl Node {
    pub fn new(kind: Kind, payload: Payload) -> Node {
        Node {
            kind,
            payload
        }
    }

    pub fn payload(&self) -> &Payload {
        &self.payload
    }

    pub fn kind(&self) -> Kind {
        self.kind
    }

    pub fn has_text(&self) -> bool {
        match self.payload {
            Payload::Text(_) => true,
            _ => false,
        }
    }

    pub fn has_index(&self) -> bool {
        match self.payload {
            Payload::Index(_) => true,
            _ => false,
        }
    }

    pub fn num_children(&self) -> usize {
        match &self.payload {
            Payload::Children(c) => c.len,
            _ => 0,
        }
    }

    pub fn has_children(&self) -> bool {
        self.num_children() != 0
    }

    pub fn get_child(&self, i: u32) -> Option<Rc<Node>>{
        match &self.payload {
            Payload::Children(c) => match c.get(i) {
                Some(r) => r.clone(),
                None => None,
            },
            _ => None,
        }
    }
}

pub fn is_alias_node(node: Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_alias_node(node.get_child(0).unwrap()),
        Kind::TypeAlias => true,
        _ => false,
    }
}

pub fn is_class_node(node: Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_class_node(node.get_child(0).unwrap()),
        Kind::Class | Kind::BoundGenericClass => true,
        _ => false,
    }
}

pub fn is_enum_node(node: Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_enum_node(node.get_child(0).unwrap()),
        Kind::Enum | Kind::BoundGenericEnum => true,
        _ => false,
    }
}

pub fn is_protocol_node(node: Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_protocol_node(node.get_child(0).unwrap()),
        Kind::Protocol | Kind::ProtocolSymbolicReference => true,
        _ => false,
    }
}

pub fn is_struct_node(node: Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_struct_node(node.get_child(0).unwrap()),
        Kind::Structure | Kind::BoundGenericStructure => true,
        _ => false,
    }
}

pub trait AddressSpace {
    fn read_bytes<T: BufMut>(&self, addr: usize, size: usize, buf: &mut T) -> IoResult<()>;

    fn read_u8(&self, addr: usize) -> IoResult<u8> {
        let mut b = [0u8];

        self.read_bytes(addr, b.len(), &b)?;

        u8::from_ne_bytes(b)
    }

    fn read_u16(&self, addr: usize) -> IoResult<u16> {
        let mut b = [0u8; 2];

        self.read_bytes(addr, b.len(), &b)?;

        u16::from_ne_bytes(b)
    }

    fn read_u32(&self, addr: usize) -> IoResult<u32> {
        let mut b = [0u8; 4];

        self.read_bytes(addr, b.len(), &b)?;

        u32::from_ne_bytes(b)
    }

    fn read_u64(&self, addr: usize) -> IoResult<u64> {
        let mut b = [0u8; 8];

        self.read_bytes(addr, b.len(), &b)?;

        u64::from_ne_bytes(b)
    }

    fn read_i8(&self, addr: usize) -> IoResult<i8> {
        let mut b = [0u8];

        self.read_bytes(addr, b.len(), &b)?;

        i8::from_ne_bytes(b)
    }

    fn read_i16(&self, addr: usize) -> IoResult<i16> {
        let mut b = [0u8; 2];

        self.read_bytes(addr, b.len(), &b)?;

        i16::from_ne_bytes(b)
    }

    fn read_i32(&self, addr: usize) -> IoResult<i32> {
        let mut b = [0u8; 4];

        self.read_bytes(addr, b.len(), &b)?;

        i32::from_ne_bytes(b)
    }

    fn read_i64(&self, addr: usize) -> IoResult<i64> {
        let mut b = [0u8; 8];

        self.read_bytes(addr, b.len(), &b)?;

        i64::from_ne_bytes(b)
    }
}


