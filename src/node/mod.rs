use std::rc::Rc;

pub mod kind;
pub use kind::Kind;

pub struct Node {
    kind: Kind,
    payload: Payload,
}

pub enum Payload {
    None,
    Text(String),
    Index(u64),
    Children(Vec<Rc<Node>>)
}

impl Node {
    /// Creates a new node.
    pub fn new(kind: Kind, payload: Payload) -> Node {
        Node {
            kind,
            payload
        }
    }

    /// Returns the payload of a node.
    pub fn payload(&self) -> &Payload {
        &self.payload
    }

    /// Returns the kind of a node.
    pub fn kind(&self) -> Kind {
        self.kind
    }

    /// Returns true if the payload of a node is text.
    pub fn has_text(&self) -> bool {
        match self.payload {
            Payload::Text(_) => true,
            _ => false,
        }
    }

    /// Returns true if the payload of a node is an index.
    pub fn has_index(&self) -> bool {
        match self.payload {
            Payload::Index(_) => true,
            _ => false,
        }
    }

    /// Returns the number of child nodes of a node.
    pub fn num_children(&self) -> usize {
        match &self.payload {
            Payload::Children(c) => c.len(),
            _ => 0,
        }
    }

    /// Returns true if a node has child nodes.
    pub fn has_children(&self) -> bool {
        self.num_children() != 0
    }

    /// Returns a reference to a child node at a given index or None in case node ether has no
    /// children or the index is OOB.
    pub fn get_child(&self, i: usize) -> Option<Rc<Node>> {
        if let Payload::Children(v) = &self.payload {
            v.get(i).and_then(|r| Some(r.clone()))
        } else {
            None
        }
    }
}

pub fn is_alias_node(node: &Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_alias_node(node.get_child(0).unwrap().as_ref()),
        Kind::TypeAlias => true,
        _ => false,
    }
}

pub fn is_class_node(node: &Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_class_node(node.get_child(0).unwrap().as_ref()),
        Kind::Class | Kind::BoundGenericClass => true,
        _ => false,
    }
}

pub fn is_enum_node(node: &Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_enum_node(node.get_child(0).unwrap().as_ref()),
        Kind::Enum | Kind::BoundGenericEnum => true,
        _ => false,
    }
}

pub fn is_protocol_node(node: &Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_protocol_node(node.get_child(0).unwrap().as_ref()),
        Kind::Protocol | Kind::ProtocolSymbolicReference => true,
        _ => false,
    }
}

pub fn is_struct_node(node: &Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_struct_node(node.get_child(0).unwrap().as_ref()),
        Kind::Structure | Kind::BoundGenericStructure => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use crate::node::*;

    #[test]
    fn test_is_class_node_recurse() {
        // create a class node
        let node1 = Node::new(
            Kind::BoundGenericClass,
            Payload::Text("test".to_string())
        );
        // create a parent node of Type kind
        let node2 = Node::new(Kind::Type, Payload::Children(
            vec![Rc::new(node1)]
        ));

        assert!(is_class_node(&node2));
        assert!(!is_alias_node(&node2));
        assert!(!is_struct_node(&node2));
        assert!(!is_enum_node(&node2));
        assert!(!is_protocol_node(&node2));
    }
}
