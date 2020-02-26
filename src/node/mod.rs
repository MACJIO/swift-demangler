use std::slice::Iter;

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
    Children(Vec<Node>)
}

pub struct NodeIterator<'a> {
    inner_iter: Option<Iter<'a, Node>>
}

impl<'a> Iterator for NodeIterator<'a> {
    type Item = &'a Node;

    fn next(&mut self) -> Option<Self::Item> {
        // could'n write it in one line since we need a mutable iterator reference
        if let Some(i) = &mut self.inner_iter {
            i.next()
        } else {
            None
        }
    }
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
        match &self.payload {
            Payload::Text(_) => true,
            _ => false,
        }
    }

    /// Returns text if the payload type is Text, otherwise None.
    pub fn get_text(&self) -> Option<&String> {
        match &self.payload {
            Payload::Text(s) => Some(s),
            _ => None,
        }
    }

    /// Returns true if the payload of a node is an index.
    pub fn has_index(&self) -> bool {
        match self.payload {
            Payload::Index(_) => true,
            _ => false,
        }
    }

    /// Returns an index if the payload type is Index, otherwise None.
    pub fn get_index(&self) -> Option<u64> {
        match &self.payload {
            Payload::Index(i) => Some(*i),
            _ => None,
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
    pub fn get_child(&self, i: usize) -> Option<&Node> {
        if let Payload::Children(v) = &self.payload {
            v.get(i)
        } else {
            None
        }
    }

    pub fn iter_children(&self) -> NodeIterator {
        NodeIterator {
            inner_iter: match &self.payload {
                Payload::Children(v) => Some(v.iter()),
                _ => None,
            }
        }
    }
}

pub fn is_alias_node(node: &Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_alias_node(node.get_child(0).unwrap()),
        Kind::TypeAlias => true,
        _ => false,
    }
}

pub fn is_class_node(node: &Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_class_node(node.get_child(0).unwrap()),
        Kind::Class | Kind::BoundGenericClass => true,
        _ => false,
    }
}

pub fn is_enum_node(node: &Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_enum_node(node.get_child(0).unwrap()),
        Kind::Enum | Kind::BoundGenericEnum => true,
        _ => false,
    }
}

pub fn is_protocol_node(node: &Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_protocol_node(node.get_child(0).unwrap()),
        Kind::Protocol | Kind::ProtocolSymbolicReference => true,
        _ => false,
    }
}

pub fn is_struct_node(node: &Node) -> bool {
    match node.kind {
        // TODO: verify that user input can not cause a panic here
        Kind::Type => is_struct_node(node.get_child(0).unwrap()),
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
            vec![node1]
        ));

        assert!(is_class_node(&node2));
        assert!(!is_alias_node(&node2));
        assert!(!is_struct_node(&node2));
        assert!(!is_enum_node(&node2));
        assert!(!is_protocol_node(&node2));
    }

    #[test]
    fn test_children_iter_wrong_payload() {
        let node = Node::new(
            Kind::BoundGenericClass, Payload::None
        );
        let mut iter = node.iter_children();

        assert!(iter.next().is_none());
    }

    #[test]
    fn test_children_iter_empty() {
        let node = Node::new(
            Kind::BoundGenericClass, Payload::None
        );
        let mut iter = node.iter_children();

        assert!(iter.next().is_none());
    }

    #[test]
    fn test_children_iter_one_child() {
        let child = Node::new(
            Kind::Class, Payload::None
        );
        let node = Node::new(
            Kind::BoundGenericClass,
            Payload::Children(vec![child])
        );
        let mut iter = node.iter_children();

        assert!(iter.next().is_some());
        assert!(iter.next().is_none());
    }
}
