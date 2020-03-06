use std::collections::HashMap;
use std::collections::hash_map::Values;
use std::rc::Rc;
use enum_map::{self, EnumMap};

use crate::node::{Node, Kind, Payload};
use crate::demangler::{STDLIB_NAME, create_type_node, create_node_with_children};

pub struct NodeCacheIterator<'a> {
    outer_iter: enum_map::Values<'a, HashMap<Payload, Rc<Node>>>,
    inner_iter: Option<Values<'a, Payload, Rc<Node>>>
}

impl<'a> Iterator for NodeCacheIterator<'a> {
    type Item = &'a Rc<Node>;

    fn next(&mut self) -> Option<Self::Item> {
        let inner_iter = self.inner_iter.as_mut();

        // in case inner_iter is None the outer_iter is already exhausted
        if let Some(inner_iter) = inner_iter {
            let next = inner_iter.next();

            // update the inner_iter in case it is exhausted
            if let None = next {
                self.inner_iter = self.outer_iter.next().and_then(|m| Some(m.values()));
                self.inner_iter.as_mut().and_then(|i| i.next())
            } else {
                next
            }
        } else {
            None
        }
    }
}

pub struct NodeCache {
    // EnumMap is quite limited due to the fact that it is impossible to disable
    // default values generation, however it only simplifies the code here since
    // we don't need to explicitly create a HashMap for each new kind here. Also
    // an EnumMap provides better performance than a HashMap for enum keys.
    nodes: EnumMap<Kind, HashMap<Payload, Rc<Node>>>
}

impl NodeCache {
    /// Creates a new node cache.
    pub fn new() -> NodeCache {
        NodeCache {
            nodes: EnumMap::new()
        }
    }

    /// Creates an arbitrary node. In case the node was previously created an Rc pointing to the
    /// same node will be returned.
    ///
    /// # Example
    /// ```
    /// use swift_demangler::{NodeCache, Node, Kind, Payload};
    ///
    /// let mut cache = NodeCache::new();
    ///
    /// let node1 = cache.create_node(Kind::Identifier, Payload::Text("test".to_string()));
    /// let node2 = cache.create_node(Kind::Identifier, Payload::Text("test".to_string()));
    ///
    /// assert_eq!(&*node1 as *const Node, &*node2 as *const Node);
    /// ```
    pub fn create_node(&mut self, kind: Kind, payload: Payload) -> Rc<Node> {
        let inner_map = &mut self.nodes[kind];

        let entry = inner_map.entry(payload.clone());
        entry.or_insert_with(|| Rc::new(Node::new(kind, payload)))
            .clone()
    }

    /// Creates a node with none payload. In case a none node of the same kind was previously
    /// created, an Rc pointing to it will be returned.
    ///
    /// # Example
    /// ```
    /// use swift_demangler::{NodeCache, Node, Kind, Payload};
    ///
    /// let mut cache = NodeCache::new();
    ///
    /// let node1 = cache.create_none_node(Kind::Identifier);
    /// let node2 = cache.create_node(Kind::Identifier, Payload::None);
    ///
    /// assert_eq!(&*node1 as *const Node, &*node2 as *const Node);
    /// ```
    pub fn create_none_node(&mut self, kind: Kind) -> Rc<Node> {
        self.create_node(kind, Payload::None)
    }

    /// Creates a text node. In case a text node with the same text was previously created, an Rc
    /// pointing to it will be returned.
    ///
    /// # Example
    /// ```
    /// use swift_demangler::{NodeCache, Node, Kind, Payload};
    ///
    /// let mut cache = NodeCache::new();
    ///
    /// let node1 = cache.create_text_node(Kind::Identifier, "test".to_string());
    /// let node2 = cache.create_node(Kind::Identifier, Payload::Text("test".to_string()));
    ///
    /// assert_eq!(&*node1 as *const Node, &*node2 as *const Node);
    /// ```
    pub fn create_text_node(&mut self, kind: Kind, text: String) -> Rc<Node> {
        self.create_node(kind, Payload::Text(text))
    }

    /// Creates an index node. In case an index node with the same index was previously created, an
    /// Rc pointing to it will be returned.
    ///
    /// # Example
    /// ```
    /// use swift_demangler::{NodeCache, Node, Kind, Payload};
    ///
    /// let mut cache = NodeCache::new();
    ///
    /// let node1 = cache.create_index_node(Kind::Index, 1337);
    /// let node2 = cache.create_node(Kind::Index, Payload::Index(1337));
    ///
    /// assert_eq!(&*node1 as *const Node, &*node2 as *const Node);
    /// ```
    pub fn create_index_node(&mut self, kind: Kind, index: u64) -> Rc<Node> {
        self.create_node(kind, Payload::Index(index))
    }

    /// Creates a node with 0 or more children.
    ///
    /// # Example
    /// ```
    /// use swift_demangler::{NodeCache, Node, Kind, Payload};
    ///
    /// let mut cache = NodeCache::new();
    ///
    /// let child = cache.create_ident_node("test".to_string());
    /// let node1 = cache.create_node_with_children(Kind::Type, vec![child.clone()]);
    /// let node2 = cache.create_node(Kind::Type, Payload::Children(vec![child]));
    ///
    /// assert_eq!(&*node1 as *const Node, &*node2 as *const Node);
    /// ```
    pub fn create_node_with_children(&mut self, kind: Kind, children: Vec<Rc<Node>>) -> Rc<Node> {
        self.create_node(kind, Payload::Children(children))
    }

    /// Creates a node with a single child.
    ///
    /// # Example
    /// ```
    /// use swift_demangler::{NodeCache, Node, Kind, Payload};
    ///
    /// let mut cache = NodeCache::new();
    ///
    /// let child = cache.create_ident_node("test".to_string());
    /// let node1 = cache.create_node_with_child(Kind::Type, child.clone());
    /// let node2 = cache.create_node(Kind::Type, Payload::Children(vec![child]));
    ///
    /// assert_eq!(&*node1 as *const Node, &*node2 as *const Node);
    /// ```
    pub fn create_node_with_child(&mut self, kind: Kind, child: Rc<Node>) -> Rc<Node> {
        self.create_node_with_children(kind, vec![child])
    }

    /// Creates an identifier node (a text node of Identifier kind).
    ///
    /// # Example
    /// ```
    /// use swift_demangler::{NodeCache, Node, Kind, Payload};
    ///
    /// let mut cache = NodeCache::new();
    ///
    /// let node1 = cache.create_ident_node("test".to_string());
    /// let node2 = cache.create_node(Kind::Identifier, Payload::Text("test".to_string()));
    ///
    /// assert_eq!(&*node1 as *const Node, &*node2 as *const Node);
    /// ```
    pub fn create_ident_node(&mut self, ident: String) -> Rc<Node> {
        self.create_text_node(Kind::Identifier, ident)
    }

    /// Creates an module node (a text node of Module kind).
    ///
    /// # Example
    /// ```
    /// use swift_demangler::{NodeCache, Node, Kind, Payload};
    ///
    /// let mut cache = NodeCache::new();
    ///
    /// let node1 = cache.create_module_node("test".to_string());
    /// let node2 = cache.create_node(Kind::Module, Payload::Text("test".to_string()));
    ///
    /// assert_eq!(&*node1 as *const Node, &*node2 as *const Node);
    /// ```
    pub fn create_module_node(&mut self, name: String) -> Rc<Node> {
        self.create_text_node(Kind::Module, name)
    }

    /// Creates an type node (a node of Type kind with a single child).
    ///
    /// # Example
    /// ```
    /// use swift_demangler::{NodeCache, Node, Kind, Payload};
    ///
    /// let mut cache = NodeCache::new();
    ///
    /// let ty = cache.create_text_node(Kind::BuiltinTypeName, "Builtin.UInt".to_string());
    /// let node1 = cache.create_type_node(ty.clone());
    /// let node2 = cache.create_node_with_child(Kind::Type, ty);
    ///
    /// assert_eq!(&*node1 as *const Node, &*node2 as *const Node);
    /// ```
    pub fn create_type_node(&mut self, ty: Rc<Node>) -> Rc<Node> {
        self.create_node_with_child(Kind::Type, ty)
    }

    pub fn create_swift_type(&mut self, kind: Kind, name: String) -> Rc<Node> {
        create_type_node(
            create_node_with_children(
                kind,
                vec![
                    self.create_module_node(STDLIB_NAME.to_string()),
                    self.create_ident_node(name)
                ]
            )
        )
    }

    /// Returns an iterator over all currently cached nodes. This function mainly exists for tests
    /// and may be removed in the future.
    pub fn iter(&self) -> NodeCacheIterator {
        let mut outer_iter = self.nodes.values();
        let inner_iter = outer_iter.next()
            .and_then(|inner_map| Some(inner_map.values()));

        NodeCacheIterator {
            outer_iter,
            inner_iter,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::Deref;

    /// Tests if two Rc<Node> instances point to the same node.
    fn is_same_node(node1: &Rc<Node>, node2: &Rc<Node>) -> bool {
        node1.deref() as *const Node == node2.deref() as *const Node
    }

    #[test]
    fn test_basic() {
        let mut cache = NodeCache::new();

        let node1 = cache.create_node(Kind::Module, Payload::None);
        let node2 = cache.create_node(Kind::Module, Payload::None);

        assert!(is_same_node(&node1, &node2));

        let node3 = cache.create_node(Kind::Module, Payload::Text("test".to_string()));
        let node4 = cache.create_node(Kind::Module, Payload::Text("test".to_string()));

        assert!(!is_same_node(&node1, &node3));
        assert!(is_same_node(&node3, &node4));
    }
}