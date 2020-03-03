use std::collections::HashMap;
use std::collections::hash_map::Values;
use std::rc::Rc;
use enum_map::{self, EnumMap};

use crate::node::{Node, Kind, Payload};

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
    pub fn new() -> NodeCache {
        NodeCache {
            nodes: EnumMap::new()
        }
    }

    pub fn create_node(&mut self, kind: Kind, payload: Payload) -> Rc<Node> {
        let inner_map = &mut self.nodes[kind];

        let entry = inner_map.entry(payload.clone());
        entry.or_insert_with(|| Rc::new(Node::new(kind, payload)))
            .clone()
    }

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