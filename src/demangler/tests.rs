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
    make_demangler(b"123").demangle_word_subst().unwrap();
}

#[test]
#[should_panic]
fn test_demangle_word_subst_panic_on_eof() {
    make_demangler(b"").demangle_word_subst().unwrap();
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

#[test]
fn test_demangle_identifier_with_word_substs() {
    let mut dem = make_demangler(b"0abc3AAAdE2BB");

    dem.add_word_subst("One".to_string());
    dem.add_word_subst("Two".to_string());
    dem.add_word_subst("Three".to_string());
    dem.add_word_subst("Four".to_string());
    dem.add_word_subst("Five".to_string());

    let node = dem.demangle_identifier().unwrap_or_else(|e| {
        panic!("{:?}", e);
    });

    assert_eq!(node.kind(), Kind::Identifier);

    if let Payload::Text(t) = node.payload() {
        assert_eq!(t, "OneTwoThreeAAAFourFiveBB");
    } else {
        panic!("Invalid payload kind.")
    }
}

#[test]
fn test_push_multi_substitutions() {
    let mut dem = make_demangler(b"");

    let node = Rc::new(Node::new(Kind::AnonymousDescriptor, Payload::None));

    dem.add_subst(node);
    dem.push_multi_substitutions(3, 0).unwrap_or_else(|e| {
        panic!("{:?}", e);
    });

    dem.pop_node().expect("Expected at least 1 node");
    dem.pop_node().expect("Expected at least 2 nodes");
    dem.pop_node().expect("Expected at least 3 nodes");
}