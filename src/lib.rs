pub mod node;
pub mod demangler;
pub mod util;

mod punycode;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
