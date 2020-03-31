use tptp::parsers::TPTPIterator;

#[macro_use]
extern crate afl;

fn main() {
    fuzz!(|bytes| {
        let mut parser = TPTPIterator::<()>::new(&bytes);
        for input in &mut parser {
            if input.is_err() {
                break
            }
        }
    })
}
