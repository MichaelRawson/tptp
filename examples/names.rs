use std::collections::HashSet;
use std::io::Read;
use tptp::parsers::TPTPIterator;
use tptp::syntax::{Name, Visitor};

#[derive(Default)]
struct Names<'a> {
    seen: HashSet<Name<'a>>,
}

impl<'v, 'n> Visitor<'v> for Names<'n>
where
    'v: 'n,
{
    fn visit_name(&mut self, name: Name<'v>) {
        if !self.seen.contains(&name) {
            println!("{}", name);
            self.seen.insert(name);
        }
    }
}

fn read_file() -> Box<[u8]> {
    let path = std::env::args().nth(1).expect("supply an argument");
    let mut file = std::fs::File::open(path).expect("opening file failed");
    let mut buffer = vec![];
    file.read_to_end(&mut buffer).expect("failed reading file");
    return buffer.into_boxed_slice();
}

fn main() {
    let bytes = read_file();
    let mut visitor = Names::default();
    let mut parser = TPTPIterator::<()>::new(&bytes);
    for input in &mut parser {
        visitor.visit_tptp_input(input.expect("syntax error"));
    }
}
