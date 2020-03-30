use std::io::Read;
use tptp::parsers::TPTPIterator;

fn read_file() -> Box<[u8]> {
    let path = std::env::args().nth(1).expect("supply an argument");
    let mut file = std::fs::File::open(path).expect("opening file failed");
    let mut buffer = vec![];
    file.read_to_end(&mut buffer).expect("failed reading file");
    return buffer.into_boxed_slice();
}

fn main() {
    let bytes = read_file();
    let mut parser = TPTPIterator::<()>::new(&bytes);
    for input in &mut parser {
        println!("{}", input.expect("syntax error"));
    }
}
