use serde_json::ser::to_writer;
use std::io;
use std::io::Read;
use tptp::parsers::TPTPIterator;
use tptp::syntax::TPTPInput;

const BUFSIZE: usize = 1024;

fn read_stdin_chunk(buf: &mut Vec<u8>) -> io::Result<usize> {
    let mut tmp = [0; BUFSIZE];
    let read = io::stdin().lock().read(&mut tmp)?;
    buf.extend_from_slice(&tmp[0..read]);
    Ok(read)
}

fn write_formula(input: &TPTPInput) {
    to_writer(io::stdout().lock(), input).expect("failed to write data");
    println!();
}

fn main() -> io::Result<()> {
    let mut buf = vec![];
    while read_stdin_chunk(&mut buf)? > 0 {
        let mut parser = TPTPIterator::<()>::new(&buf);
        while let Some(result) = parser.next() {
            if let Ok(input) = result {
                write_formula(&input);
            }
            else {
                eprintln!("syntax error");
                break;
            }
        }
        buf = parser.remaining.to_vec();
    }
    assert!(buf.is_empty());
    Ok(())
}
