use std::io;
use std::io::Read;
use tptp::parsers::TPTPIterator;
use tptp::syntax::Name;
use tptp::visitor::Visitor;

#[derive(Default)]
struct Names;

impl<'a> Visitor<'a> for Names
{
    fn visit_name(&mut self, name: &Name<'a>) {
        println!("{}", name);
    }
}

fn read_stdin() -> io::Result<Box<[u8]>> {
    let mut buffer = vec![];
    io::stdin().lock().read_to_end(&mut buffer)?;
    Ok(buffer.into_boxed_slice())
}

fn main() -> io::Result<()> {
    let bytes = read_stdin()?;
    let mut visitor = Names::default();
    let mut parser = TPTPIterator::<()>::new(&bytes);
    for input in &mut parser {
        visitor.visit_tptp_input(&input.expect("syntax error"));
    }
    assert!(parser.remaining.is_empty());
    Ok(())
}
