use std::io;
use std::io::Read;
use tptp::TPTPIterator;

fn read_stdin() -> io::Result<Box<[u8]>> {
    let mut buffer = vec![];
    io::stdin().lock().read_to_end(&mut buffer)?;
    Ok(buffer.into_boxed_slice())
}

fn syntax_error() -> ! {
    eprintln!("syntax error");
    std::process::exit(1)
}

fn main() -> io::Result<()> {
    let bytes = read_stdin()?;
    let mut parser = TPTPIterator::<()>::new(&bytes);
    for input in &mut parser {
        if input.is_err() {
            syntax_error()
        }
    }
    if !parser.remaining.is_empty() {
        syntax_error()
    }
    Ok(())
}
