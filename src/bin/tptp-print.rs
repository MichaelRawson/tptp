extern crate tptp;
use tptp::prelude::*;

fn print(start: &str) -> Result<(), Error> {
    let reader = ReaderBuilder::new().follow_includes().read(start)?;

    for statement in reader {
        println!("{}", statement?);
    }
    Ok(())
}

fn main() {
    print("/dev/stdin").expect("failed");
}
