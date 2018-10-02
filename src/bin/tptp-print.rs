extern crate tptp;
use tptp::prelude::*;

fn print(start: &str) -> Result<(), Error> {
    let reader = ReaderBuilder::new().follow_includes().read(start)?;

    for item in reader {
        let (name, position, statement) = item?;
        println!("{} at {}", name, position);
        println!("{}\n", statement);
    }

    Ok(())
}

fn main() {
    print("/dev/stdin").expect("failed");
}
