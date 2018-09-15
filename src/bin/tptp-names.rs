extern crate tptp;
use tptp::ast::Statement;
use tptp::prelude::*;

fn print_names(start: &str) -> Result<(), Error> {
    let reader = ReaderBuilder::new().follow_includes().read(start)?;

    for statement in reader {
        if let Statement::Fof(name, _, _) = statement? {
            println!("{}", name.as_ref());
        }
    }
    Ok(())
}

fn main() {
    print_names("/dev/stdin").expect("failed");
}
