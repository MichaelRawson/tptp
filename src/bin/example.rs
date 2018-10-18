extern crate tptp;

use std::env::args;

fn run(path: &str) -> Result<(), tptp::error::ErrorInfo> {
    for statement in tptp::stream(path)? {
        println!("{}", statement?);
    }
    Ok(())
}

fn main() {
    let path = args().nth(1).expect("need a file to parse");
    match run(&path) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{:#?}", e);
        }
    }
}
