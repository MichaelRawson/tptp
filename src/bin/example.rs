extern crate tptp;

fn run() -> Result<(), tptp::error::ErrorWithContext> {
    for statement in tptp::stream("example.p")? {
        println!("{:#?}", statement?);
    }
    Ok(())
}

fn main() {
    match run() {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{:#?}", e);
        }
    }
}
