use std::time::Instant;
use tptp::parsers::tptp_input_iterator;
use tptp::syntax::Visitor;

const ITERATIONS: usize = 100_000;
const FOF: &[u8] = include_bytes!("SYN000+1.p");
const CNF: &[u8] = include_bytes!("SYN000-1.p");

struct DoNothing;
impl<'a> Visitor<'a> for DoNothing {}

fn benchmark(bytes: &[u8], name: &'static str) {
    println!(
        "{} iterations, {} bytes of {}",
        ITERATIONS,
        bytes.len(),
        name
    );

    let start = Instant::now();

    for _ in 0..ITERATIONS {
        let mut visitor = DoNothing;
        let mut parser = tptp_input_iterator::<()>(bytes);
        for input in &mut parser {
            visitor.visit_tptp_input(input);
        }
        assert!(parser.finish().is_ok());
    }

    let elapsed = start.elapsed();
    let whole = elapsed.as_secs() as u128;
    let fractional = elapsed.subsec_nanos() as u128;
    let nanoseconds = 1_000_000_000 * whole + fractional;
    let seconds = (nanoseconds as f64) / 1E9;
    let seconds_per_iter = seconds / (ITERATIONS as f64);
    let bytes_per_second = (bytes.len() as f64) / seconds_per_iter;
    let mb_per_second = bytes_per_second / 1E6;

    println!("{:.2} seconds total ({:.2} MB/s).", seconds, mb_per_second);
}

fn main() {
    benchmark(CNF, "SYN000-1.p");
    benchmark(FOF, "SYN000+1.p");
}
