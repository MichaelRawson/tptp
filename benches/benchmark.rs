use std::time::Instant;
use tptp::parsers::ignored_then_tptp_input;

const ITERATIONS: usize = 100_000;
const FOF: &[u8] = include_bytes!("SYN000+1.p");

fn synthetic_fof() {
    println!(
        "{} iterations, {} bytes of SYN000+1.p",
        ITERATIONS,
        FOF.len()
    );

    let start = Instant::now();

    for _ in 0..ITERATIONS {
        let mut position = FOF;
        loop {
            let result = ignored_then_tptp_input::<()>(position);
            let (next, parsed) = result.expect("parse error");
            if parsed.is_none() {
                break;
            }
            position = next;
        }
    }

    let elapsed = start.elapsed();
    let whole = elapsed.as_secs() as u128;
    let fractional = elapsed.subsec_nanos() as u128;
    let nanoseconds = 1_000_000_000 * whole + fractional;
    let seconds = (nanoseconds as f64) / 1E9;
    let seconds_per_iter = seconds / (ITERATIONS as f64);
    let bytes_per_second = (FOF.len() as f64) / seconds_per_iter;
    let mb_per_second = bytes_per_second / 1E6;

    println!(
        "{:.*} seconds total ({:.*} MB/s).",
        2, seconds, 2, mb_per_second
    );
}

fn main() {
    synthetic_fof();
}
