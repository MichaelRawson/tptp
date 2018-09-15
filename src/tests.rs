use super::*;

fn count_inputs(start: &str) -> Result<usize, Context> {
    let reader = Reader::new().follow_includes().read(start)?;

    let mut count = 0;
    for statement in reader {
        println!("{:?}\n", statement?);
        count += 1;
    }

    Ok(count)
}

#[test]
fn example() {
    let count = count_inputs("example.p").expect("failed");
    println!("{} total inputs", count);
}
