# tptp

A crate for reading files in the [TPTP](tptp.org) format.

## Quickstart

```rust
use tptp;

// start to read with the DefaultResolver for paths
let reader = tptp::Reader::new()
	.follow_includes() // follow include directives
	.read("example.p")?; // start here, resolved from current directory

let mut count = 0;

// stream TPTP statements
for statement in reader {
	println!("{:?}\n", statement?);
	count += 1;
}

println!("{} total inputs", count);
```

## Features

* Streaming parser
* Flexible `include()` handling with the `Resolve` trait
* Reasonably complete FOF support
* Sensible AST
* String sharing

## Wishlist

In order of urgency:

* More complete FOF parsing
* CNF support
* `Display` instances
* Documentation
* "other" support?
