# tptp

A [crate](https://crates.io/crates/tptp) for reading files in the [TPTP](http://tptp.org) format.

## Features
* [`nom`](https://crates.io/crates/nom) parsers for maximum flexibility
* high-performance, streaming, zero-copy parsing
* convenient abstractions: visitor pattern, input iterator
* near-complete CNF/FOF dialect support

## Documentation
See [docs.rs](https://docs.rs/tptp).
The `examples/` directory contains some trivial programs.

## Performance
Unscientific benchmark:
```
$ cargo bench
100000 iterations, 2159 bytes of SYN000-1.p
1.03 seconds total (210.36 MB/s).
100000 iterations, 2702 bytes of SYN000+1.p
1.89 seconds total (142.68 MB/s).
$
```

`examples/validate` currently checks 458MB of `CSR002+5.ax` in under 4 seconds.

## Wishlist

* support complete CNF/FOF
* FOFX/TFF/THF support?
