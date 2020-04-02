# tptp

A [crate](https://crates.io/crates/tptp) for reading files in the [TPTP](http://tptp.org) format.

## Features
* [`nom`](https://crates.io/crates/nom) parsers for maximum flexibility
* high-performance, streaming, zero-copy parsing
* convenient abstractions: visitor pattern, input iterator
* complete CNF/FOF dialect support
* adherence to TPTP BNF: transform as you wish
* comprehensive test suite, fuzzing with [AFL](https://lcamtuf.coredump.cx/afl/)

## Documentation
See [docs.rs](https://docs.rs/tptp).
The `examples/` directory contains some trivial programs.
`tptp2json/` contains a slightly-less trivial program to transform TPTP input to [JSON Lines](http://jsonlines.org) via the magic of [serde](https://serde.rs).

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

## Limitations
Since this is effectively recursive-descent parsing, extremely-deeply-nested structures will cause you to run out of stack: this has not been a problem in practice.
Parsers work only on byte slices: this is by design. If you want to read data from somewhere, either use `mmap(2)` (useful for large files) or read data in chunks until you can parse an input. See `nom`'s [streaming documentation](https://docs.rs/nom/5.1.1/nom/#streaming--complete).

## Wishlist

* FOFX/TFF/THF support?
