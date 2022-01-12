# tptp

A [crate](https://crates.io/crates/tptp) of parsers for the [TPTP](http://tptp.org) format.

## Features
* [`nom`](https://crates.io/crates/nom) parsers for maximum flexibility
* high-performance, streaming, zero-copy parsing
* convenient abstractions: visitor pattern, input iterator
* adherence to TPTP BNF
* complete CNF/FOF dialect support
* growing TFX support

## Documentation and Examples
Documentation on [docs.rs](https://docs.rs/tptp).
The `examples/` directory contains some trivial programs.
`tptp2json/` contains a slightly-less trivial program to transform TPTP input to [JSON Lines](http://jsonlines.org) via the magic of [serde](https://serde.rs).

## Performance
"Fast enough".

Unscientific benchmark:
```
$ cargo bench
100000 iterations, 970 bytes of SYN000-1.p
0.74 seconds total (130.54 MB/s).
100000 iterations, 1281 bytes of SYN000+1.p
1.51 seconds total (84.75 MB/s).
100000 iterations, 2420 bytes of SYN000_1.p
2.94 seconds total (82.37 MB/s).
100000 iterations, 5209 bytes of SYN000=2.p
6.38 seconds total (81.68 MB/s).
$
```

`examples/validate` currently checks 458MB of `CSR002+5.ax` in under 4 seconds.

## Limitations
Since this is effectively recursive-descent parsing, extremely-deeply-nested structures will cause you to run out of stack: this has not been a problem in practice.
Parsers work only on bytes in memory: this is by design. If you want to read data from somewhere, either use `mmap(2)` (useful for large files) or read data in chunks until you can parse an input. See `nom`'s [streaming documentation](https://docs.rs/nom/5.1.1/nom/#streaming--complete).