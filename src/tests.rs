use alloc::fmt;
use alloc::format;
use insta::{assert_debug_snapshot, assert_display_snapshot};

use crate::{Parse, Result};

fn check_parse<'a, P, T: 'a>(parser: P, input: &'a [u8]) -> T
where
    P: FnOnce(&'a [u8]) -> Result<T, ()>,
{
    match parser(input) {
        Ok((b"\0", result)) => result,
        Ok((_, _)) => panic!("parsed, but bytes remaining"),
        Err(_) => panic!("parse error"),
    }
}

pub(crate) fn parse_unit<'a, P, T: 'a>(parser: P, input: &'a [u8])
where
    P: FnOnce(&'a [u8]) -> Result<T, ()>,
{
    check_parse(parser, input);
}

pub(crate) fn parse<'a, T: 'a>(input: &'a [u8])
where
    T: Parse<'a, ()> + fmt::Debug + fmt::Display,
{
    let parsed = check_parse(T::parse, input);
    assert_debug_snapshot!(parsed);
    assert_display_snapshot!(parsed);
}

pub(crate) fn check_size<T>() {
    assert!(core::mem::size_of::<T>() <= 64);
}
