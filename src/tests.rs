use crate::Result;

pub(crate) fn check_parse<'a, P, T: 'a>(parser: P, input: &'a [u8]) -> T
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

macro_rules! parse_snapshot {
    ($T: ident, $input: expr) => {
        let parsed = crate::tests::check_parse($T::parse, $input);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed);
    };
}

pub(crate) use parse_snapshot;

pub(crate) fn check_size<T>() {
    assert!(core::mem::size_of::<T>() <= 64);
}
