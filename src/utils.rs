use alloc::fmt;
use alloc::vec;
use alloc::vec::Vec;
use core::mem;
use nom::Parser;

use crate::{Error, Result};

macro_rules! impl_unit_display {
    ($Type:ident) => {
        impl fmt::Display for $Type {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}

macro_rules! impl_unit_anon_display {
    ($Type:ident) => {
        impl fmt::Display for $Type<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}

macro_rules! impl_enum_anon_display {
    ($Type:ident, $($Tag:ident),*) => {
        impl fmt::Display for $Type<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self {
                    $(
                        $Type::$Tag(data) => write!(f, "{}", data),
                    )*
                }
            }
        }
    };
}

macro_rules! parser {
    ($(#[$outer:meta])* $Type:ident, $parser:expr) => {
        impl<'a, E: Error<'a>> Parse<'a, E> for $Type<'a> {
            $(#[$outer])*
            fn parse(x: &'a [u8]) -> Result<Self, E> {
                $parser(x)
            }
        }
    };
}

macro_rules! parser_no_lifetime {
    ($(#[$outer:meta])* $Type:ident, $parser:expr) => {
        impl<'a, E: Error<'a>> Parse<'a, E> for $Type {
            $(#[$outer])*
            fn parse(x: &'a [u8]) -> Result<Self, E> {
                $parser(x)
            }
        }
    };
}

macro_rules! unit_parser {
    ($(#[$outer:meta])* $name:ident, $parser:expr) => {
        $(#[$outer])*
        pub fn $name<'a, E: Error<'a>>(x: &'a [u8]) -> Result<(), E> {
            $parser(x)
        }
    }
}

macro_rules! slice_parser {
    ($(#[$outer:meta])* $name:ident, $parser:expr) => {
        $(#[$outer])*
        pub fn $name<'a, E: Error<'a>>(x: &'a [u8]) -> Result<&'a[u8], E> {
            $parser(x)
        }
    }
}

pub(crate) fn fmt_list<T: fmt::Display>(
    f: &mut fmt::Formatter,
    sep: &'static str,
    args: &[T],
) -> fmt::Result {
    if args.is_empty() {
        return Ok(());
    }

    let mut args = args.iter();
    write!(f, "{}", args.next().unwrap())?;
    for arg in args {
        write!(f, "{}{}", sep, arg)?;
    }
    Ok(())
}

// https://github.com/Geal/nom/issues/898
pub(crate) fn fold_many0_once<'a, E, Item, Acc, F, G>(
    mut item: F,
    mut acc: Acc,
    mut fold: G,
) -> impl FnOnce(&'a [u8]) -> Result<'a, Acc, E>
where
    F: Parser<&'a [u8], Item, E>,
    G: FnMut(Acc, Item) -> Acc,
    E: Error<'a>,
{
    move |x| {
        let mut start = x;
        while let Ok((x, item)) = item.parse(start) {
            acc = fold(acc, item);
            start = x;
        }
        Ok((start, acc))
    }
}

pub(crate) struct GarbageFirstVec<T>(Vec<mem::MaybeUninit<T>>);

impl<T> Default for GarbageFirstVec<T> {
    fn default() -> Self {
        let first = mem::MaybeUninit::uninit();
        Self(vec![first])
    }
}

impl<T> GarbageFirstVec<T> {
    pub(crate) fn push(&mut self, t: T) {
        self.0.push(mem::MaybeUninit::new(t));
    }

    pub(crate) fn finish(mut self, t: T) -> Vec<T> {
        self.0[0] = mem::MaybeUninit::new(t);
        let original = mem::ManuallyDrop::new(self.0);
        let ptr = original.as_ptr() as *mut T;
        let len = original.len();
        let cap = original.capacity();
        unsafe { Vec::from_raw_parts(ptr, len, cap) }
    }
}
