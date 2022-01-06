use alloc::fmt;
use alloc::vec;
use alloc::vec::Vec;
use core::mem;
use nom::Parser;

use crate::{Error, Result};

pub(crate) struct Separated<'a, T>(pub(crate) char, pub(crate) &'a [T]);

impl<'a, T: fmt::Display> fmt::Display for Separated<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.1.is_empty() {
            return Ok(());
        }

        let mut print_sep = false;
        for arg in self.1 {
            if print_sep {
                write!(f, "{}", self.0)?;
            }
            write!(f, "{}", arg)?;
            print_sep = true;
        }
        Ok(())
    }
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
