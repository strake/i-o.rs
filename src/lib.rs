//! Input/Output

#![no_std]

#![cfg_attr(test, feature(plugin))]
#![cfg_attr(test, plugin(quickcheck_macros))]

extern crate containers;
extern crate void;

#[cfg(test)] extern crate quickcheck;
#[cfg(test)] extern crate std;

use containers::collections::Vec;
use core::cmp::max;
use core::convert::From;
use core::marker::PhantomData;
use core::mem;
use core::ops::*;
use void::Void;

mod pos;
pub use pos::Pos;

mod util;

#[derive(Debug)] pub struct EndOfFile;
#[derive(Debug)] pub struct NoMemory;

pub trait TryRead<T: Copy> {
    type Err;

    /// `r.try_read(buf) = r.try_readv(&mut [buf])`
    #[inline]
    fn try_read(&mut self, buf: &mut [T]) -> Result<Result<usize, EndOfFile>, Self::Err> { self.try_readv(&mut [buf]) }

    /// Pull some data, at most `bufs.fold(0, |n, buf| n+buf.len())`, from this source into given buffers; return how many data were actually read, or a failure.
    /// If no data can be read when called, return `0` rather than wait.
    #[inline]
    fn try_readv(&mut self, bufs: &mut [&mut [T]]) -> Result<Result<usize, EndOfFile>, Self::Err> {
        for buf in bufs { if buf.len() > 0 { return self.try_read(buf) } }
        Ok(Ok(0))
    }
}

pub trait Read<T: Copy> {
    type Err;

    /// `r.read(buf) = self.readv(&mut [buf])`
    #[inline]
    fn read(&mut self, buf: &mut [T]) -> Result<usize, Self::Err> { self.readv(&mut [buf]) }

    /// Pull some data, at most `bufs.fold(0, |n, buf| n+buf.len())`, from this source into given buffers; return how many data were actually read, or a failure.
    /// May block if no data can be read when called.
    ///
    /// If this returns 0 data read, the possibilities are these:
    ///
    /// * `bufs.all(|buf| buf.len() == 0)`
    /// * The reader reached some end, and can unlikely produce further bytes, at least temporarily.
    #[inline]
    fn readv(&mut self, bufs: &mut [&mut [T]]) -> Result<usize, Self::Err> {
        for buf in bufs { if buf.len() > 0 { return self.read(buf) } }
        Ok(0)
    }

    /// Pull `buf.len()` data from this source into given buffer; return `()` if so many data were actually read, or a failure and how many data were read before the failure.
    #[inline]
    fn read_full<E: From<Self::Err> + From<EndOfFile>>(&mut self, buf: &mut [T]) -> Result<(), (E, usize)> {
        let mut n = 0;
        while n < buf.len() {
            match self.read(&mut buf[n..]) {
                Err(e) => return Err((E::from(e), n)),
                Ok(0) => return Err((E::from(EndOfFile), n)),
                Ok(m) => n += m,
            }
        }
        Ok(())
    }

    /// Return bounds on number of data ready to read.
    ///
    /// The default returns `(0 None)` which is never wrong.
    #[inline] fn size_hint(&self) -> (usize, Option<usize>) { (0, None) }

    /// Make an `Iterator` over the data of this reader.
    #[inline] fn data(self) -> Data<Self, T> where Self: Sized { Data(PhantomData, self) }

    /// Pull data from this source into the spare storage of `xs`, and modify its length to include the data read.
    /// If this fails, `xs` is unmodified.
    #[inline]
    fn read_onto_vec(&mut self, xs: &mut Vec<T>) -> Result<(), Self::Err> {
        let l = xs.len();
        let m = try!(self.read(unsafe { &mut xs.storage_mut()[l..] }));
        unsafe { xs.set_length(l+m) };
        Ok(())
    }

    #[inline]
    fn split<P: FnMut(T) -> bool, E: From<Self::Err> + From<NoMemory>>(self, p: P, keep_delim: bool) -> Split<Self, T, P, E> where Self: Sized {
        Split{ φ: PhantomData, r: self, p: p, buf: Some(Vec::new()), keep_delim: keep_delim }
    }
}

pub trait PosRead<T: Copy>: Read<T> {
    /// `r.pread(buf, pos) = r.preadv(&mut [buf], pos)`
    #[inline]
    fn pread(&mut self, buf: &mut [T], pos: usize) -> Result<usize, Self::Err> { self.preadv(&mut [buf], pos) }

    /// Pull some data, at most `bufs.fold(0, |n, buf| n+buf.len())`, from this source at given position into given buffers; return how many data were actually read, or a failure.
    /// May block if no data can be read when called.
    ///
    /// If this returns 0 data read, the possibilities are these:
    ///
    /// * `bufs.all(|buf| buf.len() == 0)`
    /// * The reader reached some end, and can unlikely produce further bytes, at least temporarily.
    #[inline]
    fn preadv(&mut self, bufs: &mut [&mut [T]], pos: usize) -> Result<usize, Self::Err> {
        for buf in bufs { if buf.len() > 0 { return self.pread(buf, pos) } }
        Ok(0)
    }

    /// Pull `buf.len()` data from this source at given position into given buffer; return `()` if so many data were actually read, or a failure and how many data were read before the failure.
    #[inline]
    fn pread_full<E: From<Self::Err> + From<EndOfFile>>(&mut self, buf: &mut [T], mut pos: usize) -> Result<(), (E, usize)> {
        let mut n = 0;
        while n < buf.len() {
            match self.pread(&mut buf[n..], pos) {
                Err(e) => return Err((E::from(e), n)),
                Ok(0) => return Err((E::from(EndOfFile), n)),
                Ok(m) => {
                    n += m;
                    pos += m;
                },
            }
        }
        Ok(())
    }
}

pub struct Data<R: Read<T>, T: Copy>(PhantomData<T>, R);

impl<R: Read<T>, T: Copy> Iterator for Data<R, T> {
    type Item = Result<T, R::Err>;

    #[inline] fn next(&mut self) -> Option<Self::Item> {
        let mut buf: [T; 1] = unsafe { mem::uninitialized() };
        match self.1.read(&mut buf) {
            Err(e) => Some(Err(e)),
            Ok(0) => None,
            Ok(_) => Some(Ok(buf[0])),
        }
    }
}

impl<R: Read<T>, T: Copy> Deref for Data<R, T> {
    type Target = R;
    #[inline]
    fn deref(&self) -> &R { &self.1 }
}

impl<R: Read<T>, T: Copy> DerefMut for Data<R, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut R { &mut self.1 }
}

pub struct Split<R: Read<T>, T: Copy, P: FnMut(T) -> bool, E: From<R::Err> + From<NoMemory>> {
    φ: PhantomData<E>,
    pub r: R,
    p: P,
    pub buf: Option<Vec<T>>,
    keep_delim: bool,
}

impl<R: Read<T>, T: Copy, P: FnMut(T) -> bool, E: From<R::Err> + From<NoMemory>> Iterator for Split<R, T, P, E> {
    type Item = Result<Vec<T>, E>;

    #[inline] fn next(&mut self) -> Option<Self::Item> {
        let mut buf = match self.buf.take() { Some(buf) => buf, None => return None, };
        let p = &mut self.p;
        loop {
            if let Some(n) = buf.iter().position(|&x| p(x)) {
                return match buf.split_off(n+1) {
                    None => {
                        self.buf = Some(buf);
                        Some(Err(E::from(NoMemory)))
                    },
                    Some(xs) => {
                        self.buf = Some(xs);
                        if !self.keep_delim { buf.pop(); }
                        Some(Ok(buf))
                    },
                }
            }
            let l = buf.len();
            if !buf.reserve(max(l, 1)) {
                self.buf = Some(buf);
                return Some(Err(E::from(NoMemory)));
            }
            match self.r.read_onto_vec(&mut buf) {
                Err(e) => {
                    self.buf = Some(buf);
                    return Some(Err(E::from(e)));
                },
                Ok(()) => if buf.len() == l { return Some(Ok(buf)) },
            }
        }
    }
}

pub trait TryWrite<T: Copy> {
    type Err;

    /// `w.try_write(buf) = w.try_writev(&[buf])`
    #[inline]
    fn try_write(&mut self, buf: &[T]) -> Result<Result<usize, EndOfFile>, Self::Err> { self.try_writev(&[buf]) }

    /// Push some data, at most `bufs.fold(0, |n, buf| n+buf.len())`, to this sink from given buffers; return how many data were actually written, or a failure.
    /// If no data can be written when called, return `0` rather than wait.
    #[inline]
    fn try_writev(&mut self, bufs: &[&[T]]) -> Result<Result<usize, EndOfFile>, Self::Err> {
        for buf in bufs { if buf.len() > 0 { return self.try_write(buf) } }
        Ok(Ok(0))
    }
}

pub trait Write<T: Copy> {
    type Err;

    /// `w.write(buf) = self.writev(&[buf])`
    #[inline]
    fn write(&mut self, buf: &[T]) -> Result<usize, Self::Err> { self.writev(&[buf]) }

    /// Push some data, at most `bufs.fold(0, |n, buf| n+buf.len())`, to this sink from given buffers; return how many data were actually written, or a failure.
    /// May block if no data can be written when called.
    ///
    /// If this returns 0 data written, the possibilities are these:
    ///
    /// * `bufs.all(|buf| buf.len() == 0)`
    /// * The writer reached some end, and can unlikely consume further bytes, at least temporarily.
    #[inline]
    fn writev(&mut self, bufs: &[&[T]]) -> Result<usize, Self::Err> {
        for buf in bufs { if buf.len() > 0 { return self.write(buf) } }
        Ok(0)
    }

    fn flush(&mut self) -> Result<(), Self::Err>;

    /// Push `buf.len()` data to this sink from given buffer; return `()` if so many data were actually written, or a failure and how many data were written before the failure.
    #[inline]
    fn write_all(&mut self, buf: &[T]) -> Result<(), (Self::Err, usize)> where Self::Err: From<EndOfFile> {
        let mut n = 0;
        while n < buf.len() {
            match self.write(&buf[n..]) {
                Err(e) => return Err((e, n)),
                Ok(0) => return Err((Self::Err::from(EndOfFile), n)),
                Ok(m) => n += m,
            }
        }
        Ok(())
    }
}

pub trait PosWrite<T: Copy> : Write<T> {
    /// `w.pwrite(buf, pos) = w.pritev(&[buf], pos)`
    fn pwrite(&mut self, buf: &[T], pos: usize) -> Result<usize, Self::Err> { self.pwritev(&[buf], pos) }

    /// Push some data, at most `bufs.fold(0, |n, buf| n+buf.len())`, to this sink at given position from given buffers; return how many data were actually written, or a failure.
    /// May block if no data can be written when called.
    ///
    /// If this returns 0 data written, the possibilities are these:
    ///
    /// * `bufs.all(|buf| buf.len() == 0)`
    /// * The writer reached some end, and can unlikely consume further bytes, at least temporarily.
    fn pwritev(&mut self, bufs: &[&[T]], pos: usize) -> Result<usize, Self::Err> {
        for buf in bufs { if buf.len() > 0 { return self.pwrite(buf, pos) } }
        Ok(0)
    }

    /// Push `buf.len()` data to this sink at given position from given buffer; return `()` if so many data were actually written, or a failure and how many data were written before the failure.
    #[inline] fn pwrite_all(&mut self, buf: &[T], mut pos: usize) -> Result<(), (Self::Err, usize)> where Self::Err: From<EndOfFile> {
        let mut n = 0;
        while n < buf.len() {
            match self.pwrite(&buf[n..], pos) {
                Err(e) => return Err((e, n)),
                Ok(0) => return Err((Self::Err::from(EndOfFile), n)),
                Ok(m) => {
                    n += m;
                    pos += m;
                },
            }
        }
        Ok(())
    }
}

impl<'a, T: Copy, R: ?Sized + DerefMut> Read<T> for R where R::Target: Read<T> {
    type Err = <R::Target as Read<T>>::Err;

    #[inline]
    fn read(&mut self, buf: &mut [T]) -> Result<usize, Self::Err> {
        R::Target::read(self.deref_mut(), buf)
    }

    #[inline]
    fn readv(&mut self, buf: &mut [&mut [T]]) -> Result<usize, Self::Err> {
        R::Target::readv(self.deref_mut(), buf)
    }
}

impl<'a, T: Copy, W: ?Sized + DerefMut> Write<T> for W where W::Target: Write<T> {
    type Err = <W::Target as Write<T>>::Err;

    #[inline]
    fn write(&mut self, buf: &[T]) -> Result<usize, Self::Err> {
        W::Target::write(self.deref_mut(), buf)
    }

    #[inline]
    fn writev(&mut self, buf: &[&[T]]) -> Result<usize, Self::Err> {
        W::Target::writev(self.deref_mut(), buf)
    }

    #[inline]
    fn flush(&mut self) -> Result<(), Self::Err> {
        W::Target::flush(self.deref_mut())
    }
}

// Void is actually initial in the category of types, but alas, we can not
// impl<T> From<Void> for T
impl From<Void> for EndOfFile { fn from(void: Void) -> Self { match void {} } }
impl From<Void> for NoMemory  { fn from(void: Void) -> Self { match void {} } }

#[cfg(test)] mod tests {
    use std;
    use std::iter::Iterator;

    use containers::collections::Vec;
    use ::util::*;
    use super::*;
    use super::pos::*;

    fn test_split_singleton<T: Copy + Eq>(xs: std::vec::Vec<T>, keep_delim: bool) -> bool {
        Iterator::eq(xs.clone().into_iter(),
                     Pos::from(xs).split(|_| false, keep_delim).map(Result::<_, NoMemory>::unwrap).next().unwrap().into_iter())
    }
    #[quickcheck] fn split_singleton_unit(xs: std::vec::Vec<()>, keep_delim: bool) -> bool { test_split_singleton(xs, keep_delim) }
    #[quickcheck] fn split_singleton_ABC(xs: std::vec::Vec<ABC>, keep_delim: bool) -> bool { test_split_singleton(xs, keep_delim) }

    fn test_split_count<T: Copy + Eq>(xs: std::vec::Vec<T>, y: T, keep_delim: bool) -> bool {
        xs.clone().into_iter().filter(|&x| x == y).count() + 1 == Pos::from(xs).split(|x| x == y, keep_delim).map(Result::<_, NoMemory>::unwrap).count()
    }
    #[quickcheck] fn split_count_unit(xs: std::vec::Vec<()>, keep_delim: bool) -> bool { test_split_count(xs, (), keep_delim) }
    #[quickcheck] fn split_count_abc(xs: std::vec::Vec<ABC>, y: ABC, keep_delim: bool) -> bool { test_split_count(xs, y, keep_delim) }

    fn test_split_concatenation<T: Copy + Eq>(xs: std::vec::Vec<T>, y: T) -> bool {
        Iterator::eq(xs.clone().into_iter(),
                     Pos::from(xs).split(|x| x == y, true).map(Result::<_, NoMemory>::unwrap).flat_map(Vec::into_iter))
    }
    #[quickcheck] fn split_concatenation_unit(xs: std::vec::Vec<()>) -> bool { test_split_concatenation(xs, ()) }
    #[quickcheck] fn split_concatenation_abc(xs: std::vec::Vec<ABC>, y: ABC) -> bool { test_split_concatenation(xs, y) }
}
