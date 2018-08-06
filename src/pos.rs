use core::cmp::min;
use core::fmt;
use core::ops::{ Deref, DerefMut };
use void::Void;

use super::*;

pub struct Pos<T> {
    buf: T,
    pub pos: usize,
}

impl<T> From<T> for Pos<T> {
    #[inline] fn from(x: T) -> Self { Pos { buf: x, pos: 0 } }
}

impl<T> Pos<T> {
    #[inline] pub fn unwrap(self) -> T { self.buf }
}

impl<S: Copy, T: Deref<Target = [S]>> Read<S> for Pos<T> {
    type Err = Void;

    #[inline] fn read(&mut self, buf: &mut [S]) -> Result<usize, Void> {
        let l = min(buf.len(), self.buf.len() - self.pos);
        buf[0..l].copy_from_slice(&self.buf[self.pos..][0..l]);
        self.pos += l;
        Ok(l)
    }
}

impl<S: Copy, T: DerefMut<Target = [S]>> Write<S> for Pos<T> {
    type Err = Void;

    #[inline] fn write(&mut self, buf: &[S]) -> Result<usize, Void> {
        let l = min(buf.len(), self.buf.len() - self.pos);
        self.buf[self.pos..][0..l].copy_from_slice(&buf[0..l]);
        self.pos += l;
        Ok(l)
    }

    #[inline] fn flush(&mut self) -> Result<(), Void> { Ok(()) }
}

impl<T: DerefMut<Target = [u8]>> fmt::Write for Pos<T> {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result { match self.write(s.as_bytes()) {
        Ok(0) if s.len() > 0 => Err(fmt::Error),
        Ok(_) => Ok(()),
        Err(v) => match v {},
    } }
}

#[cfg(test)] mod tests {
    use core::fmt;
    use std;
    use void;

    use ::util::*;
    use super::*;

    fn test_read<T: Copy + Eq + fmt::Debug>(xs: std::vec::Vec<T>) -> bool {
        Iterator::eq(xs.clone().into_iter(),
                     Pos::from(xs).data().map(void::ResultVoidExt::void_unwrap))
    }
    #[quickcheck] fn read_unit(xs: std::vec::Vec<()>) -> bool { test_read(xs) }
    #[quickcheck] fn read_abc(xs: std::vec::Vec<ABC>) -> bool { test_read(xs) }
    #[quickcheck] fn read_u8(xs: std::vec::Vec<u8>) -> bool { test_read(xs) }
    #[quickcheck] fn read_usize(xs: std::vec::Vec<usize>) -> bool { test_read(xs) }
}
