//! Gap buffer implementation for UTF-8 and binary buffers.
//!
//! A gap buffer like the one implemented here is often used in
//! editors where changes are often highly localized.  The buffer
//! maintains a "hole", or gap, which is simply empty space somewhere
//! in the buffer.
//!
//! In the following picture, we have a buffer containing the string
//! "Hällo!".  Note that the string is supposed to be UTF8-encoded,
//! therefore the second character takes up two bytes.
//!
//! ```text
//!             |---gaplen--|
//! +---+---+---+---+---+---+---+---+---+---+
//! | H |   ä   |   |   |   | l | l | o | ! |
//! +---+---+---+---+---+---+---+---+---+---+
//!             |
//!            gap
//! ```
//!
//! We use the following definitions for gap buffers:
//!
//! - The length of the buffer is the length of the underlying vector
//!   minus the gap length (in bytes).
//! - "Moving the gap" means moving the buffer content so that gaplen
//!   bytes following the new gap position are unused.

use std::io;
use std::string::FromUtf8Error;
use std::error;
use std::fmt;

/// Allowed encodings for text content.
///
/// Currently, only UTF-8 is supported, because this is the standard
/// encoding for strings in Rust.
#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum Encoding {
    /// UTF-8 encoded.
    UTF8
}

/// Line ending convention for text content.
#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum LineEnding {
    /// Unix-style LF line endings.
    Lf,
    /// Windows-style CRLF line endings.
    CrLf
}

/// A buffer can be in text or binary mode.  This affects how certain
/// navigation operations work.
#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum Mode {
    /// Text mode for buffers. Buffers can have different encodings
    /// and line endings.
    Text(Encoding, LineEnding),
    /// Binary mode.  Some operations don't work on binary buffers,
    /// such as line navigation.
    Binary
}

/// Errors produced by this module.
#[derive(Debug)]
pub enum Error {
    /// This error is returned whenever an operation is performed that
    /// is only meaningful for buffers in text mode.
    NotInTextMode,
    NotUTF8,
    IoError(io::Error)
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::IoError(err)
    }
}

impl From<FromUtf8Error> for Error {
    fn from(_: FromUtf8Error) -> Error {
        Error::NotUTF8
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::NotInTextMode => write!(f, "buffer not in text mode"),
            Error::NotUTF8 => write!(f, "not in UTF-8"),
            Error::IoError(ref err) => write!(f, "IO error: {}", err)
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::NotInTextMode => "buffer not in text mode",
            Error::NotUTF8 => "not in UTF-8",
            Error::IoError(ref err) => err.description()
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::NotUTF8 => None,
            Error::NotInTextMode => None,
            Error::IoError(ref err) => Some(err)
        }
    }
}

/// Gap buffer implementation.
#[derive(Debug)]
pub struct Buf {
    /// Mode of the buffer.
    mode: Mode,
    /// Underlying buffer for storing the buffer contents.
   buf: Vec<u8>,
    /// Position of the gap in the buffer.
    gap: usize,
    /// Length of the gap.
    gaplen: usize
}

const DEFAULT_GAPLEN: usize = 128;

impl Buf {
    /// Create a new empty buffer with the provided mode.
    ///
    /// # Example
    ///
    /// ```
    /// let mode = Mode::Text(Encoding::UTF8, LineEnding::Lf);
    /// let buf = Buf::new(mode);
    /// assert_eq!(buf.len(), 0);
    /// assert_eq!(buf.mode, mode);
    /// assert_eq!(buf.to_string(), "");
    /// ```
    pub fn new(mode: Mode) -> Self
    {
        Buf {
            mode: mode,
            buf: Vec::new(),
            gap: 0,
            gaplen: 0
        }
    }

    /// Create a new buffer which contains a copy of the given string.
    /// The buffer will be in UTF-8-encoded text mode, with the given
    /// line ending convention.
    ///
    /// # Example
    ///
    /// ```
    /// let le = LineEnding::CrLf;
    /// let buf = Buf::from_str("Überfjäll", le);
    /// assert_eq!(buf.len(), 11);
    /// assert_eq!(buf.mode, Mode::Text(Encoding::UTF8, le));
    /// assert_eq!(buf.to_string(), "Überfjäll");
    /// ```
    pub fn from_str(s: &str, le: LineEnding) -> Self {
        let mut buf = Vec::new();
        buf.extend(s.bytes());
        Buf {
            mode: Mode::Text(Encoding::UTF8, le),
            buf: buf,
            gap: 0,
            gaplen: 0
        }
    }

    /// Create a new buffer with the given capacity and the given
    /// mode.
    ///
    /// # Example
    ///
    /// ```
    /// let buf = Buf::with_capacity(32, Mode::Binary);
    /// assert_eq!(buf.len(), 0);
    /// assert_eq!(buf.mode, Mode::Binary);
    /// assert_eq!(buf.to_string(), "");
    /// ```
    pub fn with_capacity(capacity: usize, mode: Mode) -> Self {
        Buf {
            mode: mode,
            buf: Vec::with_capacity(capacity),
            gap: 0,
            gaplen: 0
        }
    }

    /// Create a new buffer with the given capacity and the given
    /// mode.
    ///
    /// # Example
    ///
    /// ```
    /// let le = LineEnding::CrLf;
    /// let buf = Buf::from_str("Contents", le);
    /// assert_eq!(buf.len(), 8);
    /// ```
    pub fn len(&self) -> usize {
        self.buf.len() - self.gaplen
    }

    /// Insert the contents of string `s` at position `pos`.
    ///
    /// # Panics
    /// Panics when `pos` is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// let le = LineEnding::CrLf;
    /// let mut buf = Buf::from_str("Überfjäll", le);
    /// assert_eq!(buf.len(), 11);
    ///
    /// buf.insert_str("tjord", 5);
    /// assert_eq!(buf.len(), 16);
    /// assert_eq!(buf.to_string(), Ok("Übertjordfjäll".to_string()));
    ///
    /// buf.insert_str("bJoll", 0);
    /// assert_eq!(buf.len(), 21);
    /// assert_eq!(buf.to_string(), Ok("bJollÜbertjordfjäll".to_string()));
    ///
    /// let l = buf.len();
    /// buf.insert_str("Twaanrö", l);
    /// assert_eq!(buf.len(), 29);
    /// assert_eq!(buf.to_string(), Ok("bJollÜbertjordfjällTwaanrö".to_string()));
    /// ```
    pub fn insert_str(&mut self, s: &str, pos: usize) {
        let l = s.len();
        self.mkgap(pos, l);
        let mut i = pos;
        for b in s.bytes() {
            self.buf[i] = b;
            i += 1;
        }
        self.gap += l;
        self.gaplen -= l;
    }

    pub fn insert_all<I>(&mut self, it: I, pos: usize)
        where I: Iterator<Item=u8> {
        let mut i = pos;
        for b in it {
            if self.gaplen < DEFAULT_GAPLEN {
                self.mkgap(i, DEFAULT_GAPLEN);
            }
            self.buf[i] = b;
            i += 1;
            self.gap += 1;
            self.gaplen -= 1;
        }
    }

    /// Delete `cnt` bytes at position `at`.
    ///
    /// # Panics
    /// Panics when `at` or `at+cnt` is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// let le = LineEnding::CrLf;
    /// let mut buf = Buf::from_str("Überfjäll", le);
    /// assert_eq!(buf.len(), 11);
    ///
    /// buf.delete(7, 3);
    /// assert_eq!(buf.len(), 8);
    /// assert_eq!(buf.to_string(), Ok("Überfjl".to_string()));
    /// ```
    pub fn delete(&mut self, at: usize, cnt: usize) {
        self.movegap(at);
        self.gaplen += cnt;
    }

    /// Make sure that the buffer has a gap at position `ngap`, of at
    /// least `ngaplen` bytes.
    ///
    /// # Panics
    /// Panics when the new gap position is out of bounds or the
    /// buffer size overflows.
    fn mkgap(&mut self, ngap: usize, ngaplen: usize) {
        self.growgap(ngaplen);
        self.movegap(ngap);
    }

    /// Ensure that the gap is at least `ngaplen` bytes large.  The
    /// gap position is not changed.
    ///
    /// # Panics
    /// Panics when the buffer size overflows.
    ///
    fn growgap(&mut self, ngaplen: usize) {
        if ngaplen > self.gaplen {
            self.buf.reserve(ngaplen - self.gaplen);
            for _ in 0..ngaplen-self.gaplen {
                self.buf.insert(self.gap, 0);
            }
            self.gaplen = ngaplen;
        }
    }

    /// Move the gap to position `ngap`.
    ///
    /// # Panics
    /// Panics when `ngap` is out of bounds.
    fn movegap(&mut self, ngap: usize) {
        if ngap < self.gap {
            let movecnt = self.gap - ngap;
            for i in 0..movecnt {
                self.buf[self.gap + self.gaplen - 1 - i] = self.buf[self.gap - i - 1];
            }
            self.gap = ngap;
        } else if ngap > self.gap {
            for i in self.gap..ngap {
                self.buf[i] = self.buf[self.gaplen + i];
            }
            self.gap = ngap;

        } else {
            return;
        }
        // The following helps with debugging, but could be
        // optimized out as the bytes at buf[gap..gap+gaplen] must
        // never be read.
        for i in 0..self.gaplen {
            self.buf[self.gap + i] = 0;
        }
    }

    /// Return the contents of a buffer as a string, if it is in
    /// UTF-8-encoded text mode.  Returns an error if the buffer
    /// contents cannot be converted to a string.
    ///
    /// # Example
    ///
    /// ```
    /// let le = LineEnding::CrLf;
    /// let buf = Buf::from_str("Contents", le).unwrap();
    /// assert_eq!(buf.len(), 8);
    /// ```
    pub fn to_string(&self) -> Result<String, Error> {
        match self.mode {
            Mode::Text(Encoding::UTF8, _) => {
                let mut v = Vec::new();
                v.extend(&self.buf[0..self.gap]);
                v.extend(&self.buf[self.gap+self.gaplen..self.buf.len()]);
                match String::from_utf8(v) {
                    Ok(s) => Ok(s),
                    Err(_) => Err(Error::NotUTF8)
                }
            }
            _ => Err(Error::NotInTextMode)
        }
    }

    /// Map an offset into the logical buffer, `p`, to an offset in
    /// the gap buffer.  That means that the returned offset can be
    /// used to directly access the underlying vector.
    fn index_of(&self, p: usize) -> usize {
        if p < self.gap {
            p
        } else {
            p + self.gaplen
        }
    }

    /// Return an iterator over the bytes stored in the buffer.
    ///
    /// # Example
    ///
    /// ```
    /// let le = LineEnding::CrLf;
    /// let buf = Buf::from_str("Überfjäll", le);
    /// let mut v = Vec::new();
    /// for b in buf.bytes() {
    ///    v.push(b);
    /// }
    /// assert_eq!(v, vec![195, 156, 98, 101, 114, 102, 106, 195, 164, 108, 108]);
    /// ```
    pub fn bytes(&self) -> Bytes {
        Bytes {
            buf: self,
            pos: 0
        }
    }
}

/// Iterator over the bytes of a buffer.
pub struct Bytes<'a> {
    buf: &'a Buf,
    pos: usize
}

/// Iterator implementation.
impl<'a> Iterator for Bytes<'a> {
    type Item = u8;
    fn next(&mut self) -> Option<u8> {
        if self.pos < self.buf.len() {
            let c = self.buf.buf[self.buf.index_of(self.pos)];
            self.pos += 1;
            Some(c)
        } else {
            None
        }
    }
}

fn main() {
    let mut buf2 = Buf::from_str("Hällo!\n", LineEnding::Lf);
    println!("{:#?} {}", buf2, buf2.len());
    println!("{}", buf2.to_string().unwrap());

    buf2.insert_str("NEW", 1);
    println!("{:#?} {}", buf2, buf2.len());
    println!("{}", buf2.to_string().unwrap());
}

mod test {
    #[test]
    fn new() {
        use super::{Mode, Encoding, LineEnding, Buf};
        let mode = Mode::Text(Encoding::UTF8, LineEnding::Lf);
        let buf = Buf::new(mode);
        assert_eq!(buf.len(), 0);
        assert_eq!(buf.mode, mode);
        assert_eq!(buf.to_string().unwrap(), "".to_string());
    }

    #[test]
    fn from_str() {
        use super::{Mode, Encoding, LineEnding, Buf};
        let le = LineEnding::CrLf;
        let buf = Buf::from_str("Überfjäll", le);
        assert_eq!(buf.len(), 11);
        assert_eq!(buf.mode, Mode::Text(Encoding::UTF8, le));
        assert_eq!(buf.to_string().unwrap(), "Überfjäll".to_string());
    }

    #[test]
    fn with_capacity() {
        use super::{Mode, Buf};
        let buf = Buf::with_capacity(32, Mode::Binary);
        assert_eq!(buf.len(), 0);
        assert_eq!(buf.mode, Mode::Binary);
        assert!(buf.to_string().is_err());
    }

    #[test]
    fn bytes() {
        use super::{LineEnding, Buf};
        let le = LineEnding::CrLf;
        let buf = Buf::from_str("Überfjäll", le);
        let mut v = Vec::new();
        for b in buf.bytes() {
            v.push(b);
        }
        assert_eq!(v, vec![195, 156, 98, 101, 114, 102, 106, 195, 164, 108, 108]);
    }

    #[test]
    fn insert_str() {
        use super::{LineEnding, Buf};
        let le = LineEnding::CrLf;
        let mut buf = Buf::from_str("Überfjäll", le);
        assert_eq!(buf.len(), 11);

        buf.insert_str("tjord", 5);
        assert_eq!(buf.len(), 16);
        assert_eq!(buf.to_string().unwrap(), "Übertjordfjäll".to_string());

        buf.insert_str("bJoll", 0);
        assert_eq!(buf.len(), 21);
        assert_eq!(buf.to_string().unwrap(), "bJollÜbertjordfjäll".to_string());

        let l = buf.len();
        buf.insert_str("Twaanrö", l);
        assert_eq!(buf.len(), 29);
        assert_eq!(buf.to_string().unwrap(), "bJollÜbertjordfjällTwaanrö".to_string());
    }

    #[test]
    fn insert_all() {
        use super::{Mode, Encoding, LineEnding, Buf};
        let mode = Mode::Text(Encoding::UTF8, LineEnding::Lf);
        let mut buf = Buf::new(mode);

        buf.insert_all("Hallo ällerseits!".to_string().bytes(), 0);
        assert_eq!(buf.to_string().unwrap(), "Hallo ällerseits!".to_string());

        buf.insert_all("auch ".to_string().bytes(), 6);
        assert_eq!(buf.to_string().unwrap(), "Hallo auch ällerseits!".to_string());

        buf.insert_all("o_O".to_string().bytes(), 23);
        assert_eq!(buf.to_string().unwrap(), "Hallo auch ällerseits!o_O".to_string());
    }

    #[test]
    fn delete() {
        use super::{LineEnding, Buf};
        let le = LineEnding::CrLf;
        let mut buf = Buf::from_str("Überfjäll", le);
        assert_eq!(buf.len(), 11);

        buf.delete(7, 3);
        assert_eq!(buf.len(), 8);
        assert_eq!(buf.to_string().unwrap(), "Überfjl".to_string());

        buf.delete(0, 2);
        assert_eq!(buf.len(), 6);
        assert_eq!(buf.to_string().unwrap(), "berfjl".to_string());

        let p = buf.len() - 1;
        buf.delete(p, 1);
        assert_eq!(buf.len(), 5);
        assert_eq!(buf.to_string().unwrap(), "berfj".to_string());
    }
}
