// Gap buffer implementation for UTF8 buffers.

#[derive(Debug)]
pub enum Encoding {
    UTF8
}

#[derive(Debug)]
pub enum LineEnding {
    Lf,
    CrLf,
    Cr
}

#[derive(Debug)]
pub enum Mode {
    Text(Encoding, LineEnding),
    Binary
}

#[derive(Debug)]
pub struct Buf {
    mode: Mode,
    buf: Vec<u8>,
    gap: usize,
    gaplen: usize
}

impl Buf {
    pub fn new(mode: Mode) -> Self
    {
        Buf {
            mode: mode,
            buf: Vec::new(),
            gap: 0,
            gaplen: 0
        }
    }

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

    pub fn with_capacity(capacity: usize, mode: Mode) -> Self {
        Buf {
            mode: mode,
            buf: Vec::with_capacity(capacity),
            gap: 0,
            gaplen: 0
        }
    }

    pub fn len(&self) -> usize {
        self.buf.len() - self.gaplen
    }

    pub fn mkgap(&mut self, ngap: usize, ngaplen: usize) {
        self.growgap(ngaplen);
        self.movegap(ngap);
    }

    fn growgap(&mut self, ngaplen: usize) {
        if ngaplen > self.gaplen {
            self.buf.reserve(ngaplen - self.gaplen);
            for _ in 0..ngaplen-self.gaplen {
                self.buf.insert(self.gap, 0);
            }
            self.gaplen = ngaplen;
        }
    }

    //                  gap
    //             |-----3-----|
    // +---+---+---+---+---+---+---+---+---+---+
    // | H |   ä   |   |   |   | l | l | o | ! |
    // +---+---+---+---+---+---+---+---+---+---+
    //     | ngap
    //
    //      gap
    // |-----3-----|
    // +---+---+---+---+---+---+---+---+---+---+
    // |   |   |   | H |   ä   | l | l | o | ! |
    // +---+---+---+---+---+---+---+---+---+---+
    //     | ngap
    fn movegap(&mut self, ngap: usize) {
        if ngap < self.gap {
            let movecnt = self.gap - ngap;
            println!("gap: {}, ngap: {}", self.gap, ngap);
            for i in 0..movecnt {
                self.buf[self.gap + self.gaplen - 1 - i] = self.buf[self.gap - i - 1];
            }
            self.gap = ngap;

            // The following helps with debugging, but could be
            // optimized out as the bytes at buf[gap..gap+gaplen] must
            // never be read.
            for i in 0..self.gaplen {
                self.buf[self.gap + i] = 0;
            }
        } else if ngap > self.gap {
            for i in self.gap..ngap {
                self.buf[i] = self.buf[self.gaplen + i];
            }
            self.gap = ngap;

            // The following helps with debugging, but could be
            // optimized out as the bytes at buf[gap..gap+gaplen] must
            // never be read.
            for i in 0..self.gaplen {
                self.buf[self.gap + i] = 0;
            }
        }
    }

    pub fn to_string(&self) -> String {
        let mut v = Vec::new();
        v.extend(&self.buf[0..self.gap]);
        v.extend(&self.buf[self.gap+self.gaplen..self.buf.len()]);
        String::from_utf8(v).unwrap()
    }
}

fn main() {
    let buf0 = Buf::new(Mode::Binary);
    println!("{:?} {}", buf0, buf0.len());

    let buf1 = Buf::with_capacity(32, Mode::Binary);
    println!("{:?} {}", buf1, buf1.len());

    let mut buf2 = Buf::from_str("Hällo!\n", LineEnding::Lf);
    println!("{:?} {}", buf2, buf2.len());
    println!("{}", buf2.to_string());

    buf2.mkgap(3, 4);
    println!("{:?} {}", buf2, buf2.len());
    println!("{}", buf2.to_string());
    buf2.mkgap(0, 5);
    println!("{:?} {}", buf2, buf2.len());
    println!("{}", buf2.to_string());
    buf2.mkgap(3, 4);
    println!("{:?} {}", buf2, buf2.len());
    println!("{}", buf2.to_string());
    buf2.mkgap(7, 5);
    println!("{:?} {}", buf2, buf2.len());
    println!("{}", buf2.to_string());
}
