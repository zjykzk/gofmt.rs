//! This crate provides an implementation of
//! [elastic tabstops](http://nickgravgaard.com/elastictabstops/index.html).
//! It is a minimal port of Go's
//! [tabwriter](http://golang.org/pkg/text/tabwriter/) package.
//! Namely, its main mode of operation is to wrap a `Writer` and implement
//! elastic tabstops for the text written to the wrapped `Writer`.

#![deny(missing_docs)]

use std::cmp;
use std::error;
use std::fmt;
use std::io;
use std::iter;
use std::str;

/// TabWriter wraps an arbitrary writer and aligns tabbed output.
///
/// Elastic tabstops work by aligning *contiguous* tabbed delimited fields
/// known as *column blocks*. When a line appears that breaks all contiguous
/// blocks, all buffered output will be flushed to the underlying writer.
/// Otherwise, output will stay buffered until `flush` is explicitly called.
#[derive(Debug)]
pub(crate) struct TabWriter<'a, W> {
    pub(crate) w: W,
    lines: Vec<Vec<Cell<'a>>>,
    minwidth: usize,
    padding: usize,
    padchar: char,
    flags: u32,
    alignment: Alignment,
    tab_indent: bool,
    tabwidth: usize,
    ignore_empty_column: bool,
}

/// `Alignment` represents how a `TabWriter` should align text within its cell.
#[derive(Debug)]
pub enum Alignment {
    /// Text should be aligned with the left edge of the cell
    Left,
    /// Text should be centered within the cell
    Center,
    /// Text should be aligned with the right edge of the cell
    Right,
}

#[derive(Debug)]
struct Cell<'a> {
    width: usize, // in characters
    data: &'a [u8],
    htab: bool,
    contains_escape: bool,
}

impl<'a, W: io::Write> TabWriter<'a, W> {
    /// Create a new `TabWriter` from an existing `Writer`.
    ///
    /// All output written to `Writer` is passed through `TabWriter`.
    /// Contiguous column blocks indicated by tabs are aligned.
    ///
    /// Note that `flush` must be called to guarantee that `TabWriter` will
    /// write to the given writer.
    pub(crate) fn new(w: W) -> TabWriter<'a, W> {
        TabWriter {
            w,
            lines: vec![vec![]],
            minwidth: 2,
            padding: 2,
            alignment: Alignment::Left,
            tab_indent: false,
            tabwidth: 0,
            padchar: ' ',
            flags: 0,
            ignore_empty_column: false,
        }
    }

    pub fn tabwidth(mut self, tabwidth: usize) -> TabWriter<'a, W> {
        self.tabwidth = tabwidth;
        self
    }

    pub fn flags(mut self, flags: u32) -> TabWriter<'a, W> {
        self.flags = flags;
        if flags & ALIGN_RIGHT != 0 {
            self.alignment(Alignment::Right)
        } else {
            self
        }
    }

    /// Set the minimum width of each column. That is, all columns will have
    /// *at least* the size given here. If a column is smaller than `minwidth`,
    /// then it is padded with spaces.
    ///
    /// The default minimum width is `2`.
    pub fn minwidth(mut self, minwidth: usize) -> TabWriter<'a, W> {
        self.minwidth = minwidth;
        self
    }

    pub fn padchar(mut self, padchar: char) -> TabWriter<'a, W> {
        self.padchar = padchar;
        self
    }

    /// Set the padding between columns. All columns will be separated by
    /// *at least* the number of spaces indicated by `padding`. If `padding`
    /// is zero, then columns may run up against each other without any
    /// separation.
    ///
    /// The default padding is `2`.
    pub fn padding(mut self, padding: usize) -> TabWriter<'a, W> {
        self.padding = padding;
        self
    }

    /// Set the alignment of text within cells. This will effect future flushes.
    ///
    /// The default alignment is `Alignment::Left`.
    pub fn alignment(mut self, alignment: Alignment) -> TabWriter<'a, W> {
        self.alignment = alignment;
        self
    }

    /// Always use tabs for indentation columns (i.e., padding of
    /// leading empty cells on the left).
    ///
    /// This is disabled by default.
    pub fn tab_indent(mut self, yes: bool) -> TabWriter<'a, W> {
        self.tab_indent = yes;
        self
    }

    pub(crate) fn ignore_empty_column(mut self) -> TabWriter<'a, W> {
        self.ignore_empty_column = true;
        self
    }

    /// Unwraps this `TabWriter`, returning the underlying writer.
    ///
    /// This internal buffer is flushed before returning the writer. If the
    /// flush fails, then an error is returned.
    pub fn into_inner(mut self) -> Result<W, IntoInnerError<TabWriter<'a, W>>> {
        match self.flush() {
            Ok(()) => Ok(self.w),
            Err(err) => Err(IntoInnerError(self, err)),
        }
    }

    fn reset(&mut self) {
        self.lines = vec![vec![]];
    }
}

const TAB: char = '\t';
const VTAB: char = '\x0B';
const NEWLINE: char = '\n';
const FORM_FEED: char = '\x0C';

impl<'a, W: io::Write> TabWriter<'a, W> {
    pub(crate) fn write(&mut self, buf: &'a [u8]) -> io::Result<usize> {
        let mut it = CharIndices::new(buf);

        let mut width: usize = 0;
        let mut start_offset: usize = 0;
        let mut escape_count = 0;

        while let Some((offset, c)) = it.next() {
            match c {
                TAB | VTAB | NEWLINE | FORM_FEED if escape_count & 1 == 0 => {
                    let i = self.lines.len() - 1;
                    self.lines[i].push(Cell {
                        width,
                        data: &buf[start_offset..offset],
                        htab: c == TAB,
                        contains_escape: escape_count != 0,
                    });
                    start_offset = offset + 1;
                    width = 0;
                    if c == NEWLINE || c == FORM_FEED {
                        let ncell = self.lines[i].len();
                        if ncell == 1 || c == FORM_FEED {
                            self.flush()?;
                        }
                        self.lines.push(vec![]); // new line
                    }
                }
                ESCAPE_CHAR => {
                    escape_count += 1;
                }
                _ => width += c.width().unwrap_or(1),
            }
        }

        if start_offset < buf.len() {
            let i = self.lines.len() - 1;
            self.lines[i].push(Cell {
                width,
                data: &buf[start_offset..],
                htab: false,
                contains_escape: escape_count != 0,
            });
        }
        self.flush()?;
        Ok(buf.len())
    }

    fn dump(&self) {
        for (i, c) in self.lines.iter().enumerate() {
            print!("({})", i);
            for c0 in c {
                print!("[{}]", unsafe { str::from_utf8_unchecked(c0.data) });
            }
            print!("\n");
        }
        print!("\n");
    }

    fn cell_widths(&self) -> Vec<Vec<usize>> {
        // Naively, this algorithm looks like it could be O(n^2m) where `n` is
        // the number of lines and `m` is the number of contiguous columns.
        //
        // However, I claim that it is actually O(nm). That is, the width for
        // every contiguous column is computed exactly once.
        let mut ws: Vec<_> = (0..self.lines.len()).map(|_| vec![]).collect();
        for (lineno, cells) in self.lines.iter().enumerate() {
            if cells.is_empty() {
                continue;
            }

            for col in ws[lineno].len()..(cells.len() - 1) {
                let mut width = 0;
                let mut contig_count = 0;
                for line in &self.lines[lineno..] {
                    if col + 1 >= line.len() {
                        // ignores last column
                        break;
                    }
                    contig_count += 1;
                    width = cmp::max(width, line[col].width);
                }
                if width > 0 || !self.ignore_empty_column {
                    width = self.minwidth.max(width + self.padding)
                }
                assert!(contig_count >= 1);
                for j in lineno..(lineno + contig_count) {
                    ws[j].push(width);
                }
            }
        }
        ws
    }

    fn flush(&mut self) -> io::Result<()> {
        let widths = self.cell_widths();

        // This is a trick to avoid allocating padding for every cell.
        // Just allocate the most we'll ever need and borrow from it.
        let biggest_width = widths
            .iter()
            .map(|ws| ws.iter().map(|&w| w).max().unwrap_or(0))
            .max()
            .unwrap_or(0);
        let padding: String = iter::repeat(self.padchar).take(biggest_width).collect();
        let use_tabs = self.tab_indent || self.padchar == '\t';
        for (i, (line, widths)) in self.lines.iter().zip(widths.iter()).enumerate() {
            if i != 0 {
                self.w.write(b"\n")?;
            }

            let indent_count = Self::write_indents(&mut self.w, line, use_tabs)?;
            for (i, cell) in line[indent_count..].iter().enumerate() {
                let i = i + indent_count;
                let bytes = cell.data;
                if i >= widths.len() {
                    // There is no width for the last column
                    assert_eq!(i, line.len() - 1);
                    Self::write_cell(&mut self.w, bytes, cell.contains_escape)?;
                } else {
                    assert!(widths[i] >= cell.width);
                    let extra_space = self.extra_space_count(widths[i], cell.width);
                    let (left_spaces, right_spaces) = match self.alignment {
                        Alignment::Left => (0, extra_space),
                        Alignment::Right => (extra_space, 0),
                        Alignment::Center => (extra_space / 2, extra_space - extra_space / 2),
                    };
                    self.w.write(padding[0..left_spaces].as_bytes())?;
                    Self::write_cell(&mut self.w, bytes, cell.contains_escape)?;
                    self.w.write(padding[0..right_spaces].as_bytes())?;
                }
            }
        }

        self.reset();
        Ok(())
    }

    fn write_indents(w: &mut W, line: &[Cell], use_tabs: bool) -> io::Result<usize> {
        let indent_count = if use_tabs {
            line.iter().position(|e| !e.data.is_empty()).unwrap_or(0)
        } else {
            0
        };

        Ok(if indent_count == line.len() {
            0
        } else {
            w.write(&b"\t\t\t\t\t\t\t\t\t\t\t"[..indent_count])?;
            indent_count
        })
    }

    fn extra_space_count(&self, outputw: usize, cellw: usize) -> usize {
        let n = outputw - cellw;
        if !(self.padchar == '\t') {
            n
        } else {
            (n + self.tabwidth - 1) / self.tabwidth
        }
    }

    fn write_cell(w: &mut W, data: &[u8], contains_escape: bool) -> io::Result<()> {
        if !contains_escape {
            return w.write_all(data);
        }

        for d in data.split(|b| *b == ESCAPE) {
            w.write_all(d)?;
        }

        Ok(())
    }
}

/// An error returned by `into_inner`.
///
/// This combines the error that happened while flushing the buffer with the
/// `TabWriter` itself.
pub struct IntoInnerError<W>(W, io::Error);

impl<W> IntoInnerError<W> {
    /// Returns the error which caused the `into_error()` call to fail.
    pub fn error(&self) -> &io::Error {
        &self.1
    }

    /// Returns the `TabWriter` instance which generated the error.
    pub fn into_inner(self) -> W {
        self.0
    }
}

impl<W> fmt::Debug for IntoInnerError<W> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.error().fmt(f)
    }
}

impl<W> fmt::Display for IntoInnerError<W> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.error().fmt(f)
    }
}

impl<W: ::std::any::Any> error::Error for IntoInnerError<W> {
    #[allow(deprecated)]
    fn description(&self) -> &str {
        self.error().description()
    }

    fn cause(&self) -> Option<&dyn error::Error> {
        Some(self.error())
    }
}

const ESCAPE: u8 = 255;
const ESCAPE_CHAR: char = '\u{FF}';

const CONT_MASK: u8 = 0b0011_1111;

/// Returns the initial codepoint accumulator for the first byte.
/// The first byte is special, only want bottom 5 bits for width 2, 4 bits
/// for width 3, and 3 bits for width 4.
#[inline]
const fn utf8_first_byte(byte: u8, width: u32) -> u32 {
    (byte & (0x7F >> width)) as u32
}

/// Returns the value of `ch` updated with continuation byte `byte`.
#[inline]
const fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | (byte & CONT_MASK) as u32
}

/// Checks whether the byte is a UTF-8 continuation byte (i.e., starts with the
/// bits `10`).
#[inline]
pub(super) const fn utf8_is_cont_byte(byte: u8) -> bool {
    (byte as i8) < -64
}

/// Reads the next code point out of a byte iterator (assuming a
/// UTF-8-like encoding).
///
/// # Safety
///
/// `bytes` must produce a valid UTF-8-like (UTF-8 or WTF-8) string
#[inline]
pub unsafe fn next_code_point_ignore_invalid<'a, I: Iterator<Item = &'a u8>>(
    bytes: &mut I,
) -> Option<u32> {
    // Decode UTF-8
    let x = *bytes.next()?;

    if x < 128 || x == ESCAPE {
        return Some(x as u32);
    }

    // Multibyte case follows
    // Decode from a byte combination out of: [[[x y] z] w]
    // NOTE: Performance is sensitive to the exact formulation here
    let init = utf8_first_byte(x, 2);
    // SAFETY: `bytes` produces an UTF-8-like string,
    // so the iterator must produce a value here.
    let y = unsafe { *bytes.next().unwrap_unchecked() };
    let mut ch = utf8_acc_cont_byte(init, y);
    if x >= 0xE0 {
        // [[x y z] w] case
        // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
        // SAFETY: `bytes` produces an UTF-8-like string,
        // so the iterator must produce a value here.
        let z = unsafe { *bytes.next().unwrap_unchecked() };
        let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
        ch = init << 12 | y_z;
        if x >= 0xF0 {
            // [x y z w] case
            // use only the lower 3 bits of `init`
            // SAFETY: `bytes` produces an UTF-8-like string,
            // so the iterator must produce a value here.
            let w = unsafe { *bytes.next().unwrap_unchecked() };
            ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
        }
    }

    Some(ch)
}

use std::slice;

use unicode_width::UnicodeWidthChar;
/// An iterator over the [`char`]s of a string slice.
///
///
/// This struct is created by the [`chars`] method on [`str`].
/// See its documentation for more.
///
/// [`char`]: prim@char
/// [`chars`]: str::chars
#[derive(Clone)]
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct Chars<'a> {
    pub(super) iter: slice::Iter<'a, u8>,
}

impl<'a> Iterator for Chars<'a> {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<char> {
        // SAFETY: `str` invariant says `self.iter` is a valid UTF-8 string and
        // the resulting `ch` is a valid Unicode Scalar Value.
        unsafe {
            next_code_point_ignore_invalid(&mut self.iter).map(|ch| char::from_u32_unchecked(ch))
        }
    }
}

impl fmt::Debug for Chars<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Chars(")?;
        f.debug_list().entries(self.clone()).finish()?;
        write!(f, ")")?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct CharIndices<'a> {
    pub(super) front_offset: usize,
    pub(super) iter: Chars<'a>,
}

impl<'a> CharIndices<'a> {
    fn new(data: &[u8]) -> CharIndices<'_> {
        CharIndices {
            front_offset: 0,
            iter: Chars { iter: data.iter() },
        }
    }
}

impl<'a> Iterator for CharIndices<'a> {
    type Item = (usize, char);

    #[inline]
    fn next(&mut self) -> Option<(usize, char)> {
        let pre_len = self.iter.iter.len();
        match self.iter.next() {
            None => None,
            Some(ch) => {
                let index = self.front_offset;
                let len = self.iter.iter.len();
                self.front_offset += pre_len - len;
                Some((index, ch))
            }
        }
    }

    #[inline]
    fn count(self) -> usize {
        self.iter.count()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

const FILTER_HTML: u32 = 1 << 0;
const STRIP_ESCAPE: u32 = 1 << 1;
const ALIGN_RIGHT: u32 = 1 << 2;
const DISCARD_EMPTY_COLUMNS: u32 = 1 << 3;

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn char_indices() {
        let data = [b'\xff', b'a'];
        let mut ci = CharIndices {
            front_offset: 0,
            iter: Chars { iter: data.iter() },
        };

        assert_eq!(
            Some((0, unsafe { char::from_u32_unchecked(ESCAPE as u32) })),
            ci.next()
        );

        assert_eq!(Some((1, 'a')), ci.next());
    }

    #[test]
    fn tabwriter_smoke() {
        #[derive(Debug, Default)]
        struct Case {
            desc: &'static str,
            minwidth: usize,
            tabwidth: usize,
            padding: usize,
            padchar: char,
            flags: u32,
            src: &'static [u8],
            expected: &'static [u8],
        }
        let tests = vec![
            Case {
                desc: "1a",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: 0,
                src: &[],
                expected: b"",
            },
            Case {
                desc: "1b esc stripped",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: STRIP_ESCAPE,
                src: &[b'\xff', b'\xff'],
                expected: b"",
            },
            Case {
                desc: "1c esc stripped",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: STRIP_ESCAPE,
                src: b"\xff\t\xff",
                expected: b"\t",
            },
            // Case {
            //     desc: "1c esc",
            //     minwidth: 8,
            //     tabwidth: 0,
            //     padding: 1,
            //     padbyte: b'.',
            //     flags: 0,
            //     src: b"\xff\t\xff",
            //     expected: b"\xff\t\xff",
            // },
            Case {
                desc: "1d esc stripped",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: STRIP_ESCAPE,
                src: b"\xff\"foo\t\n\tbar\"\xff",
                expected: b"\"foo\t\n\tbar\"",
            },
            //            	{
            // 	"1d esc",
            // 	8, 0, 1, '.', 0,
            // 	"\xff\"foo\t\n\tbar\"\xff",
            // 	"\xff\"foo\t\n\tbar\"\xff",
            // },
            Case {
                desc: "1e esc stripped",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: STRIP_ESCAPE,
                src: b"abc\xff\tdef", // unterminated escape
                expected: b"abc\tdef",
            },
            //            	{
            // 	"1e esc",
            // 	8, 0, 1, '.', 0,
            // 	"abc\xff\tdef", // unterminated escape
            // 	"abc\xff\tdef",
            // },
            Case {
                desc: "2",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: 0,
                src: b"\n\n\n",
                expected: b"\n\n\n",
            },
            Case {
                desc: "3",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: 0,
                src: b"a\nb\nc",
                expected: b"a\nb\nc",
            },
            Case {
                desc: "4a",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: 0,
                src: b"\t",
                expected: b"",
            },
            Case {
                desc: "4b",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: ALIGN_RIGHT,
                src: b"\t",
                expected: b"",
            },
            Case {
                desc: "5",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: 0,
                src: b"*\t*",
                expected: b"*.......*",
            },
            Case {
                desc: "5b",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: 0,
                src: b"*\t*\n",
                expected: b"*.......*\n",
            },
            Case {
                desc: "5c",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: 0,
                src: b"*\t*\t",
                expected: b"*.......*",
            },
            //            {
            // 	"5c debug",
            // 	8, 0, 1, '.', Debug,
            // 	"*\t*\t",
            // 	"*.......|*",
            // }
            Case {
                desc: "5d",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: ALIGN_RIGHT,
                src: b"*\t*\t",
                expected: b".......**",
            },
            Case {
                desc: "6",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: 0,
                src: b"\t\n",
                expected: b"........\n",
            },
            Case {
                desc: "7a",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: 0,
                src: b"a) foo",
                expected: b"a) foo",
            },
            Case {
                desc: "7b",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: ' ',
                flags: 0,
                src: b"b) foo\tbar",
                expected: b"b) foo  bar",
            },
            Case {
                desc: "7c",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: 0,
                src: b"c) foo\tbar\t",
                expected: b"c) foo..bar",
            },
            Case {
                desc: "7d",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: 0,
                src: b"d) foo\tbar\n",
                expected: b"d) foo..bar\n",
            },
            Case {
                desc: "7e",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: 0,
                src: b"e) foo\tbar\t\n",
                expected: b"e) foo..bar.....\n",
            },
            // TODO
            // {
            // 	"7f",
            // 	8, 0, 1, '.', FilterHTML,
            // 	"f) f&lt;o\t<b>bar</b>\t\n",
            // 	"f) f&lt;o..<b>bar</b>.....\n",
            // },
            //
            //
            // {
            // 	"7g",
            // 	8, 0, 1, '.', FilterHTML,
            // 	"g) f&lt;o\t<b>bar</b>\t non-terminated entity &amp",
            // 	"g) f&lt;o..<b>bar</b>..... non-terminated entity &amp",
            // },
            //
            // {
            // 	"7g debug",
            // 	8, 0, 1, '.', FilterHTML | Debug,
            // 	"g) f&lt;o\t<b>bar</b>\t non-terminated entity &amp",
            // 	"g) f&lt;o..|<b>bar</b>.....| non-terminated entity &amp",
            // },
            //
            Case {
                desc: "8",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '*',
                flags: 0,
                src: b"Hello, world!\n",
                expected: b"Hello, world!\n",
            },
            Case {
                desc: "9a",
                minwidth: 1,
                tabwidth: 0,
                padding: 0,
                padchar: '.',
                flags: 0,
                src: b"1\t2\t3\t4\n11\t222\t3333\t44444\n",
                expected: b"1.2..3...4\n11222333344444\n",
            },
            //
            // {
            // 	"9b",
            // 	1, 0, 0, '.', FilterHTML,
            // 	"1\t2<!---\f--->\t3\t4\n" + // \f inside HTML is ignored
            // 		"11\t222\t3333\t44444\n",
            //
            // 	"1.2<!---\f--->..3...4\n" +
            // 		"11222333344444\n",
            // },
            //

            Case {
                desc: "9c",
                minwidth: 1,
                tabwidth: 0,
                padding: 0,
                padchar: '.',
                flags: 0,
                src: b"1\t2\t3\t4\x0C11\t222\t3333\t44444\n",
                expected: b"1234\n11222333344444\n",
            },

            //
            // {
            // 	"9c debug",
            // 	1, 0, 0, '.', Debug,
            // 	"1\t2\t3\t4\f" + // \f causes a newline and flush
            // 		"11\t222\t3333\t44444\n",
            //
            // 	"1|2|3|4\n" +
            // 		"---\n" +
            // 		"11|222|3333|44444\n",
            // },
            //
            Case {
                desc: "10a",
                minwidth: 5,
                tabwidth: 0,
                padding: 0,
                padchar: '.',
                flags: 0,
                src: b"1\t2\t3\t4\n",
                expected: b"1....2....3....4\n",
            },
            Case {
                desc: "10b",
                minwidth: 5,
                tabwidth: 0,
                padding: 0,
                padchar: '.',
                flags: 0,
                src: b"1\t2\t3\t4\t\n",
                expected: b"1....2....3....4....\n",
            },
            Case {
                desc: "11",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '.',
                flags: 0,
                src: "本\tb\tc\naa\t\u{672c}\u{672c}\u{672c}\tcccc\tddddd\naaa\tbbbb\n".as_bytes(),
                expected: "本......b.......c\naa......本本本..cccc....ddddd\naaa.....bbbb\n"
                    .as_bytes(),
            },
            Case {
                desc: "12a",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: ' ',
                flags: ALIGN_RIGHT,
                src: "a\tè\tc\t\naa\tèèè\tcccc\tddddd\t\naaa\tèèèè\t\n".as_bytes(),
                expected:
                    "       a       è       c\n      aa     èèè    cccc   ddddd\n     aaa    èèèè\n"
                    .as_bytes(),
            },
            Case {
                desc: "12b",
                minwidth: 2,
                tabwidth: 0,
                padding: 0,
                padchar: ' ',
                flags: 0,
                src: b"a\tb\tc\naa\tbbb\tcccc\naaa\tbbbb\n",
                expected: b"a  b  c\naa bbbcccc\naaabbbb\n",
            },
            Case {
                desc: "12c",
                minwidth: 8,
                tabwidth: 0,
                padding: 1,
                padchar: '_',
                flags: 0,
                src: b"a\tb\tc\naa\tbbb\tcccc\naaa\tbbbb\n",
                expected: b"a_______b_______c\naa______bbb_____cccc\naaa_____bbbb\n",
            },
            Case {
                desc: "13a",
                minwidth: 4,
                tabwidth: 0,
                padding: 1,
                padchar: '-',
                flags: 0,
                src: "4444\t日本語\t22\t1\t333\n999999999\t22\n7\t22\n\t\t\t88888888\n\n666666\t666666\t666666\t4444\n1\t1\t999999999\t0000000000\n".as_bytes(),
                expected: "4444------日本語-22--1---333\n999999999-22\n7---------22\n------------------88888888\n\n666666-666666-666666----4444\n1------1------999999999-0000000000\n".as_bytes(),
            },
            Case {
                desc: "13b",
                minwidth: 4,
                tabwidth: 0,
                padding: 3,
                padchar: '.',
                flags: 0,
                src: "4444\t333\t22\t1\t333\n999999999\t22\n7\t22\n\t\t\t88888888\n\n666666\t666666\t666666\t4444\n1\t1\t999999999\t0000000000\n".as_bytes(),
                expected: "4444........333...22...1...333\n999999999...22\n7...........22\n....................88888888\n\n666666...666666...666666......4444\n1........1........999999999...0000000000\n".as_bytes(),
            },
            //        {
            // 	"13c",
            // 	8, 8, 1, '\t', FilterHTML,
            // 	"4444\t333\t22\t1\t333\n" +
            // 		"999999999\t22\n" +
            // 		"7\t22\n" +
            // 		"\t\t\t88888888\n" +
            // 		"\n" +
            // 		"666666\t666666\t666666\t4444\n" +
            // 		"1\t1\t<font color=red attr=日本語>999999999</font>\t0000000000\n",
            //
            // 	"4444\t\t333\t22\t1\t333\n" +
            // 		"999999999\t22\n" +
            // 		"7\t\t22\n" +
            // 		"\t\t\t\t88888888\n" +
            // 		"\n" +
            // 		"666666\t666666\t666666\t\t4444\n" +
            // 		"1\t1\t<font color=red attr=日本語>999999999</font>\t0000000000\n",
            // }
            Case {
                desc: "14",
                minwidth: 1,
                tabwidth: 0,
                padding: 2,
                padchar: ' ',
                flags: ALIGN_RIGHT,
                src: ".0\t.3\t2.4\t-5.1\t\n23.0\t12345678.9\t2.4\t-989.4\t\n5.1\t12.0\t2.4\t-7.0\t\n.0\t0.0\t332.0\t8908.0\t\n.0\t-.3\t456.4\t22.1\t\n.0\t1.2\t44.4\t-13.3\t\t".as_bytes(),
                expected: "    .0          .3    2.4    -5.1\n  23.0  12345678.9    2.4  -989.4\n   5.1        12.0    2.4    -7.0\n    .0         0.0  332.0  8908.0\n    .0         -.3  456.4    22.1\n    .0         1.2   44.4   -13.3".as_bytes(),
            },
            //        {
            // 	"14 debug",
            // 	1, 0, 2, ' ', AlignRight | Debug,
            // 	".0\t.3\t2.4\t-5.1\t\n" +
            // 		"23.0\t12345678.9\t2.4\t-989.4\t\n" +
            // 		"5.1\t12.0\t2.4\t-7.0\t\n" +
            // 		".0\t0.0\t332.0\t8908.0\t\n" +
            // 		".0\t-.3\t456.4\t22.1\t\n" +
            // 		".0\t1.2\t44.4\t-13.3\t\t",
            //
            // 	"    .0|          .3|    2.4|    -5.1|\n" +
            // 		"  23.0|  12345678.9|    2.4|  -989.4|\n" +
            // 		"   5.1|        12.0|    2.4|    -7.0|\n" +
            // 		"    .0|         0.0|  332.0|  8908.0|\n" +
            // 		"    .0|         -.3|  456.4|    22.1|\n" +
            // 		"    .0|         1.2|   44.4|   -13.3|",
            // }
            Case {
                desc: "15a",
                minwidth: 4,
                tabwidth: 0,
                padding: 0,
                padchar: '.',
                flags: 0,
                src: b"a\t\tb",
                expected: b"a.......b",
            },


            Case {
                desc: "15b",
                minwidth: 4,
                tabwidth: 0,
                padding: 0,
                padchar: '.',
                flags: DISCARD_EMPTY_COLUMNS, // htabs - do not discard column
                src: b"a\t\tb",
                expected: b"a.......b",
            },
            Case {
                desc: "16a",
                minwidth: 100,
                tabwidth: 100,
                padding: 0,
                padchar: '\t',
                flags: 0,
                src: concat!("a\tb\t\td\n",
                    "a\tb\t\td\te\n",
                    "a\n",
                    "a\tb\tc\td\n",
                    "a\tb\tc\td\te\n").as_bytes(),
                    expected: concat!("a\tb\t\td\n",
                        "a\tb\t\td\te\n",
                        "a\n",
                        "a\tb\tc\td\n",
                        "a\tb\tc\td\te\n").as_bytes(),
            },
            Case {
                desc: "16c",
                minwidth: 100,
                tabwidth: 100,
                padding: 0,
                padchar: '\t',
                flags: DISCARD_EMPTY_COLUMNS,
                src: concat!("a\tb\t\td\n", "a\tb\t\td\te\n", "a\n", "a\tb\tc\td\n", "a\tb\tc\td\te\n").as_bytes(),
                expected: concat!("a\tb\t\td\n", "a\tb\t\td\te\n", "a\n", "a\tb\tc\td\n", "a\tb\tc\td\te\n").as_bytes(),
            },
            Case {
                desc: "form_feed",
                minwidth: 100,
                tabwidth: 100,
                padding: 0,
                padchar: '\t',
                flags: DISCARD_EMPTY_COLUMNS,
                src: b"(a + k) +\x0C\tb/\x0C\t\t(c + d / *e)",
                expected: b"(a + k) +\n\tb/\n\t\t(c + d / *e)",
            }
        ];

        for c in tests {
            let data = vec![];
            let mut tw = TabWriter::new(data)
                .tabwidth(c.tabwidth)
                .flags(c.flags)
                .minwidth(c.minwidth)
                .padchar(c.padchar)
                .padding(c.padding);
            println!("Case: {}", c.desc);
            let _ = tw.write(c.src);
            assert_eq!(
                String::from_utf8_lossy(c.expected),
                String::from_utf8_lossy(tw.w.as_slice())
            );
        }
    }
}
