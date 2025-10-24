use std::collections::LinkedList;

pub(crate) trait Source<'a> {
    fn nextch(&mut self) -> char;
    fn pos(&self) -> (usize, usize);
    fn pos_byte(&self) -> (usize, usize);
    fn start(&mut self);
    fn rewind(&mut self);
    fn segment(&self) -> &'a str;
    fn size(&self) -> usize;
}

#[derive(Debug)]
pub(crate) struct Lines<'a> {
    lines: &'a [&'a str],
    lineno: usize, // 1-based
    col: usize,
    col_byte: usize,
    recorded_col_byte: usize,
    recorded_col: usize,
    recorded_lineno: usize,
    rewind_offset: usize,
    line: &'a str,
    chars: std::str::CharIndices<'a>,

    multi_lines_comment: std::cell::RefCell<LinkedList<String>>,
}

// SAFETY: the lifetime of the `Lines` must be the same as the string returned by the method `segment()`.
impl<'a> Lines<'a> {
    pub(crate) fn new(lines: &'a [&'a str]) -> Self {
        let mut r = Self {
            lines,
            lineno: 1,
            col: 0,
            col_byte: 0,
            chars: "".char_indices(),
            line: "",
            recorded_col_byte: 0,
            recorded_col: 0,
            recorded_lineno: 1,
            rewind_offset: 0,
            multi_lines_comment: std::cell::RefCell::new(LinkedList::new()),
        };

        r.line = r.lines[r.lineno - 1];
        r.chars = r.line.char_indices();
        r
    }
}

impl<'a> Source<'a> for Lines<'a> {
    fn nextch(&mut self) -> char {
        if let Some((offset, c)) = self.chars.next() {
            self.col += 1;
            self.col_byte = offset + self.rewind_offset;
            return c;
        }

        if self.lineno >= self.lines.len() {
            self.col = 0;
            self.lineno += 1;
            return '\0';
        }

        self.col = 0;
        self.col_byte = 0;
        self.line = self.lines[self.lineno];
        self.chars = self.line.char_indices();
        self.lineno += 1;
        '\n'
    }

    #[inline]
    fn pos(&self) -> (usize, usize) {
        (self.lineno, self.col)
    }

    #[inline]
    fn pos_byte(&self) -> (usize, usize) {
        (self.lineno, self.col_byte)
    }

    fn start(&mut self) {
        self.recorded_col_byte = self.col_byte;
        self.recorded_col = self.col;
        self.recorded_lineno = self.lineno;
    }

    // rewind rewinds the scanner's read position and character s.ch
    // to the start of the currently active segment, which must not
    // contain any newlines (otherwise position information will be
    // incorrect). Currently, rewind is only needed for handling the
    // source sequence ".."; it must not be called outside an active
    // segment.
    fn rewind(&mut self) {
        self.col_byte = self.recorded_col_byte;
        self.col = self.recorded_col;
        self.rewind_offset = self.col_byte;
        self.lineno = self.recorded_lineno;
        self.chars = self.line[self.col_byte..].char_indices();
        self.nextch();
    }

    fn segment(&self) -> &'a str {
        if self.recorded_lineno == self.lineno {
            return &self.lines[self.recorded_lineno - 1][self.recorded_col_byte..self.col_byte];
        }

        let mut vs = Vec::with_capacity(self.lineno - self.recorded_lineno + 1);
        vs.push(&self.lines[self.recorded_lineno - 1][self.recorded_col_byte..]);
        (self.recorded_lineno..self.lineno - 1).for_each(|l| vs.push(&self.lines[l]));
        if self.col > 0 {
            vs.push(&self.lines[self.lineno - 1][..self.col_byte]);
        }
        self.multi_lines_comment
            .borrow_mut()
            .push_back(vs.join("\n"));
        return unsafe {
            std::mem::transmute(self.multi_lines_comment.borrow().back().unwrap().as_str())
        };
    }

    fn size(&self) -> usize {
        self.lines.iter().map(|l| l.len()).sum::<usize>() + self.lines.len()
    }
}

#[derive(Debug)]
pub(crate) struct Block<'a> {
    source: &'a str,
    lineno: usize, // 1-based
    col: usize,
    col_byte: usize,
    start_col_byte: usize,
    start_col: usize,
    rewind_offset: usize,
    chars: std::str::CharIndices<'a>,
}

impl<'a> Block<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Self {
            source,
            lineno: 1,
            col: 0,
            col_byte: 0,
            chars: source.char_indices(),
            start_col_byte: 0,
            start_col: 0,
            rewind_offset: 0,
        }
    }
}

impl<'a> Source<'a> for Block<'a> {
    fn nextch(&mut self) -> char {
        let Some((offset, c)) = self.chars.next() else {
            self.col_byte = self.chars.offset() + self.rewind_offset;
            return '\0';
        };

        self.col_byte = offset + self.rewind_offset;
        if c == '\n' {
            self.col = 0;
            self.lineno += 1;
        } else {
            self.col += 1;
        }
        c
    }

    #[inline]
    fn pos(&self) -> (usize, usize) {
        (self.lineno, self.col)
    }

    #[inline]
    fn pos_byte(&self) -> (usize, usize) {
        (self.lineno, self.col_byte)
    }

    fn start(&mut self) {
        self.start_col_byte = self.col_byte;
        self.start_col = self.col;
    }

    // rewind rewinds the scanner's read position and character s.ch
    // to the start of the currently active segment, which must not
    // contain any newlines (otherwise position information will be
    // incorrect). Currently, rewind is only needed for handling the
    // source sequence ".."; it must not be called outside an active
    // segment.
    fn rewind(&mut self) {
        self.col_byte = self.start_col_byte;
        self.col = self.start_col;
        self.rewind_offset = self.col_byte;
        self.chars = self.source[self.col_byte..].char_indices();
        self.nextch();
    }

    fn segment(&self) -> &'a str {
        let r = &self.source[self.start_col_byte..self.col_byte];

        r
    }

    fn size(&self) -> usize {
        self.source.len()
    }
}

#[cfg(test)]
mod tests {
    use super::Source;

    #[test]
    fn nextch_of_lines() {
        let mut lines = super::Block::new("1\n23");
        assert_eq!(lines.nextch(), '1');
        assert_eq!(lines.pos(), (1, 1));
        assert_eq!(lines.nextch(), '\n');
        assert_eq!(lines.nextch(), '2');
        assert_eq!(lines.pos(), (2, 1));
        assert_eq!(lines.nextch(), '3');
        assert_eq!(lines.pos(), (2, 2));
        assert_eq!(lines.nextch(), '\0');
    }
}
