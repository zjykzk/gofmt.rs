use std::io::Write;
use std::mem::MaybeUninit;
use std::mem::discriminant;
use std::{cmp::Ordering, collections::HashMap, fmt::Debug, io, ptr};

use crate::{
    ast::*,
    parser::Parser,
    source::Block,
    source::Lines,
    tabwriter::TabWriter,
    tokens::{LiteralKind, Operator, Precedence, Token},
};

const FORM_FEED: u8 = b'\x0C';
const VTAB: u8 = b'\x0B';
const BLANK: u8 = b' ';
const ESCAPE: u8 = 255;
const MAX_NEWLINES: u32 = 2;

const INDENTS: &[u8] = b"\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t";
const NEWLINES: &[u8] = b"\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";

#[inline(always)]
fn write_indents<W: io::Write>(w: &mut W, n: u32) -> io::Result<usize> {
    w.write(&INDENTS[..n as usize])
}

#[inline(always)]
fn write_blank<W: io::Write>(w: &mut W) -> io::Result<usize> {
    w.write(&[BLANK])
}

#[derive(Debug)]
pub struct StmtPlace {
    is_last: bool,
}

#[derive(Debug)]
struct Context<'a, 'b>
where
    'a: 'b,
{
    size_of_node: HashMap<*const u8, usize>,
    comments: &'b [CommentGroup<'a>],

    indent_count: u32,
    comment_header: usize,
    last_end_pos: Pos,
    sep_before_line_comment: &'b [u8],
    use_ff_when_newline: bool,
    force_newline: bool,

    unindent_when_newline: u32,
    max_newlines: u32,

    stmt_place: Vec<StmtPlace>, // for ignoring the empty statement of `Label` before rbrace
    output_lineno: u32,
}

impl<'a, W: io::Write> Formatter<W> for Comment<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        w.write(&[ESCAPE])?;
        if self.tok == Token::LineComment {
            w.write(self.content.as_bytes())?;
        } else if self.tok == Token::FullComment {
            for (i, l) in self.content.lines().enumerate() {
                if i > 0 {
                    ctx.write_newlines(w, 1)?;
                }
                w.write(l.as_bytes())?;
            }
        }
        w.write(&[ESCAPE])?;
        Ok(())
    }
}

impl<'a> CommentGroup<'a> {
    fn size(&self) -> usize {
        let mut s = 0;
        for c in &self.comments {
            s += c.content.len();
        }
        s
    }
}

impl<'a, W: io::Write> Formatter<W> for CommentGroup<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        self.comments[0].format(w, ctx)?;
        for (i, c) in self.comments[1..].iter().enumerate() {
            if self.comments[i].pos.lineno != c.pos.lineno {
                ctx.write_newlines(w, 1)?;
                write_indents(w, ctx.indent_count)?;
            } else {
                write_blank(w)?;
            }

            c.format(w, ctx)?;
        }
        Ok(())
    }
}

impl<'a, 'b> Context<'a, 'b> {
    #[inline(always)]
    fn write_newlines<W: io::Write>(&mut self, w: &mut W, n: u32) -> io::Result<usize> {
        self.output_lineno += n;
        w.write(&NEWLINES[..n as usize])
    }

    #[inline(always)]
    fn write_formfeed<W: io::Write>(&mut self, w: &mut W) -> io::Result<usize> {
        self.output_lineno += 1;
        w.write(&[FORM_FEED])
    }

    fn write_formfeed_newlines<W: io::Write>(
        &mut self,
        w: &mut W,
        n: u32,
        use_ff: bool,
    ) -> io::Result<(usize, u32)> {
        let n = n.max(1).min(MAX_NEWLINES);
        let n = if use_ff {
            self.write_formfeed(w)?;
            n - 1
        } else {
            n
        };
        let s = self.write_newlines(w, n)?;
        Ok((s, n))
    }

    fn ignore_space(&mut self, pos: &Pos) {
        if !self.has_comments_before(pos) {
            // ignore the space
            self.last_end_pos = *pos;
        }
    }

    fn is_next_rbrace(&self) -> bool {
        for s in &self.stmt_place {
            if !s.is_last {
                return false;
            }
        }
        true
    }

    fn line_break<W: io::Write>(&mut self, w: &mut W, n: u32, use_ff: bool) -> io::Result<()> {
        let n = n.max(1).min(MAX_NEWLINES);
        let n = if use_ff {
            self.write_formfeed(w)?;
            n - 1
        } else {
            n
        };
        self.write_newlines(w, n)?;
        write_indents(w, self.indent_count)?;
        Ok(())
    }

    fn format_left_comment<W: io::Write>(&mut self, w: &mut W) -> io::Result<()> {
        if self.comment_header == self.comments.len() {
            return Ok(());
        }

        if self.last_end_pos.is_zero() {
            self.last_end_pos = Pos { col: 1, lineno: 1 }
        }
        self.format_comments_before_index(w, self.comments.len(), b" ")?;
        Ok(())
    }

    fn comment_size_before(&self, pos: &Pos) -> usize {
        let mut s = 0;
        for c in &self.comments[self.comment_header..] {
            if !c.start().before(pos) {
                break;
            }

            s += c.size();
        }
        s
    }

    fn get_comment_content(&self, index: CommentIndex) -> &str {
        self.comments[index as usize].comments[0].content
    }

    fn has_comments_before(&self, pos: &Pos) -> bool {
        self.comments[self.comment_header..]
            .iter()
            .filter(|c| c.start().before(pos))
            .next()
            .is_some()
    }

    fn first_comment_start_lineno_before(&self, pos: &Pos) -> Option<u32> {
        self.comments[self.comment_header..]
            .iter()
            .filter(|c| c.start().before(pos))
            .next()
            .map(|c| c.start().lineno)
    }

    fn has_tok_before_in_same_line(&self, pos: &Pos) -> bool {
        return self.comments[self.comment_header..]
            .iter()
            .filter(|c| c.start().before(pos))
            .last()
            .map(|c| c.end().lineno)
            .map(|l| l == pos.lineno) // same line comment
            .unwrap_or(self.last_end_pos.col < pos.col && self.last_end_pos.lineno == pos.lineno); // same line other token
    }

    fn first_index_of_available_comment_after(&self, pos: &Pos) -> Option<usize> {
        let mut i = self.comment_header;
        for c in &self.comments[self.comment_header..] {
            if pos.before(&c.start()) {
                break;
            }
            i += 1;
        }

        if i <= self.comment_header {
            None
        } else {
            Some(i)
        }
    }

    fn consume_surrent_line_comments_for_import_spec_chunk(
        &mut self,
        pos: &Pos,
        next_pos: &Pos,
    ) -> (&'b [CommentGroup<'a>], &'b [CommentGroup<'a>]) {
        let start = self.comment_header;
        let l = self.comments.len();
        while self.comment_header < l && self.comments[self.comment_header].start().before(pos) {
            self.comment_header += 1;
        }

        let left = &self.comments[start..self.comment_header];
        if pos.lineno == next_pos.lineno {
            return (left, &[]);
        }

        // TODO: the comments crossing multi-line
        // let next_pos = Pos {
        //     col: 0,
        //     lineno: pos.lineno + 1,
        // };
        let start = self.comment_header;
        while self.comment_header < l && self.comments[self.comment_header].end().before(next_pos) {
            self.comment_header += 1;
        }

        (left, &self.comments[start..self.comment_header])
    }

    fn format_comments_before_pos_align<W: io::Write>(
        &mut self,
        w: &mut W,
        pos: &Pos,
        sep_when_same_line: &[u8],
    ) -> io::Result<bool> {
        let Some(comment_tail) = self.first_index_of_available_comment_after(pos) else {
            return Ok(false);
        };

        let col = pos.col;
        let mut i = self.comment_header;
        for c in &self.comments[self.comment_header..comment_tail] {
            if c.start().col == col {
                break;
            }
            i += 1;
        }

        let has_indent_comments = self.comment_header < i;
        if has_indent_comments {
            self.indent_count += 1;
            self.format_comments_before_index(w, i, sep_when_same_line)?;
            self.indent_count -= 1;
        }

        if i < comment_tail {
            self.format_comments_before_index(w, comment_tail, sep_when_same_line)?;
        }

        self.write_formfeed_newlines(w, pos.lineno - self.last_end_pos.lineno, true)?;
        write_indents(w, self.indent_count)?; // for the code
        self.ignore_space(pos);

        Ok(true)
    }

    // return true when some comments formatted
    fn format_comments_before_pos<W: io::Write>(
        &mut self,
        w: &mut W,
        pos: &Pos,
        sep_when_same_line: &[u8],
    ) -> io::Result<bool> {
        let Some(comment_tail) = self.first_index_of_available_comment_after(pos) else {
            return Ok(false);
        };
        self.format_comments_before_index(w, comment_tail, sep_when_same_line)?;

        Ok(true)
    }

    fn format_comments_before_index<W: io::Write>(
        &mut self,
        w: &mut W,
        comment_tail: usize,
        sep_when_same_line: &[u8],
    ) -> io::Result<()> {
        let comments = &self.comments[self.comment_header as usize..comment_tail];
        let c_pos = comments[0].start();
        let newline = c_pos.lineno - self.last_end_pos.lineno;
        match newline {
            1.. => self.line_break(w, newline, false)?,
            0 if c_pos.col > self.last_end_pos.col => {
                w.write(sep_when_same_line)?;
            }
            _ => (),
        }

        comments[0].format(w, self)?;
        for (i, c) in comments[1..].iter().enumerate() {
            let newlines = c.start().lineno - comments[i].end().lineno;
            if newlines > 0 {
                self.write_newlines(w, newlines.min(MAX_NEWLINES))?;
                write_indents(w, self.indent_count)?;
            } else {
                write_blank(w)?;
            }

            c.format(w, self)?;
        }

        self.comment_header = comment_tail;
        self.last_end_pos = self.comments[comment_tail - 1].end();
        Ok(())
    }

    fn format_line_comments_before<W: io::Write>(
        &mut self,
        w: &mut W,
        pos: &Pos,
    ) -> io::Result<bool> {
        let Some((i, _)) = self.comments.iter().enumerate().find(|(_, c)| {
            let s = c.start();
            s.lineno == pos.lineno && s.before(pos)
        }) else {
            return Ok(false);
        };

        for c in &self.comments[i..] {
            let p = c.start();
            if pos.before(&p) {
                break;
            }
            c.format(w, self)?;
            write_blank(w)?;
        }
        Ok(true)
    }

    fn format_line_comments_between<W: io::Write>(
        &mut self,
        w: &mut W,
        start: &Pos,
        end: &Pos,
    ) -> io::Result<bool> {
        let Some((i, _)) = self.comments.iter().enumerate().find(|(_, c)| {
            let s = c.start();
            s.lineno == start.lineno
                && s.col > start.col
                && s.lineno == end.lineno
                && s.col < end.col
        }) else {
            return Ok(false);
        };

        for c in &self.comments[i..] {
            let p = c.start();
            if end.before(&p) {
                break;
            }
            c.format(w, self)?;
            write_blank(w)?;
        }
        Ok(true)
    }
}

#[derive(Debug, Default)]
pub struct SingleLineNodeSizeWriter {
    size: usize,
}

impl io::Write for SingleLineNodeSizeWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if self.size >= SingleLineNodeSizeWriter::MAX_SIZE {
            return Ok(self.size);
        }

        let mut escape_count = 0;
        for b in buf {
            match *b {
                FORM_FEED | b'\n' if escape_count & 1 == 0 => {
                    self.size = SingleLineNodeSizeWriter::MAX_SIZE;
                    break;
                }
                ESCAPE => escape_count += 1,
                _ => self.size += 1,
            }
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl SingleLineNodeSizeWriter {
    const MAX_SIZE: usize = usize::MAX >> 1;
}

impl<'a, 'b> Context<'a, 'b> {
    fn new() -> Self {
        Context {
            indent_count: 0,
            size_of_node: HashMap::new(),
            comments: &[],
            comment_header: 0,
            sep_before_line_comment: b" ",
            use_ff_when_newline: false,
            last_end_pos: Pos::zero(),
            stmt_place: Vec::with_capacity(32),
            unindent_when_newline: 0,
            max_newlines: MAX_NEWLINES,
            force_newline: false,
            output_lineno: 0,
        }
    }

    fn singleline_node_size<T: Formatter<SingleLineNodeSizeWriter> + Range>(
        &mut self,
        b: &T,
    ) -> usize {
        let ptr = ptr::from_ref(b) as *const u8;
        if let Some(v) = self.size_of_node.get(&ptr) {
            return *v;
        }

        let backup_comment_header = self.comment_header;
        let backup_last_end_pos = self.last_end_pos;
        let backup_indent_count = self.indent_count;
        let backup_lineno = self.output_lineno;
        let backup_force_newline = self.force_newline;
        let backup_max_newlines = self.max_newlines;
        let backup_unindent_when_new_line = self.unindent_when_newline;

        self.comment_header = self.comments.len(); // skip the comments
        self.indent_count = 0;
        self.last_end_pos = b.start();

        let mut sw = SingleLineNodeSizeWriter::default();
        let _ = b.format(&mut sw, self); // impossible error
        self.size_of_node.insert(ptr, sw.size);

        self.indent_count = backup_indent_count;
        self.last_end_pos = backup_last_end_pos;
        self.comment_header = backup_comment_header;
        self.output_lineno = backup_lineno;
        self.force_newline = backup_force_newline;
        self.max_newlines = backup_max_newlines;
        self.unindent_when_newline = backup_unindent_when_new_line;

        sw.size
    }
}

trait Formatter<W: io::Write> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()>;
}

#[derive(Debug)]
struct SepWhenSameLineFormatter<'a, W, F>
where
    W: io::Write,
    F: Formatter<W> + Range,
{
    f: &'a F,
    sep_when_same_line: &'a [u8],
    _marker: std::marker::PhantomData<W>,
}

impl<'a, W: io::Write, F: Formatter<W> + Range> SepWhenSameLineFormatter<'a, W, F> {
    fn format(f: &F, sep_when_same_line: &[u8], w: &mut W, ctx: &mut Context) -> io::Result<()> {
        SepWhenSameLineFormatter {
            f,
            sep_when_same_line,
            _marker: std::marker::PhantomData,
        }
        .format(w, ctx)
    }
}

impl<'a, W: io::Write, F: Formatter<W> + Range> Formatter<W>
    for SepWhenSameLineFormatter<'a, W, F>
{
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        let r = FormatterWithComment {
            pos: self.f.start(),
            end: self.f.start(),
            v: b"",
        }
        .format(w, ctx)?;
        if !r.line_break {
            w.write(self.sep_when_same_line)?;
        }
        self.f.format(w, ctx)
    }
}

#[derive(Debug)]
struct FormatterWithComment<'a> {
    v: &'a [u8],
    pos: Pos,
    end: Pos,
}

#[derive(Debug)]
pub struct FormatterWithCommentRet {
    line_break: bool,
}

impl<'a> FormatterWithComment<'a> {
    #[inline(always)]
    fn format<W: io::Write>(
        &self,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<FormatterWithCommentRet> {
        ctx.format_comments_before_pos(w, &self.pos, ctx.sep_before_line_comment)?;

        let mut line_break = false;
        let newline_count = self.pos.lineno - ctx.last_end_pos.lineno;
        if newline_count > 0 || ctx.force_newline {
            ctx.write_formfeed_newlines(
                w,
                ctx.max_newlines.min(newline_count as u32),
                ctx.use_ff_when_newline || newline_count > 1,
            )?;
            write_indents(w, ctx.indent_count - ctx.unindent_when_newline)?; // since comment eats the indent
            line_break = true;
        }
        ctx.sep_before_line_comment = b" ";
        ctx.use_ff_when_newline = false;
        ctx.force_newline = false;
        ctx.max_newlines = MAX_NEWLINES;
        w.write(self.v)?;
        ctx.last_end_pos = self.end;
        Ok(FormatterWithCommentRet { line_break })
    }
}

impl<W: io::Write> Formatter<W> for TokenPos {
    #[inline(always)]
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        FormatterWithComment {
            v: self.tok.to_string().as_bytes(),
            pos: self.pos,
            end: self.end(),
        }
        .format(w, ctx)?;
        Ok(())
    }
}

impl TokenPos {
    #[inline(always)]
    fn format_and_record_pos<W: io::Write>(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        self.tok.format(w, ctx)?;
        ctx.last_end_pos = self.pos;
        Ok(())
    }
}

impl<'a, W: io::Write> Formatter<W> for BasicLit<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        FormatterWithComment {
            v: &[ESCAPE],
            pos: self.pos,
            end: self.end(),
        }
        .format(w, ctx)?;
        self.format_number_if(w)?;
        w.write(&[ESCAPE])?;
        Ok(())
    }
}

impl<'a> BasicLit<'a> {
    fn format_number_if<W: io::Write>(&self, w: &mut W) -> io::Result<()> {
        use crate::tokens::LiteralKind;

        if !matches!(
            self.kind,
            LiteralKind::Int | LiteralKind::Float | LiteralKind::Imag
        ) || self.value.len() < 2
        {
            w.write(self.value.as_bytes())?;
            return Ok(());
        }

        let x = self.value;

        match &x[..2] {
            "0X" => {
                let x = format!("0x{}", &x[2..]);
                if let Some(i) = x.rfind('P') {
                    w.write(format!("{}p{}", &x[..i], &x[i + 1..]).as_bytes())?
                } else {
                    w.write(x.as_bytes())?
                }
            }
            "0x" => {
                if let Some(i) = x.rfind('P') {
                    w.write(format!("{}p{}", &x[..i], &x[i + 1..]).as_bytes())?
                } else {
                    w.write(x.as_bytes())?
                }
            }
            "0O" => w.write(format!("0o{}", &x[2..]).as_bytes())?,
            "0o" | "0b" => w.write(x.as_bytes())?,
            "0B" => w.write(format!("0b{}", &x[2..]).as_bytes())?,
            _ => {
                if let Some(i) = x.rfind('E') {
                    w.write(format!("{}e{}", &x[..i], &x[i + 1..]).as_bytes())?
                } else if x.ends_with('i') && !x.contains('.') && !x.contains('e') {
                    let trimmed = x.trim_start_matches(['0', '_']);
                    if trimmed == "i" {
                        w.write(b"0i")?
                    } else {
                        w.write(trimmed.as_bytes())?
                    }
                } else {
                    w.write(x.as_bytes())?
                }
            }
        };

        Ok(())
    }
}

impl<'a, W: io::Write> Formatter<W> for Identifier<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        FormatterWithComment {
            v: self.name.as_bytes(),
            pos: self.pos,
            end: self.end(),
        }
        .format(w, ctx)?;
        Ok(())
    }
}

impl<W: io::Write> Formatter<W> for OperatorPos {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        FormatterWithComment {
            v: self.op.to_string().as_bytes(),
            pos: self.pos,
            end: self.pos,
        }
        .format(w, ctx)?;
        Ok(())
    }
}

impl<'a, W: io::Write> Formatter<W> for Expression<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        ExprFormatter::new(self).format(w, ctx)
    }
}

#[derive(Debug)]
struct ExprFormatter<'a> {
    prec: Precedence,
    depth: usize,
    expr: &'a Expression<'a>,
}

impl<'a> Range for ExprFormatter<'a> {
    fn start(&self) -> Pos {
        self.expr.start()
    }

    fn end(&self) -> Pos {
        self.expr.end()
    }
}

impl<'a, W: io::Write> Formatter<W> for ExprFormatter<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        ExprFormatter { ..*self }.format0(w, ctx)
    }
}

impl<'a> ExprFormatter<'a> {
    fn new(expr: &'a Expression<'a>) -> Self {
        ExprFormatter {
            prec: Precedence::None,
            depth: 1,
            expr,
        }
    }

    fn format0<W: io::Write>(&mut self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        match self.expr {
            Expression::ReplaceMe => panic!("cannot format placeholder"),
            Expression::CompositeLit(c) => self.format_composite_lit(c, w, ctx),
            Expression::LiteralValue(v) => self.format_lit_value(v, w, ctx),
            Expression::TypeInstance(i) => i.format(w, ctx, self.depth),
            Expression::Struct(s) => s.format(w, ctx),
            Expression::SliceType(s) => s.format(w, ctx),
            Expression::MapType(e) => {
                Token::Map.append_pos(e.pos).format(w, ctx)?;
                Token::Lbrack.append_pos(e.lbrack).format(w, ctx)?;
                e.key_type.format(w, ctx)?;
                Token::Rbrack.append_pos(e.rbrack).format(w, ctx)?;
                e.ele_type.format(w, ctx)
            }
            Expression::ChanType(c) => {
                match c.dir {
                    ChanDir::Both => Token::Chan.append_pos(c.ch).format(w, ctx),
                    ChanDir::Send(_) => {
                        Token::Chan.format(w, ctx)?;
                        Token::Arrow.format(w, ctx)
                    }
                    ChanDir::Recv(_) => {
                        Token::Arrow.format(w, ctx)?;
                        Token::Chan.format(w, ctx)
                    }
                }?;
                write_blank(w)?;
                c.typ.format(w, ctx)
            }
            Expression::FuncType(f) => f.format(w, ctx),
            Expression::InterfaceType(i) => i.format(w, ctx),
            Expression::ArrayType(a) => {
                Token::Lbrack.append_pos(a.lbrack).format(w, ctx)?;
                a.len.format(w, ctx)?;
                Token::Rbrack.append_pos(a.rbrack).format(w, ctx)?;
                a.typ.format(w, ctx)
            }
            Expression::KeyedElement(k) => self.format_keyed_ele(k, w, ctx),
            Expression::BasicLit(e) => e.format(w, ctx),
            Expression::FuncLit(f) => f.format(w, ctx),
            Expression::UnderlyingType(u) => {
                Operator::Tilde.append_pos(u.pos).format(w, ctx)?;
                let pos = u.typ.start();
                if !ctx.has_comments_before(&pos) {
                    // ignore the space
                    ctx.last_end_pos = pos;
                }
                u.typ.format(w, ctx)
            }
            Expression::Index(i) => self.format_index(i, w, ctx),
            Expression::ListExpr(e) => {
                (ListExprFormatter {
                    depth: self.depth,
                    ..ListExprFormatter::new(&e)
                })
                .format(w, ctx)?;
                Ok(())
            }
            Expression::Operation(e) => self.format_oper(e, w, ctx),
            Expression::Token(t) => t.tok.format(w, ctx),
            Expression::Ident(i) => i.format(w, ctx),
            Expression::QualifiedName(q) => self.format_qualified_name(q, w, ctx),
            Expression::ParenedExpr(p) => self.format_parened_expr(p, w, ctx),
            Expression::Selector(s) => self.format_selector(s, w, ctx),
            Expression::CallExpr(c) => self.format_call(c, w, ctx),
            Expression::TypeAssert(t) => self.format_type_assert(t, w, ctx),
            Expression::TypeSwitchGuard(t) => self.format_type_switch_guard(t, w, ctx),
            Expression::SliceExpr(s) => self.format_slice(s, w, ctx),
            Expression::DotDotDotArgs(a) => {
                a.expr.format(w, ctx)?;
                if let Expression::BasicLit(l) = &a.expr
                    && l.kind == LiteralKind::Int
                {
                    write_blank(w)?;
                }
                Token::DotDotDot.format(w, ctx)
            }
            Expression::Expr(e) => e.format(w, ctx),
        }
    }
}

impl<'a> ExprFormatter<'a> {
    fn format_qualified_name<W: io::Write>(
        &self,
        q: &QualifiedName<'a>,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        q.pkg.format(w, ctx)?;
        Token::Dot.format(w, ctx)?;
        let indent = if q.dot.lineno != q.ident.pos.lineno {
            ctx.indent_count += 1;
            1
        } else {
            0
        };
        ctx.max_newlines = 1;
        FormatterWithComment {
            v: q.ident.name.as_bytes(),
            pos: q.ident.start(),
            end: q.ident.end(),
        }
        .format(w, ctx)?;
        ctx.indent_count -= indent;
        Ok(())
    }

    fn ignore_high_elem_if_len<'b>(
        expr: &'b Expression<'a>,
        indice: &'b [Expression<'a>],
    ) -> &'b [Expression<'a>] {
        if indice.len() == 5 {
            return indice;
        }
        if let Expression::Ident(ei) = expr {
            if let Expression::CallExpr(c) = &indice[indice.len() - 1] {
                if let CallExpr {
                    pexpr: Expression::Ident(i),
                    args,
                    ..
                } = &**c
                {
                    if let (Expression::Ident(ai), _) = &args[0]
                        && args.len() == 1
                    {
                        if i.name == "len" && ai.name == ei.name {
                            return &indice[..indice.len() - 1];
                        }
                    }
                }
            }
        }

        indice
    }

    fn format_slice<W: io::Write>(
        &self,
        s: &SliceExpr<'a>,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        Self::format_expr(&s.expr, w, ctx, Precedence::Highest, 1)?;
        Token::Lbrack.append_pos(s.lbrack).format(w, ctx)?;
        let indice = Self::ignore_high_elem_if_len(&s.expr, &s.indice);
        if self.depth > 1 {
            for i in &s.indice {
                Self::format_expr(i, w, ctx, Precedence::None, self.depth + 1)?;
            }
        } else {
            let mut flags = [0; 7];
            const COLON: u8 = 1;
            const BINARY: u8 = 2;
            const OTHER: u8 = 3;
            for (i, e) in indice.iter().enumerate() {
                flags[i + 1] = match e {
                    Expression::Operation(op) => {
                        if op.y.is_some() {
                            BINARY
                        } else {
                            OTHER
                        }
                    }
                    Expression::Token(_) => COLON,
                    _ => OTHER,
                };
            }

            let print_blank = flags.contains(&BINARY)
                && flags
                    .iter()
                    .filter(|f| **f == BINARY || **f == OTHER)
                    .count()
                    > 1;
            for (i, e) in indice.iter().enumerate() {
                match flags[i + 1] {
                    COLON => {
                        if print_blank && flags[i] > COLON {
                            write_blank(w)?;
                        }
                        Token::Colon.format(w, ctx)?;
                        if print_blank && flags[i + 2] > COLON {
                            write_blank(w)?;
                        }
                    }
                    _ => {
                        Self::format_expr(e, w, ctx, Precedence::None, self.depth + 1)?;
                    }
                }
            }
        };
        Token::Rbrack.append_pos(s.rbrack).format(w, ctx)
    }

    fn format_type_switch_guard<W: io::Write>(
        &self,
        t: &TypeSwitchGuard<'a>,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        if let Some((ident, _)) = &t.lhs {
            ident.format(w, ctx)?;
            write_blank(w)?;
            Token::Define.format(w, ctx)?;
            write_blank(w)?;
        }
        Self::format_expr(&t.x, w, ctx, Precedence::Highest, self.depth)?;
        Token::Dot.format(w, ctx)?;
        Token::Lparen.format(w, ctx)?;
        Token::Type.format(w, ctx)?;
        Token::Rparen.format(w, ctx)
    }

    fn format_type_assert<W: io::Write>(
        &self,
        t: &TypeAssert<'a>,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        Self::format_expr(&t.pexpr, w, ctx, Precedence::Highest, self.depth)?;
        Token::Dot.format(w, ctx)?;
        Token::Lparen.format(w, ctx)?;
        ctx.ignore_space(&t.typ.start());
        Self::format_expr(&t.typ, w, ctx, Precedence::None, self.depth + 1)?;
        Token::Rparen.format(w, ctx)
    }

    fn format_index<W: io::Write>(
        &self,
        index: &Index<'a>,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        Self::format_expr(&index.expr, w, ctx, Precedence::Highest, 1)?;
        Token::Lbrack.append_pos(index.lbrack).format(w, ctx)?;
        ctx.ignore_space(&index.indices.start());
        if let Expression::ListExpr(le) = &index.indices {
            ListExprFormatter {
                depth: self.depth + 1,
                prev_pos: Some(index.lbrack),
                next_pos: Some(index.rbrack),
                add_comma_when_newline_of_last_expr: true,
                ..ListExprFormatter::new(le)
            }
            .format(w, ctx)?;
        } else {
            Self::format_expr(&index.indices, w, ctx, Precedence::None, self.depth + 1)?;
        }
        Token::Rbrack.append_pos(index.rbrack).format(w, ctx)
    }

    fn format_call<W: io::Write>(
        &mut self,
        c: &CallExpr<'a>,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        let paren = match &c.pexpr {
            Expression::FuncType(_) => true,
            Expression::ChanType(c) => {
                if let ChanType {
                    dir: ChanDir::Recv(_),
                    ..
                } = &**c
                {
                    true
                } else {
                    false
                }
            }
            _ => false,
        };
        if paren {
            w.write(b"(")?;
        }
        c.pexpr.format(w, ctx)?;
        if paren {
            w.write(b")")?;
        }

        Token::Lparen.append_pos(c.lparen).format(w, ctx)?;
        let args_count = c.args.len();
        if args_count == 0 {
            return Token::Rparen.append_pos(c.rparen).format(w, ctx);
        }

        if args_count > 1 {
            self.depth += 1;
        }

        let indents_of_args = if let Expression::Selector(s) = &c.pexpr
            && s.dot.lineno != s.name.start().lineno
        {
            1
        } else {
            0
        };
        ctx.indent_count += indents_of_args;
        let dotdotdot_pos = if let (Expression::DotDotDotArgs(d), _) = &c.args[args_count - 1] {
            Some(d.dotdotdot)
        } else {
            None
        };
        let one_line = (ListExprFormatter {
            depth: self.depth,
            add_comma_when_newline_of_last_expr: true,
            prev_pos: Some(c.lparen),
            next_pos: dotdotdot_pos.or(Some(c.rparen)),
            ..ListExprFormatter::new(&c.args)
        })
        .format(w, ctx)?;
        if let Some(p) = dotdotdot_pos
            && p.lineno != c.rparen.lineno
        {
            w.write(b",")?;
            ctx.line_break(w, 0, true)?;
            Token::Rparen
                .append_pos(c.rparen)
                .format_and_record_pos(w, ctx)?;
        } else if one_line {
            Token::Rparen
                .append_pos(c.rparen)
                .format_and_record_pos(w, ctx)?;
        } else {
            ctx.max_newlines = 1;
            Token::Rparen.append_pos(c.rparen).format(w, ctx)?;
        }
        ctx.indent_count -= indents_of_args;
        Ok(())
    }

    fn format_selector<W: io::Write>(
        &self,
        s: &Selector<'a>,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        ExprFormatter::format_expr(&s.pexpr, w, ctx, Precedence::Highest, self.depth)?;
        if let Expression::BasicLit(l) = &s.pexpr
            && l.kind == LiteralKind::Int
        {
            write_blank(w)?;
        }
        Token::Dot.append_pos(s.dot).format(w, ctx)?;
        let indent = if s.dot.lineno != s.name.start().lineno {
            ctx.indent_count += 1;
            1
        } else {
            0
        };
        s.name.format(w, ctx)?;
        ctx.indent_count -= indent;
        Ok(())
    }

    fn format_oper<W: io::Write>(
        &mut self,
        op: &'a Operation<'a>,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        let Some(y) = &op.y else {
            op.op.format(w, ctx)?;
            self.expr = &op.x;
            self.format(w, ctx)?;
            return Ok(());
        };

        let Operation { ref op, ref x, .. } = *op;
        self.format_binary_expr(x, op, y, w, ctx, Self::cutoff(x, op.op, y, self.depth))
    }

    fn cutoff(x: &Expression<'a>, op: Operator, y: &Expression<'a>, depth: usize) -> Precedence {
        let (has4, has5, max_problem) = Self::walk_binary(x, op, y);
        if max_problem > Precedence::None {
            return max_problem;
        }

        if has4 && has5 {
            if depth == 1 {
                Precedence::Mul
            } else {
                Precedence::Add
            }
        } else {
            if depth == 1 {
                Precedence::Unary
            } else {
                Precedence::Add
            }
        }
    }

    fn walk_binary(
        x: &Expression<'a>,
        op: Operator,
        y: &Expression<'a>,
    ) -> (bool, bool, Precedence) {
        let prec = op.precedence();
        let (mut has4, mut has5, mut max_problem) = (
            prec == Precedence::Add,
            prec == Precedence::Mul,
            Precedence::None,
        );

        if let Expression::Operation(x) = x {
            if let Some(e) = &x.y {
                let (h4, h5, mp) = Self::walk_binary(&x.x, x.op.op, &e);
                has4 = has4 || h4;
                has5 = has5 || h5;
                if max_problem < mp {
                    max_problem = mp;
                }
            }
        }

        if let Expression::Operation(y) = y {
            if let Some(e) = &y.y {
                let (h4, h5, mp) = Self::walk_binary(&y.x, y.op.op, &e);
                has4 = has4 || h4;
                has5 = has5 || h5;
                if max_problem < mp {
                    max_problem = mp;
                }
            } else {
                max_problem = match (op, y.op.op) {
                    (Operator::Div, Operator::Mul)
                    | (Operator::And, Operator::And)
                    | (Operator::And, Operator::Xor) => Precedence::Unary,
                    (Operator::Add, Operator::Add) | (Operator::Sub, Operator::Sub) => {
                        Precedence::Mul
                    }
                    _ => max_problem,
                }
            }
        }

        (has4, has5, max_problem)
    }

    pub(crate) fn format_expr<W: io::Write>(
        expr: &Expression<'a>,
        w: &mut W,
        ctx: &mut Context,
        prec: Precedence,
        depth: usize,
    ) -> io::Result<()> {
        ExprFormatter {
            prec,
            depth,
            ..ExprFormatter::new(expr)
        }
        .format(w, ctx)
    }

    fn format_binary_expr<W: io::Write>(
        &mut self,
        x: &Expression<'a>,
        op: &OperatorPos,
        y: &Expression<'a>,
        w: &mut W,
        ctx: &mut Context,
        cutoff: Precedence,
    ) -> io::Result<()> {
        let prec = op.op.precedence();
        let print_blank = prec < cutoff;
        Self::format_expr(
            x,
            w,
            ctx,
            prec,
            self.depth
                + if let Expression::Operation(op) = x {
                    if op.y.is_some() && op.op.op.precedence() == prec {
                        0
                    } else {
                        1
                    }
                } else {
                    1
                },
        )?;
        if print_blank {
            w.write(&[b' '])?;
        }
        op.format(w, ctx)?;
        if y.start().lineno > op.pos.lineno {
            ctx.indent_count += 1;
            let f = &ExprFormatter {
                prec: prec.next(),
                depth: self.depth + 1,
                ..ExprFormatter::new(y)
            };
            f.format(w, ctx)?;

            ctx.indent_count -= 1;
        } else {
            if print_blank {
                w.write(&[b' '])?;
            }
            Self::format_expr(y, w, ctx, prec.next(), self.depth + 1)?;
        }
        Ok(())
    }

    fn format_parened_expr<W: io::Write>(
        &mut self,
        p: &'a ParenedExpr,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        if let Expression::ParenedExpr(expr) = &p.expr {
            return self.format_parened_expr(&expr, w, ctx);
        }

        Token::Lparen.append_pos(p.lparen).format(w, ctx)?;
        ctx.ignore_space(&p.expr.start());
        self.expr = &p.expr;
        self.depth = if self.depth > 1 { self.depth - 1 } else { 1 };
        self.prec = Precedence::None;
        self.format(w, ctx)?;
        Token::Rparen.append_pos(p.rparen).format(w, ctx)?;
        Ok(())
    }

    fn format_composite_lit<W: io::Write>(
        &mut self,
        c: &'a CompositeLit,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        Self::format_expr(&c.typ, w, ctx, Precedence::Highest, self.depth)?;
        self.format_lit_value(&c.value, w, ctx)?;
        Ok(())
    }

    fn format_lit_value<W: io::Write>(
        &mut self,
        c: &'a LiteralValue,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        Token::Lbrace.append_pos(c.lbrace).format(w, ctx)?;

        let is_empty = c.elem_list.is_empty();
        if !is_empty {
            (ListExprFormatter {
                depth: self.depth,
                add_comma_when_newline_of_last_expr: true,
                prev_pos: Some(c.lbrace),
                next_pos: Some(c.rbrace),
                ..ListExprFormatter::new(&c.elem_list)
            })
            .format(w, ctx)?;
        }

        if ctx.has_comments_before(&c.rbrace) {
            ctx.indent_count += 1;
            ctx.unindent_when_newline = 1;
            ctx.use_ff_when_newline = true;
            Token::Rbrace.append_pos(c.rbrace).format(w, ctx)?;
            ctx.indent_count -= 1;
            ctx.unindent_when_newline = 0;
            Ok(())
        } else {
            if is_empty {
                Token::Rbrace
                    .append_pos(c.rbrace)
                    .format_and_record_pos(w, ctx)
            } else {
                Token::Rbrace.append_pos(c.rbrace).format(w, ctx)
            }
        }
    }

    fn format_keyed_ele<W: io::Write>(
        &self,
        k: &'a KeyedElement,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        if let Some((k, _)) = &k.key_and_colon {
            k.format(w, ctx)?;
            Token::Colon.format(w, ctx)?;
            write_blank(w)?;
        }
        k.elem.format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for AST<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        match self {
            Self::PackageClause(p) => {
                Token::Package.append_pos(p.pos).format(w, ctx)?;
                write_blank(w)?;
                p.name.format(w, ctx)
            }
            Self::ImportDecl(i) => i.format(w, ctx),
            Self::TypeDecl(t) => t.format(w, ctx),
            Self::FuncDecl(d) => d.format(w, ctx),
            Self::MethodDecl(m) => m.format(w, ctx),
            Self::VarDecl(v) => VarDeclFormatter {
                tok: Token::Var,
                v: &v,
            }
            .format(w, ctx),
            Self::ConstDecl(v) => VarDeclFormatter {
                tok: Token::Const,
                v: &v,
            }
            .format(w, ctx),
            Self::Stmt(a) => a.format(w, ctx),
            Self::Semi(_) => Token::Semi.format(w, ctx),
        }
    }
}

impl<'a, W: io::Write> Formatter<W> for FuncLit<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        FuncFormatter {
            func: self.func,
            func_header: b"func",
            receiver: None,
            name: None,
            type_params: None,
            sign: &self.sign,
            body: Some(&self.body),
            sep: &[BLANK],
        }
        .format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for FuncDecl<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        ctx.force_newline = ctx.has_tok_before_in_same_line(&self.func);
        FuncFormatter {
            func: self.func,
            func_header: b"func ",
            receiver: None,
            name: Some(self.name),
            type_params: self.type_params.as_ref(),
            sign: &self.sign,
            body: self.body.as_ref(),
            sep: &[VTAB],
        }
        .format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for MethodDecl<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        ctx.force_newline = ctx.has_tok_before_in_same_line(&self.func);
        FuncFormatter {
            func: self.func,
            func_header: b"func ",
            receiver: Some(&self.receiver),
            name: Some(self.name),
            type_params: None,
            sign: &self.sign,
            body: self.body.as_ref(),
            sep: &[VTAB],
        }
        .format(w, ctx)
    }
}

#[derive(Debug)]
struct FuncFormatter<'a> {
    func: Pos,
    func_header: &'a [u8],
    receiver: Option<&'a Params<'a>>,
    name: Option<Identifier<'a>>,
    type_params: Option<&'a TypeParameters<'a>>,
    sign: &'a FuncSign<'a>,
    body: Option<&'a BlockStmt<'a>>,
    sep: &'a [u8],
}

impl<'a> Range for FuncFormatter<'a> {
    #[inline(always)]
    fn start(&self) -> Pos {
        self.func
    }

    fn end(&self) -> Pos {
        if let Some(b) = &self.body {
            b.rbrace
        } else {
            self.sign.end()
        }
    }
}

impl<'a, W: io::Write> Formatter<W> for FuncFormatter<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        (FormatterWithComment {
            v: self.func_header,
            pos: self.func,
            end: self.func,
        })
        .format(w, ctx)?;
        if let Some(r) = &self.receiver {
            r.format(w, ctx)?;
            write_blank(w)?;
        }

        if let Some(name) = &self.name {
            name.format(w, ctx)?;
        }
        if let Some(tp) = &self.type_params {
            TypeParametersFormatter {
                params: tp,
                tok: Token::Func,
            }
            .format(w, ctx)?;
        }
        self.sign.format(w, ctx)?;
        let Some(b) = &self.body else { return Ok(()) };
        if self.can_format_in_one_line(ctx) {
            w.write(self.sep)?;
            b.format_in_line(w, ctx)
        } else {
            write_blank(w)?;
            b.format(w, ctx)
        }
    }
}

impl<'a> FuncFormatter<'a> {
    fn can_format_in_one_line(&self, ctx: &mut Context) -> bool {
        if self.func.lineno
            != if let Some(b) = &self.body {
                b.end().lineno
            } else {
                self.sign.end().lineno
            }
        {
            return false;
        }

        const MAX_SIZE: usize = 100;
        let mut size = 6 /*"func ".len() + blank between sign and body*/ +
            if let Some(name) = self.name {name.name.len()}else {0};

        if let Some(r) = &self.receiver {
            size += ctx.singleline_node_size(*r);
            if size > MAX_SIZE {
                return false;
            }
        }

        if let Some(tp) = &self.type_params {
            size += ctx.singleline_node_size(&TypeParametersFormatter {
                params: tp,
                tok: Token::Func,
            });
            if size > MAX_SIZE {
                return false;
            }
        }

        size += ctx.singleline_node_size(self.sign);
        if size > MAX_SIZE {
            return false;
        }

        let Some(b) = &self.body else {
            return size <= MAX_SIZE;
        };

        if b.stmts.len() > 5 {
            return false;
        }

        let stmts = b.ignore_semi_stmt();
        if stmts.len() > 5 {
            return false;
        }

        size = ctx.comment_size_before(&b.rbrace);

        for (i, s) in stmts.iter().enumerate() {
            if size > MAX_SIZE {
                break;
            }
            if i > 0 {
                size += 2; // semi & blank
            }
            size += ctx.singleline_node_size(*s);
        }

        size <= MAX_SIZE
    }
}

trait IsEmptyStmt {
    fn is_empty(&self) -> bool {
        false
    }
}

impl<'a> IsEmptyStmt for Stmt<'a> {
    fn is_empty(&self) -> bool {
        match self {
            Stmt::Semi(_) | Stmt::Empty => true,
            _ => false,
        }
    }
}

impl<'a> IsEmptyStmt for &Stmt<'a> {
    fn is_empty(&self) -> bool {
        (*self).is_empty()
    }
}

impl<'a> IsEmptyStmt for CaseCause<'a> {}

trait IndentEater {
    fn eat(&self) -> u32 {
        0
    }
}

impl<'a> IndentEater for Stmt<'a> {
    fn eat(&self) -> u32 {
        match self {
            Stmt::Label(_) => 1,
            _ => 0,
        }
    }
}

impl<'a> IndentEater for &Stmt<'a> {
    fn eat(&self) -> u32 {
        (*self).eat()
    }
}

impl<'a> Range for &Stmt<'a> {
    fn start(&self) -> Pos {
        (*self).start()
    }

    fn end(&self) -> Pos {
        (*self).end()
    }
}

impl<'a, W: io::Write> Formatter<W> for &Stmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        (*self).format(w, ctx)
    }
}

#[derive(Debug)]
struct BlockStmtFormatter<'b, F> {
    lbrace: Pos,
    stmts: &'b [F],
    rbrace: Pos,
    indent_count: u32,

    sep_after_lbrace_when_empty: &'static [u8],
    max_newlines_before_rbrace: u32,
}

impl<'b, F, W: io::Write> Formatter<W> for BlockStmtFormatter<'b, F>
where
    F: Formatter<W> + IndentEater + Range + IsEmptyStmt + Debug,
{
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        ctx.stmt_place.push(StmtPlace { is_last: true });
        self.format_lbrace(w, ctx)?;

        ctx.indent_count += self.indent_count;
        Self::format_stmts(self.stmts, w, ctx)?;
        ctx.indent_count -= self.indent_count;

        self.format_rbrace(w, ctx)?;

        ctx.stmt_place.pop();

        Ok(())
    }
}

impl<'b, F> BlockStmtFormatter<'b, F>
where
    F: IndentEater + Range + IsEmptyStmt + Debug,
{
    // format the '{' and the comments before the first stmt or '}' when no stmts
    fn format_lbrace<W: io::Write>(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::Lbrace.append_pos(self.lbrace).format(w, ctx)?;
        Ok(())
    }

    // format the '}' and the comments between last stmt if any and '}'
    fn format_rbrace<W: io::Write>(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        ctx.use_ff_when_newline = true;
        ctx.force_newline = true;

        let comment_lineno = ctx.first_comment_start_lineno_before(&self.rbrace);
        if comment_lineno.is_some() {
            ctx.sep_before_line_comment = if !self.stmts.is_empty() {
                &[VTAB]
            } else {
                self.sep_after_lbrace_when_empty
            };
            ctx.indent_count += 1;
            ctx.unindent_when_newline = 1;
            Token::Rbrace.append_pos(self.rbrace).format(w, ctx)?;
            ctx.indent_count -= 1;
            ctx.unindent_when_newline = 0;
            Ok(())
        } else {
            ctx.max_newlines = self.max_newlines_before_rbrace;
            Token::Rbrace.append_pos(self.rbrace).format(w, ctx)
        }
    }

    fn format_after_eating_indent<W: io::Write>(
        ctx: &mut Context,
        s: &F,
        w: &mut W,
        is_last: bool,
    ) -> io::Result<()>
    where
        F: Formatter<W>,
    {
        let e = if ctx.indent_count == 0 { 0 } else { s.eat() };
        ctx.stmt_place.push(StmtPlace { is_last });

        ctx.indent_count -= e;
        s.format(w, ctx)?;
        ctx.indent_count += e;

        ctx.stmt_place.pop();

        Ok(())
    }

    fn format_stmt<W: io::Write>(
        stmt: &F,
        w: &mut W,
        ctx: &mut Context,
        is_last: bool,
        is_prev_stmt_multi_line: bool,
    ) -> io::Result<()>
    where
        F: Formatter<W>,
    {
        ctx.sep_before_line_comment = &[VTAB];
        if ctx.last_end_pos.lineno == stmt.start().lineno {
            ctx.write_formfeed_newlines(w, 1, stmt.start().lineno != stmt.end().lineno)?;
            write_indents(w, ctx.indent_count)?;
        } else {
            ctx.use_ff_when_newline =
                stmt.start().lineno != stmt.end().lineno || is_prev_stmt_multi_line;
        }
        Self::format_after_eating_indent(ctx, stmt, w, is_last)?;
        Ok(())
    }

    fn format_stmts<W: io::Write>(stmts: &[F], w: &mut W, ctx: &mut Context) -> io::Result<()>
    where
        F: Formatter<W>,
    {
        let [first, ..] = stmts else { return Ok(()) };
        let last_index_minus_one: i32 = stmts.len() as i32 - 2;
        Self::format_stmt(first, w, ctx, last_index_minus_one == -1, false)?;

        for (i, s) in stmts[1..].iter().enumerate() {
            if s.is_empty() {
                continue;
            }
            Self::format_stmt(
                s,
                w,
                ctx,
                i as i32 == last_index_minus_one,
                stmts[i].start().lineno != stmts[i].end().lineno,
            )?;
        }
        Ok(())
    }
}

impl<'a, W: io::Write> Formatter<W> for BlockStmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        BlockStmtFormatter {
            sep_after_lbrace_when_empty: b" ",
            lbrace: self.lbrace,
            stmts: &self.ignore_semi_stmt(),
            rbrace: self.rbrace,
            indent_count: 1,
            max_newlines_before_rbrace: MAX_NEWLINES,
        }
        .format(w, ctx)
    }
}

impl<'a> BlockStmt<'a> {
    fn format_in_line<W: io::Write>(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::Lbrace.format(w, ctx)?;
        let stmts = self.ignore_semi_stmt();
        let [first, ..] = stmts[..] else {
            if ctx.has_comments_before(&self.rbrace) {
                return SepWhenSameLineFormatter::format(
                    &Token::Rbrace.append_pos(self.rbrace),
                    b" ",
                    w,
                    ctx,
                );
            } else {
                return Token::Rbrace.append_pos(self.rbrace).format(w, ctx);
            }
        };

        write_blank(w)?;
        first.format(w, ctx)?;

        for s in &stmts[1..] {
            Token::Semi.format(w, ctx)?;
            write_blank(w)?;
            s.format(w, ctx)?;
        }

        write_blank(w)?;
        Token::Rbrace.format(w, ctx)
    }

    fn ignore_semi_stmt(&self) -> Vec<&Stmt<'a>> {
        self.stmts
            .iter()
            .filter(|s| match s {
                Stmt::Empty | Stmt::Semi(_) => false,
                _ => true,
            })
            .collect::<Vec<_>>()
    }
}

impl<'a, W: io::Write> Formatter<W> for FuncSign<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        self.params.format(w, ctx)?;
        if let Some(r) = &self.ret {
            r.format(w, ctx)?;
        }
        Ok(())
    }
}

impl<'a, W: io::Write> Formatter<W> for FuncType<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::Func.append_pos(self.func).format(w, ctx)?;
        self.params.format(w, ctx)?;
        if let Some(r) = &self.ret {
            r.format(w, ctx)?;
        }
        Ok(())
    }
}

impl<'a> FuncResult<'a> {
    fn format<W: io::Write>(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        match self {
            FuncResult::Params(p) => {
                let Some(ps) = &p.list else { return Ok(()) };
                match ps.len() {
                    0 => Ok(()),
                    1 => {
                        if ps[0].0.idents.is_some() {
                            w.write(b" (")?;
                            ps[0].0.format(w, ctx, true)?;
                            w.write(b")")?;
                        } else {
                            w.write(b" ")?;
                            ps[0].0.format(w, ctx, true)?;
                        }
                        Ok(())
                    }
                    _ => {
                        write_blank(w)?;
                        p.format(w, ctx)
                    }
                }
            }
            FuncResult::Type(t) => {
                write_blank(w)?;
                strip_parens_always(t).format(w, ctx)
            }
        }
    }
}

impl<W: io::Write> Formatter<W> for Operator {
    fn format(&self, w: &mut W, _: &mut Context) -> io::Result<()> {
        w.write(self.to_string().as_bytes())?;
        Ok(())
    }
}

impl<'a, W: io::Write> ParamFormatter<W> for ParamDecl<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context, do_indent_ident: bool) -> io::Result<()> {
        if let Some(id) = &self.idents {
            id.format_with_indent(w, ctx, do_indent_ident)?;
            write_blank(w)?;
        }

        if self.dotdotdot.is_some() {
            Token::DotDotDot.format(w, ctx)?;
        }

        strip_parens_always(&self.typ).format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for Params<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        ParamsFormatter {
            open_tok: Token::Lparen.append_pos(self.lparen),
            close_tok: Token::Rparen.append_pos(self.rparen),
            params: &(if let Some(ps) = &self.list {
                ps.iter().map(|(p, _)| p).collect::<Vec<_>>()
            } else {
                vec![]
            }),
        }
        .format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for TypeDecl<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::Type.append_pos(self.pos).format(w, ctx)?;
        write_blank(w)?;
        match &self.decl {
            TypeDeclOption::Spec(s) => s.format_with_align_name(w, ctx, &[BLANK]),
            TypeDeclOption::Group(g) => {
                Token::Lparen.format(w, ctx)?;
                ctx.indent_count += 1;
                for (s, _) in &g.specs {
                    ctx.force_newline = true;
                    s.format_with_align_name(w, ctx, &[VTAB])?;
                }
                ctx.indent_count -= 1;
                if ctx.last_end_pos.lineno == g.rparen.lineno {
                    ctx.line_break(w, 1, true)?;
                    Token::Rparen.format(w, ctx)
                } else {
                    Token::Rparen.append_pos(g.rparen).format(w, ctx)
                }
            }
        }
    }
}

impl<'a> TypeSpec<'a> {
    fn format_with_align_name<W: io::Write>(
        &self,
        w: &mut W,
        ctx: &mut Context,
        align_name: &[u8],
    ) -> io::Result<()> {
        match self {
            TypeSpec::TypeDef(t) => {
                t.ident.format(w, ctx)?;
                if let Some(p) = &t.params {
                    TypeParametersFormatter {
                        params: p,
                        tok: Token::Type,
                    }
                    .format(w, ctx)?;
                }
                w.write(align_name)?;
                t.typ.format(w, ctx)
            }
            TypeSpec::AliasDecl(t) => {
                t.ident.format(w, ctx)?;
                if let Some(p) = &t.params {
                    TypeParametersFormatter {
                        params: p,
                        tok: Token::Type,
                    }
                    .format(w, ctx)?;
                }
                w.write(align_name)?;
                Token::Assign.format(w, ctx)?;
                write_blank(w)?;
                t.typ.format(w, ctx)
            }
        }
    }
}

#[derive(Debug)]
struct ParamsFormatter<'a, 'b: 'a, T: Range + Debug> {
    open_tok: TokenPos,
    close_tok: TokenPos,
    params: &'a [&'b T],
}

trait ParamFormatter<W: io::Write> {
    fn format(&self, w: &mut W, ctx: &mut Context, do_indent_ident: bool) -> io::Result<()>;
}

impl<'a, 'b: 'a, T, W: io::Write> Formatter<W> for ParamsFormatter<'a, 'b, T>
where
    T: ParamFormatter<W> + Range + std::fmt::Debug,
{
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        self.open_tok.format(w, ctx)?;
        let [first, ..] = self.params else {
            return if !ctx.has_comments_before(&self.close_tok.pos) {
                self.close_tok.format_and_record_pos(w, ctx)
            } else {
                self.close_tok.format(w, ctx)
            };
        };

        let mut do_indent = false;
        let newline_count = first.start().lineno - self.open_tok.pos.lineno;
        if newline_count > 0 {
            do_indent = true;
            ctx.indent_count += 1;
            ctx.use_ff_when_newline = true;
        }
        first.format(w, ctx, !do_indent)?;

        for (i, p) in self.params[1..].iter().enumerate() {
            let newline_count = p.start().lineno - self.params[i].end().lineno;
            if newline_count > 0 {
                w.write(b",")?;
                if !do_indent {
                    do_indent = true;
                    ctx.indent_count += 1;
                }
                ctx.use_ff_when_newline = true;
            } else {
                w.write(b", ")?;
            }
            p.format(w, ctx, !do_indent)?;
        }

        if do_indent {
            ctx.indent_count -= 1;
        }

        let newline_count =
            self.close_tok.pos.lineno - self.params[self.params.len() - 1].end().lineno;
        if newline_count > 0 {
            w.write(b",")?;
        }
        self.close_tok.format(w, ctx)
    }
}

#[derive(Debug)]
pub struct TypeParametersFormatter<'a, 'b> {
    params: &'b TypeParameters<'a>,
    tok: Token,
}

impl<'a, 'b> Range for TypeParametersFormatter<'a, 'b> {
    fn start(&self) -> Pos {
        self.params.start()
    }

    fn end(&self) -> Pos {
        self.params.end()
    }
}

impl<'a, 'b, W: io::Write> Formatter<W> for TypeParametersFormatter<'a, 'b> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        let open_tok = TokenPos {
            tok: Token::Lbrack,
            pos: self.params.lbrack,
        };
        let close_tok = TokenPos {
            tok: Token::Rbrack,
            pos: self.params.rbrack,
        };
        if self.need_append_comma() {
            ParamsFormatter {
                open_tok,
                close_tok,
                params: &[&TypeParamDeclAndComma {
                    decl: &self.params.param_list[0].0,
                }],
            }
            .format(w, ctx)
        } else {
            ParamsFormatter {
                open_tok,
                close_tok,
                params: &self
                    .params
                    .param_list
                    .iter()
                    .map(|(p, _)| p)
                    .collect::<Vec<_>>(),
            }
            .format(w, ctx)
        }
    }
}

impl<'a, 'b> TypeParametersFormatter<'a, 'b> {
    fn need_append_comma(&self) -> bool {
        let [(first, _)] = &self.params.param_list[..] else {
            return false;
        };

        if !first.type_constraint.followers.is_empty() {
            return false;
        }

        if !first.idents.followers.is_empty() {
            return false;
        }

        self.tok == Token::Type
            && Self::can_combine_with_name(strip_parens_always(&first.type_constraint.term))
    }

    fn can_combine_with_name(expr: &Expression<'a>) -> bool {
        match expr {
            Expression::Operation(op) => {
                if op.op.op == Operator::Mul && op.y.is_none() {
                    // name *x.X
                    return !Self::is_type_elem(&op.x);
                }

                if let Some(y) = &op.y {
                    Self::can_combine_with_name(&op.x) && !Self::is_type_elem(y)
                } else {
                    false
                }
            }
            Expression::ParenedExpr(pe) => !Self::is_type_elem(&pe.expr),
            _ => false,
        }
    }

    fn is_type_elem(expr: &Expression<'a>) -> bool {
        match expr {
            Expression::ArrayType(_)
            | Expression::Struct(_)
            | Expression::FuncType(_)
            | Expression::InterfaceType(_)
            | Expression::MapType(_)
            | Expression::SliceType(_)
            | Expression::ChanType(_) => true,
            Expression::Operation(op) => {
                if op.op.op == Operator::Tilde {
                    return true;
                }

                if let Some(y) = &op.y
                    && Self::is_type_elem(y)
                {
                    return true;
                }

                Self::is_type_elem(&op.x)
            }
            _ => false,
        }
    }
}

#[derive(Debug)]
struct TypeParamDeclAndComma<'a, 'b: 'a> {
    decl: &'b TypeParamDecl<'a>,
}

impl<'a, 'b: 'a, W: io::Write> ParamFormatter<W> for TypeParamDeclAndComma<'a, 'b> {
    fn format(&self, w: &mut W, ctx: &mut Context, do_indent_ident: bool) -> io::Result<()> {
        self.decl
            .idents
            .format_with_indent(w, ctx, do_indent_ident)?;
        write_blank(w)?;
        self.decl.type_constraint.format(w, ctx)?;
        w.write(b",")?;
        Ok(())
    }
}

impl<'a, 'b: 'a> Range for TypeParamDeclAndComma<'a, 'b> {
    fn start(&self) -> Pos {
        self.decl.start()
    }

    fn end(&self) -> Pos {
        self.decl.end()
    }
}

impl<'a, W: io::Write> ParamFormatter<W> for TypeParamDecl<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context, do_indent_ident: bool) -> io::Result<()> {
        self.idents.format_with_indent(w, ctx, do_indent_ident)?;
        write_blank(w)?;
        self.type_constraint.format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for TypeElem<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        strip_parens_always(&self.term).format(w, ctx)?;
        if self.followers.is_empty() {
            return Ok(());
        }

        for tt in &self.followers {
            tt.format(w, ctx)?;
        }
        Ok(())
    }
}

impl<'a, W: io::Write> Formatter<W> for OrTypeTerm<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        write_blank(w)?;
        Operator::Or.format(w, ctx)?;

        let newline_count = self.term.start().lineno - self.or.lineno;
        if newline_count == 0 {
            write_blank(w)?;
            strip_parens_always(&self.term).format(w, ctx)
        } else {
            ctx.indent_count += 1;
            strip_parens_always(&self.term).format(w, ctx)?;
            ctx.indent_count -= 1;
            Ok(())
        }
    }
}

impl<'a> TypeInstance<'a> {
    fn format<W: io::Write>(&self, w: &mut W, ctx: &mut Context, depth: usize) -> io::Result<()> {
        ExprFormatter::format_expr(&self.type_name, w, ctx, Precedence::Highest, 1)?;
        Token::Lbrack.append_pos(self.lbrack).format(w, ctx)?;
        if self.type_args.len() > 1 {
            ListExprFormatter {
                depth: depth + 1,
                add_comma_when_newline_of_last_expr: true,
                prev_pos: Some(self.lbrack),
                next_pos: Some(self.rbrack),
                ..ListExprFormatter::new(&self.type_args)
            }
            .format(w, ctx)?;
        } else {
            ctx.ignore_space(&self.type_args[0].0.start());
            ExprFormatter {
                depth: depth + 1,
                ..ExprFormatter::new(&self.type_args[0].0)
            }
            .format(w, ctx)?;
            ctx.ignore_space(&self.rbrack);
        }
        ctx.max_newlines = 1;
        Token::Rbrack.append_pos(self.rbrack).format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for ImportDecl<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::Import.append_pos(self.pos).format(w, ctx)?;
        let pos = match &self.import {
            ImportOption::Group(g) => g.start(),
            ImportOption::Spec(s) => s.start(),
        };
        FormatterWithComment {
            pos,
            end: pos,
            v: b" ",
        }
        .format(w, ctx)?; // comment between 'import' token and import group&spec
        match &self.import {
            ImportOption::Spec(s) => s.format(w, ctx),
            ImportOption::Group(g) => g.format(w, ctx),
        }
    }
}

impl<'a, W: io::Write> Formatter<W> for ImportGroup<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::Lparen.format(w, ctx)?;
        ctx.indent_count += 1;

        const C: usize = 1024;
        #[allow(invalid_value)]
        let mut fs: [ImportSpecFormatter; C] = unsafe { MaybeUninit::uninit().assume_init() };

        let mut from = 0;
        let spec_count = self.specs.len();
        let mut last_pos_lineno = self.specs[0].start().lineno - 1;
        while from < spec_count {
            let end = self.next_specs_chunk(from);
            let specs = &self.specs[from..end];
            from = end;

            // format the block comment if any

            ctx.use_ff_when_newline = true;
            if specs[0].doc.is_some() {
                // consume the block comment
                FormatterWithComment {
                    v: b"",
                    pos: specs[0].start(),
                    end: specs[0].start(),
                }
                .format(w, ctx)?;
            } else {
                ctx.write_formfeed_newlines(w, specs[0].start_lineno(ctx) - last_pos_lineno, true)?;
                write_indents(w, ctx.indent_count)?;
            }

            let mut len = 0;
            for s in specs {
                let (left, right) = ctx.consume_surrent_line_comments_for_import_spec_chunk(
                    &s.start(),
                    &Pos {
                        col: 0,
                        lineno: &s.start().lineno + 1,
                    },
                );
                fs[len] = ImportSpecFormatter {
                    left_comments: left,
                    right_comments: right,
                    spec: &s,
                };
                len += 1;
            }

            last_pos_lineno = fs[len - 1].end_lineno();
            ctx.last_end_pos = fs[len - 1].end_pos();

            Self::sort_specs(ctx, &mut fs[..len]);
            len = Self::dedup_specs(&mut fs[..len]);

            fs[0].format(w, ctx)?;
            for f in &fs[1..len] {
                if let [c, ..] = f.left_comments
                    && c.start().lineno != c.end().lineno
                {
                    // full comment crossing two line
                    write_blank(w)?;
                } else {
                    ctx.write_formfeed_newlines(w, 1, false)?;
                    write_indents(w, ctx.indent_count)?;
                }
                f.format(w, ctx)?;
            }
        }

        if ctx.has_comments_before(&self.rparen) {
            ctx.format_comments_before_pos(w, &self.rparen, b" ")?;
        }
        ctx.write_formfeed_newlines(w, 1, true)?;
        ctx.indent_count -= 1;
        Token::Rparen
            .append_pos(self.rparen)
            .format_and_record_pos(w, ctx)?;
        Ok(())
    }
}

impl<'a> ImportGroup<'a> {
    fn next_specs_chunk(&self, from: usize) -> usize {
        let mut from = from + 1;
        let l = self.specs.len();
        while from < l {
            if self.specs[from].start().lineno - 1 > self.specs[from - 1].end().lineno {
                return from;
            }
            from += 1;
        }
        return l;
    }

    fn sort_specs(ctx: &Context, spec_fs: &mut [ImportSpecFormatter]) {
        spec_fs.sort_by(|a, b| {
            let r = a.spec.path.value[1..a.spec.path.value.len() - 1]
                .cmp(&b.spec.path.value[1..b.spec.path.value.len() - 1]);
            if r != Ordering::Equal {
                return r;
            }
            let r = a
                .spec
                .pkg_name
                .map(|i| i.name)
                .cmp(&b.spec.pkg_name.map(|i| i.name));

            if r != Ordering::Equal {
                return r;
            }
            a.spec
                .line_comment
                .map(|l| ctx.get_comment_content(l))
                .cmp(&b.spec.line_comment.map(|l| ctx.get_comment_content(l)))
        });
    }

    fn dedup_specs(specs: &mut [ImportSpecFormatter]) -> usize {
        let mut next = 1;
        let mut len = specs.len();
        while next < len {
            let last = &specs[next - 1];
            let s = &specs[next];
            if last.spec.pkg_name.map(|n| n.name) == s.spec.pkg_name.map(|n| n.name)
                && last.spec.path.value == s.spec.path.value
                && last.spec.line_comment.is_none()
            {
                unsafe {
                    let index = next - 1;
                    // the place we are taking from.
                    let ptr = specs.as_mut_ptr().add(index);

                    // Shift everything down to fill in that spot.
                    ptr::copy(ptr.add(1), ptr, len - index - 1);
                    len -= 1;
                }
            } else {
                next += 1;
            }
        }
        len
    }
}

#[derive(Debug)]
struct ImportSpecFormatter<'a, 'b, 'c> {
    left_comments: &'c [CommentGroup<'a>],
    spec: &'b ImportSpec<'a>,
    right_comments: &'c [CommentGroup<'a>],
}

impl<'a, 'b, 'c> Range for ImportSpecFormatter<'a, 'b, 'c> {
    fn start(&self) -> Pos {
        self.spec.start()
    }

    fn end(&self) -> Pos {
        self.spec.end()
    }
}

impl<'a, 'b, 'c, W: io::Write> Formatter<W> for ImportSpecFormatter<'a, 'b, 'c> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        self.format0(w, ctx)
    }
}

impl<'a, 'b, 'c> ImportSpecFormatter<'a, 'b, 'c> {
    fn end_lineno(&self) -> u32 {
        if let [.., c] = self.right_comments {
            return c.end().lineno;
        }

        self.spec.end().lineno
    }

    fn end_pos(&self) -> Pos {
        if let [.., c] = self.right_comments {
            c.end()
        } else {
            self.spec.end()
        }
    }

    fn format_comment<W: io::Write>(
        &self,
        comments: &[CommentGroup], // > 0
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        comments[0].format(w, ctx)?;

        for (i, c) in comments[1..].iter().enumerate() {
            let newline_count = c.start().lineno - comments[i].end().lineno;
            if newline_count > 0 {
                ctx.write_newlines(w, newline_count.min(MAX_NEWLINES))?;
                write_indents(w, ctx.indent_count)?;
            } else {
                w.write(b" ")?; // for simplify the vtab for the line comment
            }
            c.format(w, ctx)?;
        }
        Ok(())
    }

    fn format0<W: io::Write>(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        if let [.., c] = self.left_comments {
            self.format_comment(self.left_comments, w, ctx)?;
            let newline_count = self.spec.start().lineno - c.end().lineno;
            if newline_count > 0 {
                ctx.write_formfeed_newlines(w, newline_count, true)?;
                write_indents(w, ctx.indent_count)?;
            } else {
                write_blank(w)?;
            }
        }

        if let Some(name) = self.spec.pkg_name {
            w.write(name.name.as_bytes())?;
            // name.format(w, ctx)?;
            write_blank(w)?;
        };

        // self.spec.path.format(w, ctx)?;
        let pv = self.spec.path.value.as_bytes();
        if pv[0] == b'"' {
            w.write(self.spec.path.value.as_bytes())?;
        } else {
            // '`'
            w.write(b"\"")?;
            w.write(&pv[1..pv.len() - 1])?;
            w.write(b"\"")?;
        }
        if self.right_comments.len() > 0 {
            w.write(&[VTAB])?;
            self.format_comment(self.right_comments, w, ctx)?;
        }
        Ok(())
    }
}

impl<'a, W: io::Write> Formatter<W> for ImportSpec<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        if let Some(name) = self.pkg_name {
            ctx.format_line_comments_before(w, &name.start())?;
            name.format(w, ctx)?;
            write_blank(w)?;
            ctx.format_line_comments_between(w, &name.end(), &self.path.end())?;
        } else {
            ctx.format_line_comments_before(w, &self.path.start())?;
        };

        self.path.format(w, ctx)?;

        Ok(())
    }
}

impl<'a> ImportSpec<'a> {
    fn start_lineno(&self, ctx: &Context) -> u32 {
        if let Some(c) = self.doc {
            ctx.comments[c as usize].comments[0].pos.lineno
        } else {
            self.start().lineno
        }
    }
}

impl<'a, W: io::Write> Formatter<W> for Stmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        match self {
            Self::ConstDecl(v) => VarDeclFormatter {
                tok: Token::Const,
                v: &v,
            }
            .format(w, ctx),
            Self::VarDecl(v) => VarDeclFormatter {
                tok: Token::Var,
                v: &v,
            }
            .format(w, ctx),
            Self::TypeDecl(t) => t.format(w, ctx),
            Stmt::Block(b) => b.format(w, ctx),
            Stmt::Label(l) => l.format(w, ctx),
            Stmt::Expr(e) => e.format(w, ctx),
            Stmt::Send(s) => s.format(w, ctx),
            Stmt::IncDec(id) => id.format(w, ctx),
            Stmt::Assign(a) => a.format(w, ctx),
            Stmt::Semi(pos) => Token::Semi.append_pos(*pos).format(w, ctx),
            Stmt::Empty => Ok(()),
            Stmt::Call(c) => c.format(w, ctx),
            Stmt::Return(r) => r.format(w, ctx),
            Stmt::Branch(b) => b.format(w, ctx),
            Stmt::If(f) => f.format(w, ctx),
            Stmt::Switch(s) => s.format(w, ctx),
            Stmt::Select(s) => s.format(w, ctx),
            Stmt::For(f) => f.format(w, ctx),
        }
    }
}

impl<'a, W: io::Write> Formatter<W> for BranchStmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        self.tok.format(w, ctx)?;
        if let Some(i) = &self.label {
            write_blank(w)?;
            i.format(w, ctx)?;
        }
        Ok(())
    }
}

impl<'a> ForStmt<'a> {
    fn simplify_expr_of_range<'b>(expr: &'b Expression<'a>) -> Option<&'b Expression<'a>> {
        if let Expression::Ident(i) = expr {
            return if i.name == "_" { None } else { Some(expr) };
        }

        let Expression::ListExpr(l) = expr else {
            return Some(expr);
        };

        if let Expression::Ident(i) = &l[0].0 {
            return if i.name == "_" {
                if let Expression::Ident(j) = &l[1].0
                    && j.name == "_"
                {
                    None
                } else {
                    Some(expr)
                }
            } else {
                if let Expression::Ident(j) = &l[1].0
                    && j.name == "_"
                {
                    Some(&l[0].0)
                } else {
                    Some(expr)
                }
            };
        }

        Some(expr)
    }
}

impl<'a, W: io::Write> Formatter<W> for ForStmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::For.append_pos(self.pos).format(w, ctx)?;
        match &self.opt {
            ForOption::Cond(e) => {
                write_blank(w)?;
                strip_parans(e).format(w, ctx)?;
            }
            ForOption::Range(r) => {
                if let Some((expr, tp)) = &r.value {
                    if let Some(expr) = Self::simplify_expr_of_range(expr) {
                        write_blank(w)?;
                        expr.format(w, ctx)?;
                        write_blank(w)?;
                        tp.tok.format(w, ctx)?;
                    }
                }
                write_blank(w)?;
                Token::Range.format(w, ctx)?;
                write_blank(w)?;
                strip_parans(&r.expr).format(w, ctx)?;
            }
            ForOption::Empty => (),
            ForOption::ForClause(c) => match (&c.init, &c.cond, &c.post) {
                (Stmt::Empty, None, Stmt::Empty) => (),
                (Stmt::Empty, Some(cc), Stmt::Empty) => {
                    write_blank(w)?;
                    strip_parans(&cc).format(w, ctx)?;
                }
                _ => {
                    write_blank(w)?;
                    c.init.format(w, ctx)?;
                    Token::Semi.format(w, ctx)?;
                    write_blank(w)?;
                    if let Some(cc) = &c.cond {
                        strip_parans(cc).format(w, ctx)?;
                    }
                    Token::Semi.format(w, ctx)?;
                    if c.post != Stmt::Empty {
                        write_blank(w)?;
                        c.post.format(w, ctx)?;
                    }
                }
            },
        }
        write_blank(w)?;
        self.body.format(w, ctx)
    }
}

impl<'a> IndentEater for CommClause<'a> {}

impl<'a, W: io::Write> Formatter<W> for SelectStmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::Select.append_pos(self.pos).format(w, ctx)?;
        if self.body.is_empty() && !ctx.has_comments_before(&self.rbrace) {
            w.write(b" {}")?;
            ctx.last_end_pos = self.rbrace;
            return Ok(());
        }

        write_blank(w)?;
        BlockStmtFormatter {
            sep_after_lbrace_when_empty: &[VTAB],
            lbrace: self.lbrace,
            stmts: self.body.as_slice(),
            rbrace: self.rbrace,
            indent_count: 0,
            max_newlines_before_rbrace: MAX_NEWLINES,
        }
        .format(w, ctx)?;
        Ok(())
    }
}

impl<'a> IsEmptyStmt for CommClause<'a> {}

impl<'a, W: io::Write> Formatter<W> for CommClause<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        let is_case_tok = Stmt::Empty == self.comm.0;
        if is_case_tok {
            format_comment_with_indent_before(w, ctx, &self.comm.1.pos)?;
        }

        self.comm.1.format(w, ctx)?;
        if !is_case_tok {
            write_blank(w)?;
            self.comm.0.format(w, ctx)?;
        }
        Token::Colon.append_pos(self.colon).format(w, ctx)?;
        if !self.body.is_empty() {
            // ctx.format_comments_when_newlines(w, &self.body[0].start(), true, &[BLANK])?;

            ctx.indent_count += 1;
            BlockStmtFormatter::format_stmts(self.body.as_slice(), w, ctx)?;
            ctx.indent_count -= 1;
        }

        Ok(())
    }
}

impl<'a> IndentEater for CaseCause<'a> {}

fn is_type_name(expr: &Expression) -> bool {
    match expr {
        Expression::Ident(_) => true,
        Expression::Selector(s) => is_type_name(&s.pexpr),
        _ => false,
    }
}

impl<'a> Expression<'a> {
    fn inspect_listexpr<T, F: FnMut(&Expression) -> bool>(es: &Vec<(Expression, T)>, f: &mut F) {
        for (e, _) in es {
            e.inspect(f);
        }
    }

    fn inspect<F: FnMut(&Expression) -> bool>(&self, f: &mut F) {
        if !f(&self) {
            return;
        }

        match self {
            Self::CompositeLit(c) => {
                c.typ.inspect(f);
                Self::inspect_listexpr(&c.value.elem_list, f);
            }
            Self::LiteralValue(l) => {
                Self::inspect_listexpr(&l.elem_list, f);
            }
            Self::TypeInstance(t) => {
                Self::inspect_listexpr(&t.type_args, f);
                t.type_name.inspect(f);
            }
            Self::Operation(o) => {
                o.x.inspect(f);
                if let Some(y) = &o.y {
                    y.inspect(f);
                }
            }
            Self::Index(i) => {
                i.expr.inspect(f);
                i.indices.inspect(f);
            }
            Self::Struct(s) => {
                for (field, _) in &s.field_decls {
                    match field {
                        FieldDeclOption::Fields(fields) => {
                            fields.typ.inspect(f);
                        }
                        FieldDeclOption::Embedded(embedded) => {
                            embedded.typ.inspect(f);
                        }
                    }
                }
            }
            Self::SliceType(s) => {
                s.typ.inspect(f);
            }
            Self::MapType(m) => {
                m.key_type.inspect(f);
                m.ele_type.inspect(f);
            }
            Self::ChanType(c) => {
                c.typ.inspect(f);
            }
            Self::FuncType(f_type) => {
                // Inspect parameters
                if let Some(params) = &f_type.params.list {
                    for (param, _) in params {
                        param.typ.inspect(f);
                    }
                }
                // Inspect return type
                if let Some(ret) = &f_type.ret {
                    match ret {
                        FuncResult::Params(params) => {
                            if let Some(param_list) = &params.list {
                                for (param, _) in param_list {
                                    param.typ.inspect(f);
                                }
                            }
                        }
                        FuncResult::Type(typ) => {
                            typ.inspect(f);
                        }
                    }
                }
            }
            Self::InterfaceType(i) => {
                for elem_and_semi in &i.elems {
                    match &elem_and_semi.elem {
                        InterfaceElem::MethodElem(method) => {
                            // Inspect method signature
                            if let Some(params) = &method.sign.params.list {
                                for (param, _) in params {
                                    param.typ.inspect(f);
                                }
                            }
                            if let Some(ret) = &method.sign.ret {
                                match ret {
                                    FuncResult::Params(params) => {
                                        if let Some(param_list) = &params.list {
                                            for (param, _) in param_list {
                                                param.typ.inspect(f);
                                            }
                                        }
                                    }
                                    FuncResult::Type(typ) => {
                                        typ.inspect(f);
                                    }
                                }
                            }
                        }
                        InterfaceElem::TypeElem(type_elem) => {
                            type_elem.term.inspect(f);
                            for follower in &type_elem.followers {
                                follower.term.inspect(f);
                            }
                        }
                    }
                }
            }
            Self::ArrayType(a) => {
                a.len.inspect(f);
                a.typ.inspect(f);
            }
            Self::KeyedElement(k) => {
                if let Some((key, _)) = &k.key_and_colon {
                    key.inspect(f);
                }
                k.elem.inspect(f);
            }
            Self::BasicLit(_) => {
                // No nested expressions
            }
            Self::FuncLit(f_lit) => {
                // Inspect function signature
                if let Some(params) = &f_lit.sign.params.list {
                    for (param, _) in params {
                        param.typ.inspect(f);
                    }
                }
                if let Some(ret) = &f_lit.sign.ret {
                    match ret {
                        FuncResult::Params(params) => {
                            if let Some(param_list) = &params.list {
                                for (param, _) in param_list {
                                    param.typ.inspect(f);
                                }
                            }
                        }
                        FuncResult::Type(typ) => {
                            typ.inspect(f);
                        }
                    }
                }
            }
            Self::UnderlyingType(u) => {
                u.typ.inspect(f);
            }
            Self::ListExpr(le) => {
                Self::inspect_listexpr(le, f);
            }
            Self::ParenedExpr(p) => {
                p.expr.inspect(f);
            }
            Self::CallExpr(c) => {
                c.pexpr.inspect(f);
                for (arg, _) in &c.args {
                    arg.inspect(f);
                }
            }
            Self::Selector(s) => {
                s.pexpr.inspect(f);
            }
            Self::TypeAssert(t) => {
                t.pexpr.inspect(f);
                t.typ.inspect(f);
            }
            Self::TypeSwitchGuard(t) => {
                t.x.inspect(f);
            }
            Self::SliceExpr(s) => {
                s.expr.inspect(f);
                for index in &s.indice {
                    index.inspect(f);
                }
            }
            Self::DotDotDotArgs(a) => {
                a.expr.inspect(f);
            }
            Self::Token(_) => {
                // No nested expressions
            }
            Self::Ident(_) => {
                // No nested expressions
            }
            Self::QualifiedName(_) => {
                // No nested expressions
            }
            Self::Expr(e) => {
                e.inspect(f);
            }
            Self::ReplaceMe => {
                // Placeholder, no inspection needed
            }
        };
    }
}

fn strip_parans<'a>(p: &'a Expression<'a>) -> &'a Expression<'a> {
    let Expression::ParenedExpr(bp) = p else {
        return p;
    };

    let mut do_strip = true;
    bp.expr.inspect(&mut |e| match e {
        Expression::ParenedExpr(_) => false,
        Expression::CompositeLit(cl) => {
            if is_type_name(&cl.typ) {
                do_strip = false;
            }
            false
        }
        _ => true,
    });

    if do_strip { strip_parans(&bp.expr) } else { p }
}

fn strip_parens_always<'a, 'b>(p: &'a Expression<'b>) -> &'a Expression<'b> {
    if let Expression::ParenedExpr(p) = p {
        strip_parens_always(&p.expr)
    } else {
        p
    }
}

impl<'a, W: io::Write> Formatter<W> for SwitchStmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::Switch.append_pos(self.pos).format(w, ctx)?;
        if let Some((i, pos)) = &self.init
            && *i != Stmt::Empty
        {
            write_blank(w)?;
            i.format(w, ctx)?;
            if !pos.is_zero() {
                Token::Semi.format(w, ctx)?;
            }
        }

        if let Some(t) = &self.tag {
            write_blank(w)?;
            (strip_parans(t)).format(w, ctx)?;
        }

        write_blank(w)?;
        BlockStmtFormatter {
            sep_after_lbrace_when_empty: &[VTAB],
            lbrace: self.lbrace,
            stmts: self.body.as_slice(),
            rbrace: self.rbrace,
            indent_count: 0,
            max_newlines_before_rbrace: MAX_NEWLINES,
        }
        .format(w, ctx)?;
        Ok(())
    }
}

impl<'a, W: io::Write> Formatter<W> for CaseCause<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        format_comment_with_indent_before(w, ctx, &self.tok_pos.pos)?;
        self.tok_pos.format(w, ctx)?;

        if let Some(e) = &self.expr {
            if e.start().lineno == self.tok_pos.pos.lineno {
                write_blank(w)?;
            }
            if let Expression::ListExpr(l) = e {
                ListExprFormatter {
                    prev_pos: Some(self.tok_pos.pos),
                    next_pos: Some(self.colon),
                    ..ListExprFormatter::new(l)
                }
                .format(w, ctx)?;
            } else {
                ExprFormatter::new(e).format(w, ctx)?;
            }
        }
        Token::Colon.format(w, ctx)?;
        if !self.body.is_empty() {
            ctx.indent_count += 1;
            BlockStmtFormatter::format_stmts(self.body.as_slice(), w, ctx)?;
            ctx.indent_count -= 1;
        }

        Ok(())
    }
}

impl<'a, W: io::Write> Formatter<W> for ReturnStmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::Return.append_pos(self.pos).format(w, ctx)?;
        if let Some(e) = &self.expr {
            write_blank(w)?;
            ReturnStmt::format_expr(e, w, ctx)?;
        }
        Ok(())
    }
}

impl<'a> ReturnStmt<'a> {
    fn add_indents(l: &ListExpr<'a>) -> u32 {
        if l.len() == 1 {
            return 0;
        }

        if l.start().lineno == l.end().lineno {
            return 0;
        }

        let mut n = 0; // list span multiple lines
        let mut line = l[0].0.start().lineno;
        for (l0, _) in l {
            let (xb, xe) = (l0.start().lineno, l0.end().lineno);
            if line < xb {
                return 1;
            }

            if xb < xe {
                n += 1;
            }

            line = xe;
        }

        if n > 1 { 1 } else { 0 }
    }

    fn format_expr<W: io::Write>(
        expr: &Expression<'a>,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        let Expression::ListExpr(l) = expr else {
            return ExprFormatter::format_expr(expr, w, ctx, Precedence::None, 1);
        };

        let new_indents = Self::add_indents(l);
        ctx.indent_count += new_indents;
        ListExprFormatter {
            indent_when_newline: if new_indents > 0 { 0 } else { 1 },
            ..ListExprFormatter::new(l)
        }
        .format(w, ctx)?;
        ctx.indent_count -= new_indents;
        Ok(())
    }
}

impl Token {
    fn append_pos(self, pos: Pos) -> TokenPos {
        TokenPos { tok: self, pos }
    }
}

impl Operator {
    fn append_pos(self, pos: Pos) -> OperatorPos {
        OperatorPos { op: self, pos }
    }
}

impl<'a, W: io::Write> Formatter<W> for IfStmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::If.append_pos(self.pos).format(w, ctx)?;
        write_blank(w)?;
        if let Some((s, _)) = &self.init
            && *s != Stmt::Empty
        {
            ctx.ignore_space(&s.start());
            s.format(w, ctx)?;
            Token::Semi.format(w, ctx)?;
            write_blank(w)?;
        }
        ctx.ignore_space(&self.cond.start());
        strip_parans(&self.cond).format(w, ctx)?;
        write_blank(w)?;
        self.block.format(w, ctx)?;
        let Some((_, s)) = &self.r#else else {
            return Ok(());
        };
        write_blank(w)?;
        Token::Else.format(w, ctx)?;
        write_blank(w)?;
        s.format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for CallStmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        self.tok.format(w, ctx)?;
        ctx.ignore_space(&self.stmt.start());
        write_blank(w)?;
        self.stmt.format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for IncDecStmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        ExprFormatter::format_expr(&self.expr, w, ctx, Precedence::None, 2)?;
        self.op.format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for SendStmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        self.chan.format(w, ctx)?;
        write_blank(w)?;
        Token::Arrow.format(w, ctx)?;
        write_blank(w)?;
        self.value.format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for AssignStmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        let mut depth = 1;
        let (el, vl) = match (&self.expr, &self.value) {
            (Expression::ListExpr(el), Expression::ListExpr(vl)) => {
                if el.len() > 1 && vl.len() > 1 {
                    depth += 1;
                }
                (el, vl)
            }
            (Expression::ListExpr(el), expr) => (el, &vec![(Expression::Expr(expr), Pos::zero())]),
            (el, vl) => (
                &vec![(Expression::Expr(el), Pos::zero())],
                &vec![(Expression::Expr(vl), Pos::zero())],
            ),
        };

        ListExprFormatter {
            depth,
            prev_pos: Some(el.start()),
            next_pos: Some(self.op.pos),
            ..ListExprFormatter::new(el)
        }
        .format(w, ctx)?;
        write_blank(w)?;
        self.op.format(w, ctx)?;
        // no comment or not line comment
        // let lineno_opt = ctx.first_comment_start_lineno_before(&self.value.start());
        // if lineno_opt.is_none() || lineno_opt.unwrap() != self.op.pos.lineno {
        //     write_blank(w)?;
        // }
        if self.op.pos.lineno == self.value.start().lineno {
            write_blank(w)?;
        }
        ListExprFormatter {
            depth,
            prev_pos: Some(self.op.pos),
            ..ListExprFormatter::new(vl)
        }
        .format(w, ctx)?;
        Ok(())
    }
}

fn format_comment_with_indent_before<W: io::Write>(
    w: &mut W,
    ctx: &mut Context,
    pos: &Pos,
) -> io::Result<()> {
    ctx.format_comments_before_pos_align(w, pos, &[VTAB])?;

    Ok(())
}

impl<'a, W: io::Write> Formatter<W> for LabelStmt<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        ctx.force_newline = ctx.has_tok_before_in_same_line(&self.label.start());
        ctx.indent_count += 1; // for the comments
        ctx.unindent_when_newline = 1;
        self.label.format(w, ctx)?;
        ctx.unindent_when_newline = 0;
        ctx.indent_count -= 1;

        Token::Colon.format(w, ctx)?;
        match &self.stmt {
            Some(Stmt::Empty) | Some(Stmt::Semi(_)) if ctx.is_next_rbrace() => (),
            Some(s) => {
                ctx.indent_count += 1;
                ctx.use_ff_when_newline = true;
                ctx.force_newline = true;
                s.format(w, ctx)?;
                ctx.indent_count -= 1;
            }
            _ => (),
        }

        Ok(())
    }
}

#[derive(Debug)]
struct VarDeclFormatter<'a> {
    tok: Token,
    v: &'a VarDecl<'a>,
}

impl<'a, W: io::Write> Formatter<W> for VarDeclFormatter<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        self.tok.append_pos(self.v.pos).format(w, ctx)?;
        w.write(b" ")?;
        let pos = match &self.v.decl {
            VarDeclOption::Spec(s) => s.start(),
            VarDeclOption::Group(g) => g.lparen,
        };
        ctx.ignore_space(&pos);
        self.v.format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for VarDecl<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        match &self.decl {
            VarDeclOption::Spec(s) => s.format(w, ctx),
            VarDeclOption::Group(g) => g.format(w, ctx),
        }
    }
}

impl<'a, W: io::Write> Formatter<W> for VarSpec<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        self.ident_list.format(w, ctx)?;
        if let Some(t) = &self.typ {
            SepWhenSameLineFormatter::format(t, b" ", w, ctx)?;
        }

        if let Some(v) = &self.expr_list {
            SepWhenSameLineFormatter::format(
                &TokenPos {
                    pos: self.eq.unwrap(),
                    tok: Token::Assign,
                },
                b" ",
                w,
                ctx,
            )?;
            ctx.ignore_space(&v.start());
            SepWhenSameLineFormatter::format(v, b" ", w, ctx)?;
        }

        if self.line_comment.is_some() {
            ctx.format_comments_before_pos(
                w,
                &Pos {
                    col: 0,
                    lineno: self.end().lineno + 1,
                },
                &[VTAB],
            )?;
        }
        Ok(())
    }
}

impl<'a, W: io::Write> Formatter<W> for IdentifierList<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        self.format_with_indent(w, ctx, true)
    }
}

impl<'a> IdentifierList<'a> {
    fn format_with_indent<W: io::Write>(
        &self,
        w: &mut W,
        ctx: &mut Context,
        do_indent: bool,
    ) -> io::Result<()> {
        const N: usize = 128;
        let mut exprs: [MaybeUninit<Expression>; N] =
            unsafe { MaybeUninit::uninit().assume_init() };
        exprs[0].write(Expression::Ident(Box::new(self.ident)));
        let mut i = 1;
        for f in &self.followers {
            exprs[i].write(Expression::Ident(Box::new(f.ident)));
            i += 1;
        }

        let mut exprs_ref: [MaybeUninit<&Expression>; N] =
            unsafe { MaybeUninit::uninit().assume_init() };
        for k in 0..i {
            exprs_ref[k].write(unsafe { exprs[k].assume_init_ref() });
        }

        ListExprFormatter {
            indent_when_newline: if do_indent { 1 } else { 0 },
            ..ListExprFormatter::new(
                &unsafe { std::mem::transmute::<_, [&Expression; N]>(exprs_ref) }[..i],
            )
        }
        .format(w, ctx)?;
        Ok(())
    }
}

impl<'a, W: io::Write> Formatter<W> for VarGroup<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::Lparen
            .append_pos(self.lparen)
            .format_and_record_pos(w, ctx)?;
        ctx.indent_count += 1;

        let keep_types = self.keep_type_column();
        let mut spec_cross_multi_lines = 0;
        if let [(first, _), ..] = &self.specs[..] {
            ctx.use_ff_when_newline = true;
            ctx.force_newline = !ctx.has_comments_before(&first.start());
            spec_cross_multi_lines = Self::value_spec(first, w, ctx, keep_types[0])?;
        }

        for (i, (vs, _)) in self.specs.iter().skip(1).enumerate() {
            ctx.sep_before_line_comment = &[VTAB];
            ctx.force_newline = true;
            ctx.use_ff_when_newline = spec_cross_multi_lines > 0;
            spec_cross_multi_lines = Self::value_spec(vs, w, ctx, keep_types[i + 1])?;
        }
        ctx.indent_count -= 1;
        ctx.sep_before_line_comment = &[VTAB];
        ctx.force_newline = true;
        Token::Rparen.append_pos(self.rparen).format(w, ctx)?;
        Ok(())
    }
}

impl<'a> VarGroup<'a> {
    fn value_spec<W: io::Write>(
        s: &VarSpec<'a>,
        w: &mut W,
        ctx: &mut Context,
        keep_type: bool,
    ) -> io::Result<u32> {
        s.ident_list.format_with_indent(w, ctx, false)?;
        let output_lineno = ctx.output_lineno;

        let mut sep_filled_count = 0;
        if s.typ.is_some() || keep_type {
            w.write(&[VTAB])?;
            sep_filled_count += 1;
        }

        if let Some(typ) = &s.typ {
            typ.format(w, ctx)?;
        }

        if let Some(v) = &s.expr_list {
            w.write(&[VTAB])?;
            Token::Assign.format(w, ctx)?;
            w.write(b" ")?;
            v.format(w, ctx)?;
            sep_filled_count += 1;
        }

        if s.line_comment.is_some() {
            // w.write(&[VTAB, VTAB, VTAB][0..3 - sep_filled_count])?;
            w.write(&[VTAB, VTAB, VTAB][0..2 - sep_filled_count])?;
        }

        Ok(
            ctx.output_lineno - output_lineno + s.ident_list.end().lineno
                - s.ident_list.start().lineno, // idents not merge
        )
    }

    fn keep_type_column(&self) -> Vec<bool> {
        let len = self.specs.len();
        let mut r = vec![false; len];
        let mut keep = false;
        let mut i0 = -1;
        for (i, v) in self.specs.iter().enumerate() {
            if v.0.expr_list.is_some() {
                if i0 < 0 {
                    i0 = i as i32;
                    keep = false;
                }
            } else {
                if i0 >= 0 {
                    r[i0 as usize..i].fill(keep);
                    i0 = -1;
                }
            }
            if v.0.typ.is_some() {
                keep = true;
            }
        }
        if i0 >= 0 {
            r[i0 as usize..len].fill(keep);
        }
        r
    }
}

impl<'b, 'a: 'b, F> AsRef<Expression<'a>> for (Expression<'a>, F) {
    fn as_ref(&self) -> &Expression<'a> {
        &self.0
    }
}

impl<'b, 'a: 'b> AsRef<Expression<'a>> for Expression<'a> {
    fn as_ref(&self) -> &Expression<'a> {
        &self
    }
}

// format the expr list of assign&call expr&composite literal&ident list&return&index
//
// only do the indent when new line happened after one expr
#[derive(Debug)]
struct ListExprFormatter<'a, T: AsRef<Expression<'a>>> {
    expr: &'a [T], // MUST NOT empty
    prev_break: i32,
    prev_size: usize,
    size: usize,
    depth: usize,
    lnsum: f64,
    count: usize,
    indent_when_newline: u32,
    add_comma_when_newline_of_last_expr: bool,

    prev_pos: Option<Pos>,
    next_pos: Option<Pos>,
}

impl<'a, T: AsRef<Expression<'a>>> ListExprFormatter<'a, T> {
    fn new(expr: &'a [T]) -> Self {
        ListExprFormatter {
            expr,
            prev_break: -1,
            prev_size: 0,
            size: 0,
            depth: 1,
            lnsum: 0.0,
            count: 0,
            prev_pos: None,
            next_pos: None,
            add_comma_when_newline_of_last_expr: false,
            indent_when_newline: 1,
        }
    }

    fn calc_node_size(&mut self, e: &Expression<'a>, ctx: &mut Context) {
        self.prev_size = self.size;

        self.size = ctx.singleline_node_size(e);
        if self.size <= Self::INFINITY && self.prev_pos.is_some() && self.next_pos.is_some() {
            if let Expression::KeyedElement(k) = e {
                if let Some((k, _)) = &k.key_and_colon {
                    self.size = ctx.singleline_node_size(k);
                }
            }
        } else {
            self.size = 0;
        }

        if self.prev_size > 0 {
            self.count += 1;
            self.lnsum += libm::log(self.prev_size as f64);
        }
    }

    fn reset_ff_context(&mut self) {
        self.count = 0;
        self.lnsum = 0.0;
    }

    const INFINITY: usize = 1000000;

    fn use_ff(&mut self, cur: i32) -> bool {
        if self.prev_break + 1 < cur {
            // multi expression in one line
            return true;
        }

        if self.count == 0 {
            return false;
        }

        if self.size > Self::INFINITY {
            return true;
        }

        if self.prev_size == 0 {
            return false;
        }

        const SMALL_SIZE: usize = 40;
        if self.prev_size <= SMALL_SIZE && self.size <= SMALL_SIZE {
            return false;
        }

        let ratio = self.size as f64 / libm::exp(self.lnsum / self.count as f64);
        if ratio <= 0.4 || ratio >= 2.5 {
            true
        } else {
            false
        }
    }

    fn try_write_newlines<W: io::Write>(
        &mut self,
        w: &mut W,
        ctx: &mut Context,
        pos: &Pos,
        cur_index: i32,
    ) -> io::Result<bool> {
        let newline_count = pos.lineno - self.expr[cur_index as usize - 1].as_ref().end().lineno;
        if newline_count > 0 {
            w.write(b",")?;
            let use_ff = self.use_ff(cur_index);
            ctx.use_ff_when_newline = use_ff;
            if newline_count > 1 || use_ff {
                self.reset_ff_context();
            }

            self.prev_break = cur_index;
            Ok(true)
        } else {
            w.write(b", ")?;
            Ok(false)
        }
    }

    fn format<W: io::Write>(&mut self, w: &mut W, ctx: &mut Context) -> io::Result<bool> {
        let first = &self.expr[0].as_ref();
        let line = first.start().lineno;

        // same line
        if let Some(Pos { lineno, .. }) = self.prev_pos
            && lineno == line
            && line == self.expr[self.expr.len() - 1].as_ref().start().lineno
        {
            self.format_expr(w, first, ctx, false)?;
            for e in &self.expr[1..] {
                w.write(b", ")?;
                self.format_expr(w, e.as_ref(), ctx, false)?;
            }
            return Ok(true);
        }

        let mut need_newline = false;
        let mut do_indent = false;
        if let Some(Pos { lineno, .. }) = self.prev_pos
            && lineno < line
        {
            self.prev_break = 0;
            ctx.use_ff_when_newline = true;
            need_newline = true;

            do_indent = true;
            ctx.indent_count += self.indent_when_newline;
        }

        self.calc_node_size(first, ctx);
        self.format_expr(
            w,
            first,
            ctx,
            need_newline && self.expr.len() > 1 && self.size > 0 && self.size <= Self::INFINITY,
        )?;

        for (i, e) in self.expr.iter().skip(1).enumerate() {
            self.calc_node_size(e.as_ref(), ctx);
            need_newline = false;
            if self.try_write_newlines(w, ctx, &e.as_ref().start(), (i + 1) as i32)? {
                need_newline = true;
                if !do_indent {
                    do_indent = true;
                    ctx.indent_count += self.indent_when_newline;
                }
            };

            self.format_expr(
                w,
                e.as_ref(),
                ctx,
                need_newline && self.size > 0 && self.size <= Self::INFINITY,
            )?;
        }

        if do_indent {
            ctx.indent_count -= self.indent_when_newline;
        }

        if let Some(Pos { lineno, .. }) = self.next_pos
            && self.expr[self.expr.len() - 1].as_ref().end().lineno < lineno
            && self.add_comma_when_newline_of_last_expr
        {
            w.write(b",")?;
        }
        ctx.sep_before_line_comment = &[VTAB];

        Ok(false)
    }

    fn format_keyed_ele<W: io::Write>(
        ke: &'a KeyedElement,
        w: &mut W,
        ctx: &mut Context,
    ) -> io::Result<()> {
        if let Some((k, _)) = &ke.key_and_colon {
            k.format(w, ctx)?;
            Token::Colon.format(w, ctx)?;
            w.write(&[VTAB])?;
        }
        ke.elem.format(w, ctx)
    }

    fn format_expr<W: io::Write>(
        &self,
        w: &mut W,
        expr: &'a Expression<'a>,
        ctx: &mut Context,
        use_vtab_for_ke: bool,
    ) -> io::Result<()> {
        ctx.sep_before_line_comment = &[VTAB];
        match expr {
            Expression::KeyedElement(k) if use_vtab_for_ke && k.key_and_colon.is_some() => {
                Self::format_keyed_ele(k, w, ctx)
            }
            _ => ExprFormatter {
                depth: self.depth,
                ..ExprFormatter::new(expr)
            }
            .format(w, ctx),
        }
    }
}

impl<W: io::Write> Formatter<W> for Token {
    #[inline(always)]
    fn format(&self, w: &mut W, _: &mut Context) -> io::Result<()> {
        w.write(self.to_string().as_bytes()).map(|_| ())
    }
}

impl<'a, W: io::Write> Formatter<W> for InterfaceType<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::Interface.append_pos(self.pos).format(w, ctx)?;
        let has_comments_before_rbrace = ctx.has_comments_before(&self.rbrace);
        let elem_count = self.elems.len();

        if !has_comments_before_rbrace && self.lbrace.lineno == self.rbrace.lineno {
            if elem_count == 0 {
                w.write(b"{}")?;
                ctx.last_end_pos = self.rbrace;
                return Ok(());
            }

            let m = &self.elems[0].elem;
            if elem_count == 1 && self.is_oneline_elem(ctx, m) {
                Token::Lbrace.format(w, ctx)?;
                w.write(b" ")?;
                m.format(w, ctx)?;
                w.write(b" ")?;
                Token::Rbrace.format(w, ctx)?;
                return Ok(());
            }
        }

        w.write(b" ")?;
        let elems = self
            .elems
            .iter()
            .map(|i| InterfaceElemFormatter { elem: &i.elem })
            .collect::<Vec<_>>();
        BlockStmtFormatter {
            sep_after_lbrace_when_empty: &[VTAB],
            lbrace: self.lbrace,
            stmts: &elems,
            rbrace: self.rbrace,
            indent_count: 1,
            max_newlines_before_rbrace: 1,
        }
        .format(w, ctx)
    }
}

impl<'a> InterfaceType<'a> {
    fn is_oneline_elem(&self, ctx: &mut Context, e: &InterfaceElem<'a>) -> bool {
        match e {
            InterfaceElem::TypeElem(t) => StructType::can_place_one_line(t, 0, ctx),
            InterfaceElem::MethodElem(m) => StructType::can_place_one_line(m, 1, ctx),
        }
    }
}

#[derive(Debug)]
struct InterfaceElemFormatter<'a> {
    elem: &'a InterfaceElem<'a>,
}

impl<'a> Range for InterfaceElemFormatter<'_> {
    fn start(&self) -> Pos {
        self.elem.start()
    }

    fn end(&self) -> Pos {
        self.elem.end()
    }
}

impl<'a, W: io::Write> Formatter<W> for InterfaceElemFormatter<'a> {
    #[inline(always)]
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        self.elem.format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for InterfaceElem<'a> {
    #[inline(always)]
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        match self {
            InterfaceElem::TypeElem(t) => t.format(w, ctx),
            InterfaceElem::MethodElem(m) => m.format(w, ctx),
        }
    }
}

impl<'a> IsEmptyStmt for InterfaceElemFormatter<'a> {
    #[inline(always)]
    fn is_empty(&self) -> bool {
        false
    }
}

impl<'a> IndentEater for InterfaceElemFormatter<'a> {
    #[inline(always)]
    fn eat(&self) -> u32 {
        0
    }
}

impl<'a, W: io::Write> Formatter<W> for MethodElem<'a> {
    #[inline(always)]
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        self.name.format(w, ctx)?;
        self.sign.format(w, ctx)
    }
}

impl<'a, W: io::Write> Formatter<W> for StructType<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::Struct.append_pos(self.pos).format(w, ctx)?;
        let has_comments_before_rbrace = ctx.has_comments_before(&self.rbrace);
        let field_count = self.field_decls.len();

        if !has_comments_before_rbrace && self.lbrace.lineno == self.rbrace.lineno {
            if field_count == 0 {
                w.write(b"{}")?;
                ctx.last_end_pos = self.rbrace;
                return Ok(());
            }

            let (f, _) = &self.field_decls[0];
            if field_count == 1 && self.is_oneline_field(ctx, f) {
                Token::Lbrace.format(w, ctx)?;
                w.write(b" ")?;
                let (idents, typ, _, _) = f.expand_field_ele();
                if let Some(idents) = idents {
                    idents.format(w, ctx)?;
                    w.write(b" ")?;
                }
                typ.format(w, ctx)?;
                w.write(b" ")?;
                Token::Rbrace.format(w, ctx)?;
                return Ok(());
            }
        }

        write_blank(w)?;
        if field_count == 1 {
            return BlockStmtFormatter {
                sep_after_lbrace_when_empty: &[VTAB],
                lbrace: self.lbrace,
                stmts: &[OneFieldDeclFormatter {
                    field: &self.field_decls[0].0,
                }],
                rbrace: self.rbrace,
                indent_count: 1,
                max_newlines_before_rbrace: 1,
            }
            .format(w, ctx);
        }
        let fs = self
            .field_decls
            .iter()
            .map(|(field, _)| FieldDeclFormatter { field })
            .collect::<Vec<_>>();
        BlockStmtFormatter {
            sep_after_lbrace_when_empty: &[VTAB],
            lbrace: self.lbrace,
            stmts: &fs,
            rbrace: self.rbrace,
            indent_count: 1,
            max_newlines_before_rbrace: 1,
        }
        .format(w, ctx)
    }
}

#[derive(Debug)]
struct OneFieldDeclFormatter<'a> {
    field: &'a FieldDeclOption<'a>,
}

impl<'a> IndentEater for OneFieldDeclFormatter<'a> {}
impl<'a> IsEmptyStmt for OneFieldDeclFormatter<'a> {}
impl<'a> Range for OneFieldDeclFormatter<'a> {
    fn start(&self) -> Pos {
        self.field.start()
    }

    fn end(&self) -> Pos {
        self.field.end()
    }
}

impl<'a, W: io::Write> Formatter<W> for OneFieldDeclFormatter<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        let (idents, typ, tag, _) = self.field.expand_field_ele();
        if let Some(idents) = idents {
            idents.format(w, ctx)?;
            w.write(b" ")?;
        };

        typ.format(w, ctx)?;

        if let Some(tag) = tag {
            w.write(b" ")?;
            tag.format(w, ctx)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
struct FieldDeclFormatter<'a> {
    field: &'a FieldDeclOption<'a>,
}

impl<'a, W: io::Write> Formatter<W> for FieldDeclFormatter<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        let sep = &[VTAB];
        let (idents, typ, tag, comment_index) = self.field.expand_field_ele();
        let mut extra_tabs = if let Some(idents) = idents {
            idents.format_with_indent(w, ctx, false)?;
            w.write(sep)?;
            1
        } else {
            2
        };
        typ.format(w, ctx)?;

        if let Some(tag) = tag {
            w.write(sep)?;
            tag.format(w, ctx)?;
            extra_tabs = 1;
        }

        if comment_index.is_some() && extra_tabs > 1 {
            extra_tabs -= 1; // one is append by the `BlockStmtFormatter`
            w.write(&[VTAB, VTAB][..extra_tabs])?;
        }
        Ok(())
    }
}

impl<'a> IsEmptyStmt for FieldDeclFormatter<'a> {
    fn is_empty(&self) -> bool {
        false
    }
}

impl<'a> IndentEater for FieldDeclFormatter<'a> {
    fn eat(&self) -> u32 {
        0
    }
}

impl<'a> Range for FieldDeclFormatter<'a> {
    fn start(&self) -> Pos {
        self.field.start()
    }

    fn end(&self) -> Pos {
        self.field.end()
    }
}

impl<'a> FieldDeclOption<'a> {
    fn expand_field_ele<'b>(
        &'a self,
    ) -> (
        Option<&'b IdentifierList<'a>>,
        &'b Expression<'a>,
        Option<&'b BasicLit<'a>>,
        Option<CommentIndex>,
    ) {
        match self {
            FieldDeclOption::Fields(Fields {
                idents,
                typ,
                tag: Some(t),
                line_comment,
                ..
            }) => (Some(idents), typ, Some(t), *line_comment),
            FieldDeclOption::Fields(Fields {
                idents,
                typ,
                tag: None,
                line_comment,
                ..
            }) => (Some(idents), typ, None, *line_comment),
            FieldDeclOption::Embedded(efp) => match efp {
                EmbeddedFieldTypeField {
                    tag: Some(t),
                    typ,
                    line_comment,
                    ..
                } => (None, typ, Some(t), *line_comment),
                EmbeddedFieldTypeField {
                    tag: None,
                    typ,
                    line_comment,
                    ..
                } => (None, typ, None, *line_comment),
            },
        }
    }
}

impl<'a> StructType<'_> {
    #[inline(always)]
    fn can_place_one_line<T: Formatter<SingleLineNodeSizeWriter> + Range>(
        typ: &T,
        name_size: usize,
        ctx: &mut Context,
    ) -> bool {
        ctx.singleline_node_size(typ) + name_size <= 30
    }

    fn is_oneline_field(&self, ctx: &mut Context, f: &FieldDeclOption) -> bool {
        match &f {
            FieldDeclOption::Fields(Fields { tag: Some(_), .. }) => false,
            FieldDeclOption::Fields(Fields { typ, tag: None, .. }) => {
                Self::can_place_one_line(typ, 1, ctx)
            }
            FieldDeclOption::Embedded(e) => e.tag.is_none(),
        }
    }
}

impl<'a, W: io::Write> Formatter<W> for SliceType<'a> {
    fn format(&self, w: &mut W, ctx: &mut Context) -> io::Result<()> {
        Token::Lbrack.append_pos(self.rbrack).format(w, ctx)?;
        Token::Rbrack.append_pos(self.rbrack).format(w, ctx)?;
        self.typ.format(w, ctx)?;
        Ok(())
    }
}

struct SimpleWriter(Vec<u8>);
impl Write for SimpleWriter {
    fn write(&mut self, b: &[u8]) -> std::io::Result<usize> {
        self.0.extend_from_slice(b);
        Ok(b.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl ToString for SimpleWriter {
    fn to_string(&self) -> String {
        unsafe { std::str::from_utf8_unchecked(self.0.as_slice()).to_string() }
    }
}

#[allow(private_bounds)]
pub fn format<'a, I: IntoSource<'a>>(source: I) -> Result<Vec<u8>, String> {
    format_source(source.into())
}

#[allow(private_bounds)]
trait IntoSource<'a> {
    type Target: crate::source::Source<'a>;
    fn into(self) -> Self::Target;
}

impl<'a> IntoSource<'a> for &'a str {
    type Target = Block<'a>;
    fn into(self) -> Self::Target {
        Block::new(self)
    }
}

impl<'a> IntoSource<'a> for &'a String {
    type Target = Block<'a>;
    fn into(self) -> Self::Target {
        Block::new(self.as_ref())
    }
}

impl<'a> IntoSource<'a> for &'a [&'a str] {
    type Target = Lines<'a>;
    fn into(self) -> Self::Target {
        Lines::new(self)
    }
}

macro_rules! throw_err {
    ($c:expr) => {
        $c.map_err(|e| e.to_string())?
    };
}

fn format_source<'a, S: crate::source::Source<'a>>(s: S) -> Result<Vec<u8>, String> {
    let mut err: String = "".to_string();
    let source_size = s.size();
    let mut parser = Parser::new(s, |_, _, lit| err = lit.to_string());
    let writer = &mut SimpleWriter(Vec::with_capacity(source_size + 512));
    let ast = throw_err!(parser.parse());

    let comments = &parser.comments;
    let ctx = &mut Context {
        comments: &comments,
        ..Context::new()
    };

    ctx.last_end_pos = match (comments.first(), ast.first()) {
        (Some(c), Some(a)) => {
            let (cp, ap) = (c.start(), a.start());
            if cp.before(&ap) { cp } else { ap }
        }
        (cg, a) => cg
            .map(|c| c.start())
            .or(a.map(|a| a.start()))
            .unwrap_or(ctx.last_end_pos),
    };

    if let [first, ..] = &ast[..] {
        throw_err!(first.format(writer, ctx));

        let is_decl = !matches!(first, AST::Stmt(..)) && !matches!(first, AST::Semi(..));
        let mut prev = first;
        for a in &ast[1..] {
            if is_decl && (discriminant(prev) != discriminant(a) || a.doc().is_some()) {
                let newlines = if let Some(i) = a.doc() {
                    ctx.comments[i as usize].start().lineno
                } else {
                    a.start().lineno
                } - prev.end().lineno;
                if newlines < 2 {
                    throw_err!(ctx.write_formfeed_newlines(
                        writer,
                        2 - newlines,
                        matches!(a, AST::FuncDecl(..)) && a.start().lineno != a.end().lineno,
                    ));
                }
            }
            throw_err!(a.format(writer, ctx));
            prev = a;
        }
    }
    throw_err!(ctx.format_left_comment(writer));

    let formatter_writer = SimpleWriter(Vec::with_capacity(writer.0.len()));
    let mut tw = TabWriter::new(formatter_writer)
        .padchar(' ')
        .padding(1)
        .tabwidth(8)
        .ignore_empty_column()
        .tab_indent(true);
    throw_err!(tw.write(writer.0.as_slice()));
    Ok(tw.w.0)
}

#[cfg(test)]
mod test {
    use std::fs;
    use std::path::PathBuf;

    use pretty_assertions::assert_eq;

    use crate::ast::{BasicLit, Pos};
    use crate::formatter::{SingleLineNodeSizeWriter, format};

    use super::Context;

    #[test]
    fn size_of_node() {
        let mut ctx = Context::new();

        assert_eq!(
            1,
            ctx.singleline_node_size(&BasicLit {
                kind: crate::tokens::LiteralKind::Int,
                value: "1",
                pos: Pos::zero()
            })
        );

        assert_eq!(
            SingleLineNodeSizeWriter::MAX_SIZE,
            ctx.singleline_node_size(&BasicLit {
                kind: crate::tokens::LiteralKind::String,
                value: r#"

                    "#,
                pos: Pos::zero()
            })
        );
    }

    fn read_test_file_data(base_path: &PathBuf, test_name: &str) -> Vec<(String, String)> {
        let input_path = base_path.join(format!("{}.input", test_name));
        let golden_path = base_path.join(format!("{}.golden", test_name));

        let input_content = fs::read_to_string(&input_path)
            .expect(&format!("Failed to read input file: {:?}", input_path));
        let golden_content = fs::read_to_string(&golden_path)
            .expect(&format!("Failed to read golden file: {:?}", golden_path));

        let separator = "==================";

        // Split by separator if present, otherwise treat as single test
        if input_content.contains(separator) {
            let inputs: Vec<&str> = input_content.split(separator).collect();
            let goldens: Vec<&str> = golden_content.split(separator).collect();

            assert_eq!(
                inputs.len(),
                goldens.len(),
                "Mismatched number of test cases in {} (inputs: {}, goldens: {})",
                test_name,
                inputs.len(),
                goldens.len()
            );

            inputs
                .into_iter()
                .zip(goldens.into_iter())
                .map(|(input, golden)| {
                    // Remove leading newline if present from the separator
                    let input_trimmed = input.trim_matches('\n');
                    let golden_trimmed = golden.trim_matches('\n');
                    (input_trimmed.to_string(), golden_trimmed.to_string())
                })
                .collect()
        } else {
            vec![(
                input_content.trim_matches('\n').to_string(),
                golden_content.trim_matches('\n').to_string(),
            )]
        }
    }

    fn test_from_files(dir: &str) {
        let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(dir);
        let test_names = fs::read_dir(&test_dir)
            .unwrap()
            .map(|f| f.unwrap().path())
            .filter(|f| f.is_file() && f.extension().unwrap() == "input")
            .map(|p| {
                let s = p.strip_prefix(&test_dir).unwrap().to_str().unwrap();
                s[..s.len() - ".input".len()].to_string()
            })
            .filter(|n| !n.starts_with("comments") && n != "doc" && !n.starts_with("gobuild"))
            .collect::<Vec<_>>();

        for test_name in test_names {
            println!("Running test: {}", test_name);

            let test_cases = read_test_file_data(&test_dir, &test_name);

            for (index, (input, expected)) in test_cases.iter().enumerate() {
                let result = format(input.as_str())
                    .map_err(|e| format!("Test {}, case {}: {}", test_name, index + 1, e))
                    .unwrap();
                let result_str = unsafe { str::from_utf8_unchecked(&result) };

                assert_eq!(
                    expected.as_str(),
                    result_str,
                    "Test {} failed at case {}",
                    test_name,
                    index + 1
                );
            }
            println!("Running test: {} OK", test_name);
        }
    }

    #[test]
    fn gofmt() {
        test_from_files("tests_data");
    }
}
