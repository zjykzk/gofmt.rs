use std::char;
use std::string::ToString;

use crate::source::Source;
use crate::tokens::{self, LiteralKind, Operator, Precedence, Token};

use unic_ucd_category::GeneralCategory;

#[derive(Debug)]
pub(crate) struct Scanner<'a, T: FnMut(usize, usize, &str), S: Source<'a>> {
    pub lineno: usize,
    pub col: usize,
    source: S,
    nlsemi: bool,
    ch: char,
    col_byte: usize,
    pub lit: &'a str,
    pub tok: Token,
    pub op: Operator,
    pub prec: Precedence,
    errh: T,
    bad: bool,
    pub kind: LiteralKind,
    keyword_map: [Token; 1 << 6],
}

impl<'a, T: FnMut(usize, usize, &str), S: Source<'a>> Scanner<'a, T, S> {
    pub(crate) fn new(source: S, errh: T) -> Self {
        let mut r = Self {
            lineno: 0,
            col: 0,
            source,
            errh,
            nlsemi: false,
            ch: ' ',
            col_byte: Default::default(),
            lit: "",
            tok: Token::EOF,
            op: Operator::None,
            prec: Precedence::None,
            bad: Default::default(),
            kind: LiteralKind::Int,
            keyword_map: [Token::EOF; 1 << 6],
        };

        r.init();

        r
    }

    // hash is a perfect hash function for keywords.
    // It assumes that s has at least length 2.
    fn hash(&self, s: &[u8]) -> usize {
        (((s[0] as usize) << 4 ^ (s[1] as usize)) + s.len()) & (self.keyword_map.len() - 1)
    }

    fn init(&mut self) {
        // populate KEYWORD_MAP
        for tok_discr in Token::Break as usize..=Token::Var as usize {
            let tok = Token::from_repr(tok_discr).unwrap();
            let h = self.hash(tok.to_string().as_bytes());
            if self.keyword_map[h] != Token::EOF {
                panic!(
                    "imperfect hash of tok:{},{},{}",
                    tok.to_string(),
                    self.keyword_map[h].to_string(),
                    h
                );
            }
            self.keyword_map[h] = tok;
        }
    }

    #[inline]
    fn nextch(&mut self) {
        self.ch = self.source.nextch();
    }

    fn skip_space(&mut self) {
        // println!("skip_space:{}", self.nlsemi);
        while self.ch == ' '
            || self.ch == '\t'
            || self.ch == '\r'
            || (self.ch == '\n' && !self.nlsemi)
        {
            // println!("skip_space char [{:?}]", self.ch);
            self.nextch()
        }
    }

    #[inline]
    pub(crate) fn segment(&self) -> &'a str {
        return self.source.segment();
    }

    pub(crate) fn next(&mut self) {
        self.skip_space();

        let nlsemi = self.nlsemi;
        self.nlsemi = false;

        self.source.start();
        (self.lineno, self.col) = self.source.pos();

        if ('a' <= self.ch && self.ch <= 'z')
            || ('A' <= self.ch && self.ch <= 'Z')
            || self.ch == '_'
            || is_unicode_letter(self.ch)
        {
            self.nextch();
            self.ident();
            return;
        }

        match self.ch {
            '\0' => {
                self.tok = if nlsemi {
                    self.lit = "EOF";
                    Token::Semi
                } else {
                    Token::EOF
                }
            }

            '\n' => {
                self.lit = "newline";
                self.punct(Token::Semi);
            }

            '0'..='9' => self.number(false),
            '"' => self.std_string(),
            '`' => self.raw_string(),
            '\'' => self.rune(),
            '(' => self.punct(Token::Lparen),
            '[' => self.punct(Token::Lbrack),
            '{' => self.punct(Token::Lbrace),
            ',' => self.punct(Token::Comma),
            ';' => {
                self.lit = "semicolon";
                self.punct(Token::Semi);
            }
            ')' => self.punct_marking_nlsemi(Token::Rparen),
            ']' => self.punct_marking_nlsemi(Token::Rbrack),
            '}' => self.punct_marking_nlsemi(Token::Rbrace),
            ':' => {
                self.nextch();
                self.tok = if self.ch == '=' {
                    self.nextch();
                    self.lit = self.source.segment();
                    Token::Define
                } else {
                    Token::Colon
                }
            }
            '.' => self.tok = self.dot(),
            '+' => {
                self.nextch();
                (self.op, self.prec) = (Operator::Add, Precedence::Add);
                if self.ch != '+' {
                    self.assignop();
                    return;
                }
                self.op_marking_nlsemi(Token::IncOp);
            }
            '-' => {
                self.nextch();
                (self.op, self.prec) = (Operator::Sub, Precedence::Add);
                if self.ch != '-' {
                    self.assignop();
                    return;
                }
                self.op_marking_nlsemi(Token::IncOp);
            }
            '*' => {
                self.nextch();
                (self.op, self.prec) = (Operator::Mul, Precedence::Mul);
                self.tok = if self.ch == '=' {
                    self.nextch();
                    self.lit = self.source.segment();
                    Token::AssignOp
                } else {
                    Token::Star
                }
            }
            '/' => {
                self.nextch();
                if self.ch == '/' {
                    self.line_comment();
                    if nlsemi {
                        self.nlsemi = nlsemi;
                    }
                } else if self.ch == '*' {
                    self.full_comment();
                    let (source_lineno, _) = self.source.pos();
                    // source_lineno now is the lineno of the new token if any
                    //
                    // case 1: full comment is equal 2
                    // case 2: full comment is greater than 2
                    if self.lineno + 1 < source_lineno && nlsemi {
                        // A multi-line comment acts like a newline;
                        // it translates to a ';' if nlsemi is set.
                        self.nlsemi = false
                    } else {
                        self.nlsemi = nlsemi
                    }
                } else {
                    (self.op, self.prec) = (Operator::Div, Precedence::Mul);
                    self.assignop();
                }
            }
            '%' => {
                self.nextch();
                (self.op, self.prec) = (Operator::Rem, Precedence::Mul);
                self.assignop();
            }
            '&' => {
                self.nextch();
                if self.ch == '&' {
                    self.nextch();
                    (self.op, self.prec, self.tok) =
                        (Operator::AndAnd, Precedence::AndAnd, Token::Operator);
                } else {
                    self.op = if self.ch == '^' {
                        self.nextch();
                        Operator::AndNot
                    } else {
                        Operator::And
                    };
                    self.prec = Precedence::Mul;
                    self.assignop();
                }
            }
            '|' => {
                self.nextch();
                if self.ch == '|' {
                    self.nextch();
                    (self.op, self.prec, self.tok) =
                        (Operator::OrOr, Precedence::OrOr, Token::Operator);
                } else {
                    (self.op, self.prec) = (Operator::Or, Precedence::Add);
                    self.assignop();
                }
            }
            '^' => {
                self.nextch();
                (self.op, self.prec) = (Operator::Xor, Precedence::Add);
                self.assignop();
            }
            '<' => {
                self.nextch();
                match self.ch {
                    '=' => {
                        self.nextch();
                        (self.op, self.prec, self.tok) =
                            (Operator::Leq, Precedence::Cmp, Token::Operator);
                    }
                    '<' => {
                        self.nextch();
                        (self.op, self.prec) = (Operator::Shl, Precedence::Mul);
                        self.assignop();
                    }
                    '-' => {
                        self.nextch();
                        self.tok = Token::Arrow;
                    }
                    _ => {
                        (self.op, self.prec, self.tok) =
                            (Operator::Lss, Precedence::Cmp, Token::Operator)
                    }
                }
            }
            '>' => {
                self.nextch();
                match self.ch {
                    '=' => {
                        self.nextch();
                        (self.op, self.prec, self.tok) =
                            (Operator::Geq, Precedence::Cmp, Token::Operator);
                    }
                    '>' => {
                        self.nextch();
                        (self.op, self.prec) = (Operator::Shr, Precedence::Mul);
                        self.assignop();
                    }
                    _ => {
                        (self.op, self.prec, self.tok) =
                            (Operator::Gtr, Precedence::Cmp, Token::Operator)
                    }
                }
            }
            '=' => {
                self.nextch();
                self.tok = if self.ch == '=' {
                    self.nextch();
                    (self.op, self.prec) = (Operator::Eql, Precedence::Cmp);
                    Token::Operator
                } else {
                    self.lit = self.source.segment();
                    Token::Assign
                }
            }
            '!' => {
                self.nextch();
                (self.op, self.prec, self.tok) = if self.ch == '=' {
                    self.nextch();
                    (Operator::Neq, Precedence::Cmp, Token::Operator)
                } else {
                    (Operator::Not, Precedence::None, Token::Operator)
                }
            }
            '~' => {
                self.nextch();
                (self.op, self.prec, self.tok) =
                    (Operator::Tilde, Precedence::None, Token::Operator);
            }
            _ => {
                self.errorf(format!("invalid character U+{:X}", self.ch as u32));
                self.next();
            }
        }
    }

    #[inline]
    fn line_comment(&mut self) {
        self.nextch();
        while self.ch != '\n' && self.ch != '\0' {
            self.nextch();
        }
        self.tok = Token::LineComment;
        self.lit = self.source.segment();
    }

    fn full_comment(&mut self) {
        self.nextch();
        while self.ch != '\0' {
            while self.ch == '*' {
                self.nextch();
                if self.ch == '/' {
                    self.nextch();
                    self.tok = Token::FullComment;
                    self.lit = self.source.segment();
                    return;
                }
            }
            self.nextch();
        }
        self.error_atf(0, "comment not terminated")
    }

    #[inline]
    fn assignop(&mut self) {
        self.tok = if self.ch == '=' {
            self.nextch();
            self.lit = self.source.segment();
            Token::AssignOp
        } else {
            Token::Operator
        }
    }

    #[inline]
    fn op_marking_nlsemi(&mut self, tok: Token) {
        self.punct_marking_nlsemi(tok)
    }

    #[inline]
    fn punct_marking_nlsemi(&mut self, tok: Token) {
        self.nextch();
        self.lit = self.source.segment();
        self.nlsemi = true;
        self.tok = tok;
    }

    #[inline]
    fn punct(&mut self, tok: Token) {
        self.nextch();
        self.tok = tok;
    }

    fn dot(&mut self) -> Token {
        self.nextch();

        if is_decimal(self.ch) {
            self.number(true);
            return Token::Literal;
        }

        if self.ch == '.' {
            self.nextch();
            if self.ch == '.' {
                self.nextch();
                return Token::DotDotDot;
            }
            self.source.rewind(); // now s.ch holds 1st '.'
            self.nextch(); // consume 1st '.' again
        }
        Token::Dot
    }

    fn ident(&mut self) {
        while ('a' <= self.ch && self.ch <= 'z')
            || ('A' <= self.ch && self.ch <= 'Z')
            || self.ch == '_'
            || ('0' <= self.ch && self.ch <= '9')
            || is_unicode_letter(self.ch)
            || is_unicode_digit(self.ch)
        {
            self.nextch()
        }

        self.lit = self.source.segment();
        if self.lit.len() >= 2 {
            let tok = self.keyword_map[self.hash(self.lit.as_bytes())];
            if tok != Token::EOF && tok.to_string() == self.lit {
                self.nlsemi = tokens::contains(
                    1 << Token::Break as usize
                        | 1 << Token::Continue as usize
                        | 1 << Token::Fallthrough as usize
                        | 1 << Token::Return as usize,
                    tok,
                );
                self.tok = tok;
                return;
            }
        }
        self.nlsemi = true;
        self.tok = Token::Name;
    }

    fn number(&mut self, seen_point: bool) {
        let mut invalid = -1;
        let mut ok = true;
        let mut base: u8 = 10;
        let mut prefix = ' ';
        let mut digsep = 0;
        let mut kind = LiteralKind::Int;
        let mut seen_point = seen_point;

        // integer part
        if !seen_point {
            if self.ch == '0' {
                self.nextch();
                (digsep, base, prefix) = self.base_and_prefix();
            }
            digsep |= self.digit(base, &mut invalid);
            if self.ch == '.' {
                if prefix == 'b' || prefix == 'o' {
                    self.errorf(format!(
                        "invalid radix point in {} literal",
                        base_name(base)
                    ));

                    ok = false;
                }
                self.nextch();
                seen_point = true;
            }
        }

        // float part
        if seen_point {
            kind = LiteralKind::Float;
            digsep |= self.digit(base, &mut invalid);
        }

        if digsep & 1 == 0 && ok {
            self.errorf(format!("{} literal has no digits", base_name(base)));
            ok = false;
        }

        // exponent
        let e = lower(self.ch);
        if e == 'e' || e == 'p' {
            if ok {
                if e == 'e' && prefix != '0' && prefix != ' ' {
                    self.errorf(format!("'{}' exponent requires decimal mantissa", self.ch));
                    ok = false;
                } else if e == 'p' && prefix != 'x' {
                    self.errorf(format!(
                        "'{}' exponent requires hexadecimal mantissa",
                        self.ch,
                    ));
                    ok = false;
                }
            }
            self.nextch();
            if self.ch == '+' || self.ch == '-' {
                self.nextch();
            }
            kind = LiteralKind::Float;
            digsep = self.digit(10, &mut invalid) | digsep & 2;
            if digsep & 1 == 0 && ok {
                self.errorf("exponent has no digits");
                ok = false;
            }
        } else if prefix == 'x' && kind == LiteralKind::Float && ok {
            self.errorf("hexadecimal mantissa requires a 'p' exponent");
            ok = false;
        }

        // suffix 'i'
        if self.ch == 'i' {
            self.nextch();
            kind = LiteralKind::Imag;
        }

        self.set_lit(kind, ok); // do this now so we can use s.lit below

        if kind == LiteralKind::Int && invalid >= 0 && ok {
            self.error_atf(
                invalid as usize,
                format!(
                    "invalid digit '{}' in {} literal",
                    char_at(self.lit, invalid as usize),
                    base_name(base),
                ),
            );

            ok = false;
        }

        if digsep & 2 != 0 && ok {
            if let Some(i) = invalid_sep(self.lit) {
                self.error_atf(i, "'_' must separate successive digits");
                ok = false;
            }
        }

        self.bad = !ok;
    }

    fn errorf<R: AsRef<str>>(&mut self, msg: R) {
        let (line, col) = self.source.pos();
        (self.errh)(line, col, msg.as_ref());
    }

    fn error_atf<R: AsRef<str>>(&mut self, offset: usize, msg: R) {
        let (line, col) = self.source.pos();
        (self.errh)(line, col + offset, msg.as_ref());
    }

    fn base_and_prefix(&mut self) -> (i32, u8, char) {
        match lower(self.ch) {
            'b' => {
                self.nextch();
                (0, 2, 'b')
            }
            'o' => {
                self.nextch();
                (0, 8, 'o')
            }
            'x' => {
                self.nextch();
                (0, 16, 'x')
            }
            _ => (1, 8, '0'),
        }
    }

    // digits accepts the sequence { digit | '_' }.
    // If base <= 10, digits accepts any decimal digit but records
    // the index (relative to the literal start) of a digit >= base
    // in *invalid, if *invalid < 0.
    // digits returns a bitset describing whether the sequence contained
    // digits (bit 0 is set), or separators '_' (bit 1 is set).
    fn digit(&mut self, base: u8, invalid: &mut i32) -> i32 {
        let mut digsep = 0;
        if base <= 10 {
            let max = ('0' as u8 + base) as char;
            while is_decimal(self.ch) || self.ch == '_' {
                digsep |= if self.ch == '_' { 2 } else { 1 };
                if self.ch != '_' && self.ch >= max && *invalid < 0 {
                    let (_, col_byte) = self.source.pos_byte();
                    *invalid = (col_byte - self.col_byte) as i32;
                }
                self.nextch();
            }
        } else {
            while is_hex(self.ch) || self.ch == '_' {
                digsep |= if self.ch == '_' { 2 } else { 1 };
                self.nextch();
            }
        }
        digsep
    }

    fn rune(&mut self) {
        self.nextch();

        let (mut ok, mut n) = (true, 0);
        ok = loop {
            match self.ch {
                '\'' => {
                    ok = ok
                        && if n == 0 {
                            self.errorf("empty rune literal or unescaped '");
                            false
                        } else if n != 1 {
                            self.error_atf(0, "more than one character in rune literal");
                            false
                        } else {
                            true
                        };
                    self.nextch();
                    break ok;
                }
                '\\' => {
                    self.nextch();
                    if !self.escape('\'') {
                        ok = false;
                    }
                    n += 1;
                    continue;
                }
                '\n' => {
                    if ok {
                        self.errorf("newline in rune literal");
                    }
                    break false;
                }
                '\0' => {
                    if ok {
                        self.error_atf(0, "rune literal not terminated");
                    }
                    break false;
                }
                _ => n += 1,
            }
            self.nextch();
        };
        self.set_lit(LiteralKind::Rune, ok);
    }

    fn std_string(&mut self) {
        let mut ok = true;
        self.nextch();
        ok = loop {
            match self.ch {
                '"' => {
                    self.nextch();
                    break ok;
                }
                '\\' => {
                    self.nextch();
                    if !self.escape('"') {
                        ok = false;
                    }
                    continue;
                }
                '\n' => {
                    self.errorf("newline in string");
                    break false;
                }
                '\0' => {
                    self.error_atf(0, "string not terminated");
                    break false;
                }
                _ => (),
            }
            self.nextch();
        };

        self.set_lit(LiteralKind::String, ok);
    }

    fn raw_string(&mut self) {
        let ok = loop {
            self.nextch();
            match self.ch {
                '`' => {
                    self.nextch();
                    break true;
                }
                '\0' => {
                    self.error_atf(0, "string not terminated");
                    break false;
                }
                _ => (),
            }
        };

        self.set_lit(LiteralKind::String, ok)
    }

    fn set_lit(&mut self, kind: LiteralKind, ok: bool) {
        self.nlsemi = true;
        self.tok = Token::Literal;
        self.lit = self.source.segment();
        self.bad = !ok;
        self.kind = kind;
    }

    fn escape(&mut self, quote: char) -> bool {
        if self.ch == quote {
            self.nextch();
            return true;
        }

        let (n, base, max): (i32, u32, u32);

        match self.ch {
            'a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\\' => {
                self.nextch();
                return true;
            }
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' => {
                (n, base, max) = (3, 8, 255);
            }
            'x' => {
                self.nextch();
                (n, base, max) = (2, 16, 255);
            }
            'u' => {
                self.nextch();
                (n, base, max) = (4, 16, '\u{10FFFF}' as u32);
            }
            'U' => {
                self.nextch();
                (n, base, max) = (8, 16, '\u{10FFFF}' as u32);
            }
            _ => {
                return if self.ch == '\0' {
                    true
                } else {
                    self.errorf("unknown escape");
                    false
                };
            }
        }

        let mut x: u32 = 0;
        for _ in 0..n {
            if self.ch == '\0' {
                return true;
            }

            let d = if is_decimal(self.ch) {
                self.ch as u32 - '0' as u32
            } else if is_hex_alpha(self.ch) {
                lower(self.ch) as u32 - 'a' as u32 + 10
            } else {
                base
            };
            if d >= base {
                self.errorf(format!(
                    "invalid character '{}' in {} escape",
                    self.ch,
                    base_name(base as u8)
                ));
                return false;
            }

            x = base * x + d;
            self.nextch();
        }

        if x > max && base == 8 {
            self.errorf(format!("octal escape value {} > 255", x));
            return false;
        }

        /* surrogate range */
        if x > max || 0xD800 <= x && x < 0xE000 {
            self.errorf(format!("escape is invalid Unicode code point U+{:X?}", x));
            return false;
        }

        true
    }
}

// invalid_sep returns the index of the first invalid separator in x, or -1.
fn invalid_sep(x: &str) -> Option<usize> {
    let mut x1 = ' '; // prefix char, we only care if it's 'x'
    let mut d = '.'; // digit, one of '_', '0' (a digit), or '.' (anything else)
    let mut i = 0;

    if x.len() >= 2 && char_at(x, 0) == '0' {
        x1 = lower(char_at(x, 1));
        if x1 == 'x' || x1 == 'o' || x1 == 'b' {
            d = '0';
            i = 2;
        }
    }

    // mantissa and exponent
    for (j, c) in x[i..].char_indices() {
        let p = d;
        d = c;

        if d == '_' {
            if p != '0' {
                return Some(j + i);
            }
            continue;
        }

        if is_decimal(d) || x1 == 'x' && is_hex(d) {
            d = '0';
            continue;
        }

        if p == '_' {
            return Some(j + i - 1);
        }

        d = '.';
    }

    if d == '_' { Some(x.len() - 1) } else { None }
}

fn char_at(x: &str, offset: usize) -> char {
    x[offset..].chars().next().unwrap()
}

fn lower(c: char) -> char {
    return (('a' as u8 - 'A' as u8) | c as u8) as char;
}

fn is_hex(c: char) -> bool {
    is_decimal(c) || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
}

fn is_hex_alpha(c: char) -> bool {
    ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
}

fn is_decimal(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_unicode_letter(c: char) -> bool {
    GeneralCategory::of(c).is_letter()
}

fn is_unicode_digit(c: char) -> bool {
    matches!(GeneralCategory::of(c), GeneralCategory::DecimalNumber)
}

fn base_name(base: u8) -> &'static str {
    match base {
        2 => "binary",
        8 => "octal",
        10 => "decimal",
        16 => "hexadecimal",
        _ => panic!("invalid base"),
    }
}

#[cfg(test)]
mod test {
    use std::cell::RefCell;

    use crate::{
        source::{Block, Source},
        tokens::{LiteralKind, Token},
    };

    use super::Scanner;

    #[test]
    fn smoke() {
        let src = "if (+foo\t+=..123/***/0.9_0e-0i'a'`raw`\"string\"..f;//$";
        let tokens = [
            Token::If,
            Token::Lparen,
            Token::Operator,
            Token::Name,
            Token::AssignOp,
            Token::Dot,
            Token::Literal,
            Token::FullComment,
            Token::Literal,
            Token::Literal,
            Token::Literal,
            Token::Literal,
            Token::Dot,
            Token::Dot,
            Token::Name,
            Token::Semi,
            Token::LineComment,
            Token::EOF,
        ];

        let mut got = Scanner::new(Block::new(src), |line, col, msg| {
            panic!("{}:{} {}", line, col, msg)
        });
        for want in tokens {
            got.next();
            let (line, col) = got.source.pos();
            assert_eq!(
                got.tok, want,
                "{}:{}: got {}; want {}",
                line, col, got.tok, want
            );
        }
    }

    macro_rules! init_sample_token {
        ($tok:expr, $src:literal) => {
            SampleToken {
                token: $tok,
                src: $src,
            }
        };
    }

    macro_rules! init_sample_token_detail {
        ($tok:expr, $src:literal, $op:expr, $prec:expr) => {
            SampleToken {
                token: $tok,
                src: $src,
            }
        };
    }

    struct SampleToken {
        token: Token,
        src: &'static str,
    }

    const SAMPLE_TOKENS: [SampleToken; 135] = [
        // name samples
        init_sample_token!(Token::Name, "foo"),
        init_sample_token!(Token::Name, "x"),
        init_sample_token!(Token::Name, "X123"),
        init_sample_token!(Token::Name, "foo"),
        init_sample_token!(Token::Name, "Foo123"),
        init_sample_token!(Token::Name, "foo_bar"),
        init_sample_token!(Token::Name, "_"),
        init_sample_token!(Token::Name, "_foobar"),
        init_sample_token!(Token::Name, "a۰۱۸"),
        init_sample_token!(Token::Name, "foo६४"),
        init_sample_token!(Token::Name, "bar９８７６"),
        init_sample_token!(Token::Name, "ŝ"),
        init_sample_token!(Token::Name, "ŝfoo"),
        // literal samples
        init_sample_token!(Token::Literal, "0"),
        init_sample_token!(Token::Literal, "1"),
        init_sample_token!(Token::Literal, "12345"),
        init_sample_token!(Token::Literal, "123456789012345678890123456789012345678890"),
        init_sample_token!(Token::Literal, "01234567"),
        init_sample_token!(Token::Literal, "0_1_234_567"),
        init_sample_token!(Token::Literal, "0X0"),
        init_sample_token!(Token::Literal, "0xcafebabe"),
        init_sample_token!(Token::Literal, "0x_cafe_babe"),
        init_sample_token!(Token::Literal, "0O0"),
        init_sample_token!(Token::Literal, "0o000"),
        init_sample_token!(Token::Literal, "0o_000"),
        init_sample_token!(Token::Literal, "0B1"),
        init_sample_token!(Token::Literal, "0b01100110"),
        init_sample_token!(Token::Literal, "0b_0110_0110"),
        init_sample_token!(Token::Literal, "0."),
        init_sample_token!(Token::Literal, "0.e0"),
        init_sample_token!(Token::Literal, "0.e-1"),
        init_sample_token!(Token::Literal, "0.e+123"),
        init_sample_token!(Token::Literal, ".0"),
        init_sample_token!(Token::Literal, ".0E00"),
        init_sample_token!(Token::Literal, ".0E-0123"),
        init_sample_token!(Token::Literal, ".0E+12345678901234567890"),
        init_sample_token!(Token::Literal, ".45e1"),
        init_sample_token!(Token::Literal, "3.14159265"),
        init_sample_token!(Token::Literal, "1e0"),
        init_sample_token!(Token::Literal, "1e+100"),
        init_sample_token!(Token::Literal, "1e-100"),
        init_sample_token!(Token::Literal, "2.71828e-1000"),
        init_sample_token!(Token::Literal, "0i"),
        init_sample_token!(Token::Literal, "1i"),
        init_sample_token!(Token::Literal, "012345678901234567889i"),
        init_sample_token!(Token::Literal, "123456789012345678890i"),
        init_sample_token!(Token::Literal, "0.i"),
        init_sample_token!(Token::Literal, ".0i"),
        init_sample_token!(Token::Literal, "3.14159265i"),
        init_sample_token!(Token::Literal, "1e0i"),
        init_sample_token!(Token::Literal, "1e+100i"),
        init_sample_token!(Token::Literal, "1e-100i"),
        init_sample_token!(Token::Literal, "2.71828e-1000i"),
        init_sample_token!(Token::Literal, "'a'"),
        init_sample_token!(Token::Literal, "'\\\\'"),
        init_sample_token!(Token::Literal, "'\\000'"),
        init_sample_token!(Token::Literal, "'\\xFF'"),
        init_sample_token!(Token::Literal, "'\\uff16'"),
        init_sample_token!(Token::Literal, "'\\U0000ff16'"),
        init_sample_token!(Token::Literal, "`foobar`"),
        init_sample_token!(Token::Literal, "`foo\tbar`"),
        init_sample_token!(Token::Literal, "`\r`"),
        // operators
        init_sample_token_detail!(Token::Operator, "!", Operator::Not, Precedence::None),
        init_sample_token_detail!(Token::Operator, "~", Operator::Tilde, Precedence::None),
        init_sample_token_detail!(Token::Operator, "||", Operator::OrOr, Precedence::OrOr),
        init_sample_token_detail!(Token::Operator, "&&", Operator::AndAnd, Precedence::AndAnd),
        init_sample_token_detail!(Token::Operator, "==", Operator::Eql, Precedence::Cmp),
        init_sample_token_detail!(Token::Operator, "!=", Operator::Neq, Precedence::Cmp),
        init_sample_token_detail!(Token::Operator, "<", Operator::Lss, Precedence::Cmp),
        init_sample_token_detail!(Token::Operator, "<=", Operator::Leq, Precedence::Cmp),
        init_sample_token_detail!(Token::Operator, ">", Operator::Gtr, Precedence::Cmp),
        init_sample_token_detail!(Token::Operator, ">=", Operator::Geq, Precedence::Cmp),
        init_sample_token_detail!(Token::Operator, "+", Operator::Add, Precedence::Add),
        init_sample_token_detail!(Token::Operator, "-", Operator::Sub, Precedence::Add),
        init_sample_token_detail!(Token::Operator, "|", Operator::Or, Precedence::Add),
        init_sample_token_detail!(Token::Operator, "^", Operator::Xor, Precedence::Add),
        init_sample_token_detail!(Token::Star, "*", Operator::Mul, Precedence::Mul),
        init_sample_token_detail!(Token::Operator, "/", Operator::Div, Precedence::Mul),
        init_sample_token_detail!(Token::Operator, "%", Operator::Rem, Precedence::Mul),
        init_sample_token_detail!(Token::Operator, "&", Operator::And, Precedence::Mul),
        init_sample_token_detail!(Token::Operator, "&^", Operator::AndNot, Precedence::Mul),
        init_sample_token_detail!(Token::Operator, "<<", Operator::Shl, Precedence::Mul),
        init_sample_token_detail!(Token::Operator, ">>", Operator::Shr, Precedence::Mul),
        // assignment operations
        init_sample_token_detail!(Token::AssignOp, "+=", Operator::Add, Precedence::Add),
        init_sample_token_detail!(Token::AssignOp, "-=", Operator::Sub, Precedence::Add),
        init_sample_token_detail!(Token::AssignOp, "|=", Operator::Or, Precedence::Add),
        init_sample_token_detail!(Token::AssignOp, "^=", Operator::Xor, Precedence::Add),
        init_sample_token_detail!(Token::AssignOp, "*=", Operator::Mul, Precedence::Mul),
        init_sample_token_detail!(Token::AssignOp, "/=", Operator::Div, Precedence::Mul),
        init_sample_token_detail!(Token::AssignOp, "%=", Operator::Rem, Precedence::Mul),
        init_sample_token_detail!(Token::AssignOp, "&=", Operator::And, Precedence::Mul),
        init_sample_token_detail!(Token::AssignOp, "&^=", Operator::AndNot, Precedence::Mul),
        init_sample_token_detail!(Token::AssignOp, "<<=", Operator::Shl, Precedence::Mul),
        init_sample_token_detail!(Token::AssignOp, ">>=", Operator::Shr, Precedence::Mul),
        // other operations
        init_sample_token_detail!(Token::IncOp, "++", Operator::Add, Precedence::Add),
        init_sample_token_detail!(Token::IncOp, "--", Operator::Sub, Precedence::Add),
        init_sample_token!(Token::Assign, "="),
        init_sample_token!(Token::Define, ":="),
        init_sample_token!(Token::Arrow, "<-"),
        // delimiters
        init_sample_token!(Token::Lparen, "("),
        init_sample_token!(Token::Lbrack, "["),
        init_sample_token!(Token::Lbrace, "{"),
        init_sample_token!(Token::Rparen, ")"),
        init_sample_token!(Token::Rbrack, "]"),
        init_sample_token!(Token::Rbrace, "}"),
        init_sample_token!(Token::Comma, ","),
        init_sample_token!(Token::Semi, ";"),
        init_sample_token!(Token::Colon, ":"),
        init_sample_token!(Token::Dot, "."),
        init_sample_token!(Token::DotDotDot, "..."),
        // keywords
        init_sample_token!(Token::Break, "break"),
        init_sample_token!(Token::Case, "case"),
        init_sample_token!(Token::Chan, "chan"),
        init_sample_token!(Token::Const, "const"),
        init_sample_token!(Token::Continue, "continue"),
        init_sample_token!(Token::Default, "default"),
        init_sample_token!(Token::Defer, "defer"),
        init_sample_token!(Token::Else, "else"),
        init_sample_token!(Token::Fallthrough, "fallthrough"),
        init_sample_token!(Token::For, "for"),
        init_sample_token!(Token::Func, "func"),
        init_sample_token!(Token::Go, "go"),
        init_sample_token!(Token::Goto, "goto"),
        init_sample_token!(Token::If, "if"),
        init_sample_token!(Token::Import, "import"),
        init_sample_token!(Token::Interface, "interface"),
        init_sample_token!(Token::Map, "map"),
        init_sample_token!(Token::Package, "package"),
        init_sample_token!(Token::Range, "range"),
        init_sample_token!(Token::Return, "return"),
        init_sample_token!(Token::Select, "select"),
        init_sample_token!(Token::Struct, "struct"),
        init_sample_token!(Token::Switch, "switch"),
        init_sample_token!(Token::Type, "type"),
        init_sample_token!(Token::Var, "var"),
    ];

    #[test]
    fn token() {
        for st in &SAMPLE_TOKENS {
            let mut s = Scanner::new(Block::new(&st.src), |line, col, msg| {
                println!("{}:{}:{}:{}", st.src, line, col, msg);
            });
            s.next();
            assert_eq!(s.tok, st.token, "bad token, src:{}", st.src);
            if s.tok == Token::Literal || s.tok == Token::Name {
                assert_eq!(s.lit, st.src, "bad lit, src: {}", st.src);
            }
            assert!(!s.bad);
        }
    }

    #[derive(Debug)]
    struct TestNum<'a> {
        kind: LiteralKind,
        src: &'a str,
        tokens: &'a str,
        err: &'a str,
    }

    macro_rules! init_test_num {
        ($kind:expr, $src:literal, $tokens:literal, $err:literal) => {
            TestNum {
                kind: $kind,
                src: $src,
                tokens: $tokens,
                err: $err,
            }
        };
    }

    #[test]
    fn number() {
        for t in vec![
            // binaries
            init_test_num!(LiteralKind::Int, "0b0", "0b0", ""),
            init_test_num!(LiteralKind::Int, "0b1010", "0b1010", ""),
            init_test_num!(LiteralKind::Int, "0B1110", "0B1110", ""),
            init_test_num!(LiteralKind::Int, "0b", "0b", "binary literal has no digits"),
            init_test_num!(
                LiteralKind::Int,
                "0b0190",
                "0b0190",
                "invalid digit '9' in binary literal"
            ),
            init_test_num!(LiteralKind::Int, "0b01a0", "0b01 a0", ""), // only accept 0-9
            init_test_num!(
                LiteralKind::Float,
                "0b.",
                "0b.",
                "invalid radix point in binary literal"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0b.1",
                "0b.1",
                "invalid radix point in binary literal"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0b1.0",
                "0b1.0",
                "invalid radix point in binary literal"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0b1e10",
                "0b1e10",
                "'e' exponent requires decimal mantissa"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0b1P-1",
                "0b1P-1",
                "'P' exponent requires hexadecimal mantissa"
            ),
            init_test_num!(LiteralKind::Imag, "0b10i", "0b10i", ""),
            init_test_num!(
                LiteralKind::Imag,
                "0b10.0i",
                "0b10.0i",
                "invalid radix point in binary literal"
            ),
            // octals
            init_test_num!(LiteralKind::Int, "0o0", "0o0", ""),
            init_test_num!(LiteralKind::Int, "0o1234", "0o1234", ""),
            init_test_num!(LiteralKind::Int, "0O1234", "0O1234", ""),
            init_test_num!(LiteralKind::Int, "0o", "0o", "octal literal has no digits"),
            init_test_num!(
                LiteralKind::Int,
                "0o8123",
                "0o8123",
                "invalid digit '8' in octal literal"
            ),
            init_test_num!(
                LiteralKind::Int,
                "0o1293",
                "0o1293",
                "invalid digit '9' in octal literal"
            ),
            init_test_num!(LiteralKind::Int, "0o12a3", "0o12 a3", ""), // only accept 0-9
            init_test_num!(
                LiteralKind::Float,
                "0o.",
                "0o.",
                "invalid radix point in octal literal"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0o.2",
                "0o.2",
                "invalid radix point in octal literal"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0o1.2",
                "0o1.2",
                "invalid radix point in octal literal"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0o1E+2",
                "0o1E+2",
                "'E' exponent requires decimal mantissa"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0o1p10",
                "0o1p10",
                "'p' exponent requires hexadecimal mantissa"
            ),
            init_test_num!(LiteralKind::Imag, "0o10i", "0o10i", ""),
            init_test_num!(
                LiteralKind::Imag,
                "0o10e0i",
                "0o10e0i",
                "'e' exponent requires decimal mantissa"
            ),
            // 0-octals
            init_test_num!(LiteralKind::Int, "0", "0", ""),
            init_test_num!(LiteralKind::Int, "0123", "0123", ""),
            init_test_num!(
                LiteralKind::Int,
                "08123",
                "08123",
                "invalid digit '8' in octal literal"
            ),
            init_test_num!(
                LiteralKind::Int,
                "01293",
                "01293",
                "invalid digit '9' in octal literal"
            ),
            init_test_num!(LiteralKind::Int, "0F.", "0 F .", ""), // only accept 0-9
            init_test_num!(LiteralKind::Int, "0123F.", "0123 F .", ""),
            init_test_num!(LiteralKind::Int, "0123456x", "0123456 x", ""),
            // decimals
            init_test_num!(LiteralKind::Int, "1", "1", ""),
            init_test_num!(LiteralKind::Int, "1234", "1234", ""),
            init_test_num!(LiteralKind::Int, "1f", "1 f", ""), // only accept 0-9
            init_test_num!(LiteralKind::Imag, "0i", "0i", ""),
            init_test_num!(LiteralKind::Imag, "0678i", "0678i", ""),
            // decimal floats
            init_test_num!(LiteralKind::Float, "0.", "0.", ""),
            init_test_num!(LiteralKind::Float, "123.", "123.", ""),
            init_test_num!(LiteralKind::Float, "0123.", "0123.", ""),
            init_test_num!(LiteralKind::Float, ".0", ".0", ""),
            init_test_num!(LiteralKind::Float, ".123", ".123", ""),
            init_test_num!(LiteralKind::Float, ".0123", ".0123", ""),
            init_test_num!(LiteralKind::Float, "0.0", "0.0", ""),
            init_test_num!(LiteralKind::Float, "123.123", "123.123", ""),
            init_test_num!(LiteralKind::Float, "0123.0123", "0123.0123", ""),
            init_test_num!(LiteralKind::Float, "0e0", "0e0", ""),
            init_test_num!(LiteralKind::Float, "123e+0", "123e+0", ""),
            init_test_num!(LiteralKind::Float, "0123E-1", "0123E-1", ""),
            init_test_num!(LiteralKind::Float, "0.e+1", "0.e+1", ""),
            init_test_num!(LiteralKind::Float, "123.E-10", "123.E-10", ""),
            init_test_num!(LiteralKind::Float, "0123.e123", "0123.e123", ""),
            init_test_num!(LiteralKind::Float, ".0e-1", ".0e-1", ""),
            init_test_num!(LiteralKind::Float, ".123E+10", ".123E+10", ""),
            init_test_num!(LiteralKind::Float, ".0123E123", ".0123E123", ""),
            init_test_num!(LiteralKind::Float, "0.0e1", "0.0e1", ""),
            init_test_num!(LiteralKind::Float, "123.123E-10", "123.123E-10", ""),
            init_test_num!(LiteralKind::Float, "0123.0123e+456", "0123.0123e+456", ""),
            init_test_num!(LiteralKind::Float, "0e", "0e", "exponent has no digits"),
            init_test_num!(LiteralKind::Float, "0E+", "0E+", "exponent has no digits"),
            init_test_num!(
                LiteralKind::Float,
                "1e+f",
                "1e+ f",
                "exponent has no digits"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0p0",
                "0p0",
                "'p' exponent requires hexadecimal mantissa"
            ),
            init_test_num!(
                LiteralKind::Float,
                "1.0P-1",
                "1.0P-1",
                "'P' exponent requires hexadecimal mantissa"
            ),
            init_test_num!(LiteralKind::Imag, "0.i", "0.i", ""),
            init_test_num!(LiteralKind::Imag, ".123i", ".123i", ""),
            init_test_num!(LiteralKind::Imag, "123.123i", "123.123i", ""),
            init_test_num!(LiteralKind::Imag, "123e+0i", "123e+0i", ""),
            init_test_num!(LiteralKind::Imag, "123.E-10i", "123.E-10i", ""),
            init_test_num!(LiteralKind::Imag, ".123E+10i", ".123E+10i", ""),
            // hexadecimals
            init_test_num!(LiteralKind::Int, "0x0", "0x0", ""),
            init_test_num!(LiteralKind::Int, "0x1234", "0x1234", ""),
            init_test_num!(LiteralKind::Int, "0xcafef00d", "0xcafef00d", ""),
            init_test_num!(LiteralKind::Int, "0XCAFEF00D", "0XCAFEF00D", ""),
            init_test_num!(
                LiteralKind::Int,
                "0x",
                "0x",
                "hexadecimal literal has no digits"
            ),
            init_test_num!(LiteralKind::Int, "0x1g", "0x1 g", ""),
            init_test_num!(LiteralKind::Imag, "0xf00i", "0xf00i", ""),
            // hexadecimal floats
            init_test_num!(LiteralKind::Float, "0x0p0", "0x0p0", ""),
            init_test_num!(LiteralKind::Float, "0x12efp-123", "0x12efp-123", ""),
            init_test_num!(LiteralKind::Float, "0xABCD.p+0", "0xABCD.p+0", ""),
            init_test_num!(LiteralKind::Float, "0x.0189P-0", "0x.0189P-0", ""),
            init_test_num!(LiteralKind::Float, "0x1.ffffp+1023", "0x1.ffffp+1023", ""),
            init_test_num!(
                LiteralKind::Float,
                "0x.",
                "0x.",
                "hexadecimal literal has no digits"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0x0.",
                "0x0.",
                "hexadecimal mantissa requires a 'p' exponent"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0x.0",
                "0x.0",
                "hexadecimal mantissa requires a 'p' exponent"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0x1.1",
                "0x1.1",
                "hexadecimal mantissa requires a 'p' exponent"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0x1.1e0",
                "0x1.1e0",
                "hexadecimal mantissa requires a 'p' exponent"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0x1.2gp1a",
                "0x1.2 gp1a",
                "hexadecimal mantissa requires a 'p' exponent"
            ),
            init_test_num!(LiteralKind::Float, "0x0p", "0x0p", "exponent has no digits"),
            init_test_num!(
                LiteralKind::Float,
                "0xeP-",
                "0xeP-",
                "exponent has no digits"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0x1234PAB",
                "0x1234P AB",
                "exponent has no digits"
            ),
            init_test_num!(LiteralKind::Float, "0x1.2p1a", "0x1.2p1 a", ""),
            init_test_num!(LiteralKind::Imag, "0xf00.bap+12i", "0xf00.bap+12i", ""),
            // separators
            init_test_num!(LiteralKind::Int, "0b_1000_0001", "0b_1000_0001", ""),
            init_test_num!(LiteralKind::Int, "0o_600", "0o_600", ""),
            init_test_num!(LiteralKind::Int, "0_466", "0_466", ""),
            init_test_num!(LiteralKind::Int, "1_000", "1_000", ""),
            init_test_num!(LiteralKind::Float, "1_000.000_1", "1_000.000_1", ""),
            init_test_num!(LiteralKind::Imag, "10e+1_2_3i", "10e+1_2_3i", ""),
            init_test_num!(LiteralKind::Int, "0x_f00d", "0x_f00d", ""),
            init_test_num!(LiteralKind::Float, "0x_f00d.0p1_2", "0x_f00d.0p1_2", ""),
            init_test_num!(
                LiteralKind::Int,
                "0b__1000",
                "0b__1000",
                "'_' must separate successive digits"
            ),
            init_test_num!(
                LiteralKind::Int,
                "0o60___0",
                "0o60___0",
                "'_' must separate successive digits"
            ),
            init_test_num!(
                LiteralKind::Int,
                "0466_",
                "0466_",
                "'_' must separate successive digits"
            ),
            init_test_num!(
                LiteralKind::Float,
                "1_.",
                "1_.",
                "'_' must separate successive digits"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0._1",
                "0._1",
                "'_' must separate successive digits"
            ),
            init_test_num!(
                LiteralKind::Float,
                "2.7_e0",
                "2.7_e0",
                "'_' must separate successive digits"
            ),
            init_test_num!(
                LiteralKind::Imag,
                "10e+12_i",
                "10e+12_i",
                "'_' must separate successive digits"
            ),
            init_test_num!(
                LiteralKind::Int,
                "0x___0",
                "0x___0",
                "'_' must separate successive digits"
            ),
            init_test_num!(
                LiteralKind::Float,
                "0x1.0_p0",
                "0x1.0_p0",
                "'_' must separate successive digits"
            ),
        ] {
            let err = RefCell::new("".to_string());
            let mut s = Scanner::new(Block::new(t.src), |_, _, msg| {
                if *err.borrow() == "" {
                    *err.borrow_mut() = String::from(msg);
                }
            });

            for (i, want) in t.tokens.split(' ').enumerate() {
                s.errorf("");
                s.next();

                if *err.borrow() != "" && !s.bad {
                    assert!(false, "{}: got error but bad not set", t.src);
                }

                let lit = match s.tok {
                    Token::Name | Token::Literal => s.lit,
                    Token::Dot => ".",
                    _ => "",
                };

                if i == 0 {
                    if s.tok != Token::Literal || s.kind != t.kind {
                        assert!(
                            false,
                            "{}: got token {} (kind = {}); want literal (kind = {})",
                            t.src, s.tok, s.kind, t.kind
                        );
                    }

                    if *err.borrow() != t.err {
                        assert!(
                            false,
                            "{}: got error {}; want {}",
                            t.src,
                            err.borrow(),
                            t.err
                        );
                    }
                }
                if lit != want {
                    assert!(
                        false,
                        "{}: got literal {} ({}); want {}",
                        t.src, lit, s.tok, want
                    );
                }
            }

            s.next();
            if s.tok == Token::Semi {
                s.next();
            }
            if s.tok != Token::EOF {
                assert!(false, "{}: got {}; want EOF", t.src, s.tok);
            }
        }
    }

    #[test]
    fn multi_line_str_lit() {
        let err = RefCell::new("".to_string());
        let mut s = Scanner::new(Block::new("`\n1\n2\n`"), |_, _, msg| {
            if *err.borrow() == "" {
                *err.borrow_mut() = String::from(msg);
            }
        });
        s.next();
        assert_eq!(Token::Literal, s.tok);
        assert_eq!("`\n1\n2\n`", s.lit);
    }
}
