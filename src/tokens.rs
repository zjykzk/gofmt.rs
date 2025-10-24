//! Define all golang syntax token
use std::fmt::Debug;
use strum::{Display, EnumCount, EnumString, FromRepr, IntoStaticStr};

#[derive(
    Copy,
    Clone,
    Eq,
    PartialEq,
    Debug,
    EnumString,
    EnumCount,
    IntoStaticStr,
    Display,
    FromRepr,
    Hash,
    PartialOrd,
    Ord,
)]
pub(crate) enum Token {
    #[strum(serialize = "EOF")]
    EOF,
    #[strum(serialize = "name")]
    Name,
    #[strum(serialize = "literal")]
    Literal,
    #[strum(serialize = "op")]
    Operator,
    #[strum(serialize = "op=")]
    AssignOp,
    #[strum(serialize = "opop")]
    IncOp,
    #[strum(serialize = "=")]
    Assign,
    #[strum(serialize = ":=")]
    Define,
    #[strum(serialize = "<-")]
    Arrow,
    #[strum(serialize = "*")]
    Star,
    #[strum(serialize = "(")]
    Lparen,
    #[strum(serialize = "[")]
    Lbrack,
    #[strum(serialize = "{")]
    Lbrace,
    #[strum(serialize = ")")]
    Rparen,
    #[strum(serialize = "]")]
    Rbrack,
    #[strum(serialize = "}")]
    Rbrace,
    #[strum(serialize = ",")]
    Comma,
    #[strum(serialize = ";")]
    Semi,
    #[strum(serialize = ":")]
    Colon,
    #[strum(serialize = ".")]
    Dot,
    #[strum(serialize = "...")]
    DotDotDot,
    #[strum(serialize = "break")]
    Break,
    #[strum(serialize = "case")]
    Case,
    #[strum(serialize = "chan")]
    Chan,
    #[strum(serialize = "const")]
    Const,
    #[strum(serialize = "continue")]
    Continue,
    #[strum(serialize = "default")]
    Default,
    #[strum(serialize = "defer")]
    Defer,
    #[strum(serialize = "else")]
    Else,
    #[strum(serialize = "fallthrough")]
    Fallthrough,
    #[strum(serialize = "for")]
    For,
    #[strum(serialize = "func")]
    Func,
    #[strum(serialize = "go")]
    Go,
    #[strum(serialize = "goto")]
    Goto,
    #[strum(serialize = "if")]
    If,
    #[strum(serialize = "import")]
    Import,
    #[strum(serialize = "interface")]
    Interface,
    #[strum(serialize = "map")]
    Map,
    #[strum(serialize = "package")]
    Package,
    #[strum(serialize = "range")]
    Range,
    #[strum(serialize = "return")]
    Return,
    #[strum(serialize = "select")]
    Select,
    #[strum(serialize = "struct")]
    Struct,
    #[strum(serialize = "switch")]
    Switch,
    #[strum(serialize = "type")]
    Type,
    #[strum(serialize = "var")]
    Var,
    #[strum(serialize = "line_comment")]
    LineComment,
    #[strum(serialize = "full_comment")]
    FullComment,
}

const _: usize = 1 << Token::COUNT;

// contains reports whether tok is in tokset.
pub(crate) fn contains(tokset: usize, tok: Token) -> bool {
    return tokset & (1 << tok as usize) != 0;
}

impl Default for Token {
    fn default() -> Self {
        Token::EOF
    }
}

#[derive(
    Copy,
    Clone,
    PartialOrd,
    Ord,
    Eq,
    PartialEq,
    Debug,
    EnumString,
    EnumCount,
    IntoStaticStr,
    Display,
)]
pub(crate) enum Operator {
    None,
    // Def is the : in :=
    #[strum(serialize = ":")]
    Def,
    #[strum(serialize = "!")]
    Not,
    #[strum(serialize = "<-")]
    Recv,
    #[strum(serialize = "~")]
    Tilde,

    // precOrOr
    #[strum(serialize = "||")]
    OrOr,

    // precAndAnd
    #[strum(serialize = "&&")]
    AndAnd,

    // precCmp
    #[strum(serialize = "==")]
    Eql, // ==
    #[strum(serialize = "!=")]
    Neq, // !=
    #[strum(serialize = "<")]
    Lss, // <
    #[strum(serialize = "<=")]
    Leq, // <=
    #[strum(serialize = ">")]
    Gtr, // >
    #[strum(serialize = ">=")]
    Geq,

    // precAdd
    #[strum(serialize = "+")]
    Add, // +
    #[strum(serialize = "-")]
    Sub, // -
    #[strum(serialize = "|")]
    Or, // |
    #[strum(serialize = "^")]
    Xor,

    // precMul
    #[strum(serialize = "*")]
    Mul,
    #[strum(serialize = "/")]
    Div,
    #[strum(serialize = "%")]
    Rem,
    #[strum(serialize = "&")]
    And,
    #[strum(serialize = "&^")]
    AndNot,
    #[strum(serialize = "<<")]
    Shl,
    #[strum(serialize = ">>")]
    Shr,
}

impl Operator {
    pub(crate) fn precedence(&self) -> Precedence {
        match self {
            Operator::OrOr => Precedence::OrOr,
            Operator::AndAnd => Precedence::AndAnd,
            Operator::Eql
            | Operator::Neq
            | Operator::Lss
            | Operator::Leq
            | Operator::Gtr
            | Operator::Geq => Precedence::Cmp,
            Operator::Add | Operator::Sub | Operator::Or | Operator::Xor => Precedence::Add,
            Operator::Mul
            | Operator::Div
            | Operator::Rem
            | Operator::And
            | Operator::AndNot
            | Operator::Shl
            | Operator::Shr => Precedence::Mul,
            _ => Precedence::None,
        }
    }
}

#[derive(
    Copy, Clone, Eq, PartialOrd, PartialEq, Debug, EnumString, EnumCount, IntoStaticStr, Display,
)]
pub(crate) enum Precedence {
    None = 0,
    OrOr = 1,
    AndAnd = 2,
    Cmp = 3,
    Add = 4,
    Mul = 5,
    // for formatter
    Unary = 6,
    Highest = 7,
}

impl Precedence {
    pub(crate) fn next(self) -> Self {
        match self {
            Self::None => Self::OrOr,
            Self::OrOr => Self::OrOr,
            Self::AndAnd => Self::OrOr,
            Self::Cmp => Self::Add,
            Self::Add => Self::Mul,
            Self::Mul => Self::Unary,
            _ => Self::Highest,
        }
    }
}

#[derive(
    Copy,
    Clone,
    Eq,
    PartialEq,
    Debug,
    EnumString,
    EnumCount,
    IntoStaticStr,
    Display,
    PartialOrd,
    Ord,
    Hash,
)]
pub(crate) enum LiteralKind {
    #[strum(serialize = "int")]
    Int,
    #[strum(serialize = "float")]
    Float,
    #[strum(serialize = "imag")]
    Imag,
    #[strum(serialize = "rune")]
    Rune,
    #[strum(serialize = "string")]
    String,
}
