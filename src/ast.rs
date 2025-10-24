use std::fmt::Display;

use crate::tokens::{LiteralKind, Operator, Token};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum AST<'a> {
    PackageClause(Box<PackageClause<'a>>),
    ImportDecl(Box<ImportDecl<'a>>),
    TypeDecl(Box<TypeDecl<'a>>),
    FuncDecl(Box<FuncDecl<'a>>),
    ConstDecl(Box<VarDecl<'a>>),
    VarDecl(Box<VarDecl<'a>>),
    MethodDecl(Box<MethodDecl<'a>>),
    Stmt(Stmt<'a>),
    Semi(Pos),
}

impl<'a> AST<'a> {
    pub(crate) fn doc(&self) -> Option<CommentIndex> {
        match self {
            Self::PackageClause(_) => None,
            Self::ImportDecl(_) => None,
            Self::TypeDecl(t) => match &t.decl {
                TypeDeclOption::Group(g) => g.doc,
                TypeDeclOption::Spec(s) => match s {
                    TypeSpec::AliasDecl(_) => None,
                    TypeSpec::TypeDef(d) => d.doc,
                },
            },
            Self::FuncDecl(f) => f.doc,
            Self::ConstDecl(_) => None,
            Self::VarDecl(v) => match &v.decl {
                VarDeclOption::Group(_) => None,
                VarDeclOption::Spec(s) => s.doc,
            },
            Self::MethodDecl(m) => m.doc,
            Self::Stmt(_) => None,
            Self::Semi(_) => None,
        }
    }
    pub(crate) fn start(&self) -> Pos {
        match self {
            Self::PackageClause(p) => p.pos,
            Self::ImportDecl(i) => i.pos,
            Self::TypeDecl(t) => t.pos,
            Self::FuncDecl(f) => f.func,
            Self::ConstDecl(c) => c.pos,
            Self::VarDecl(v) => v.start(),
            Self::MethodDecl(m) => m.func,
            Self::Stmt(s) => s.start(),
            Self::Semi(s) => *s,
        }
    }

    pub(crate) fn end(&self) -> Pos {
        match self {
            Self::PackageClause(p) => p.name.end(),
            Self::ImportDecl(i) => i.import.end(),
            Self::TypeDecl(t) => t.decl.end(),
            Self::FuncDecl(f) => f.end(),
            Self::ConstDecl(c) => c.end(),
            Self::VarDecl(v) => v.end(),
            Self::MethodDecl(m) => {
                if let Some(b) = &m.body {
                    b.end()
                } else {
                    m.sign.end()
                }
            }
            Self::Stmt(s) => s.end(),
            Self::Semi(s) => *s,
        }
    }
}

type LineNo = u32;
type Col = u32;

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Pos {
    pub col: Col,
    pub lineno: LineNo,
}

pub(crate) trait Range {
    fn start(&self) -> Pos;
    fn end(&self) -> Pos;
}

impl Range for Pos {
    fn start(&self) -> Pos {
        *self
    }

    fn end(&self) -> Pos {
        *self
    }
}

impl Pos {
    pub(crate) fn zero() -> Pos {
        Pos { col: 0, lineno: 0 }
    }

    pub(crate) fn is_zero(&self) -> bool {
        self.col == 0 && self.lineno == 0
    }

    pub(crate) fn new(lineno: usize, col: usize) -> Pos {
        Pos {
            col: col as u32,
            lineno: lineno as u32,
        }
    }

    pub(crate) fn before(&self, p: &Pos) -> bool {
        self.lineno < p.lineno || (self.lineno == p.lineno && self.col < p.col)
    }
}

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Comment<'a> {
    pub pos: Pos,
    pub content: &'a str,
    pub tok: Token,
}

impl<'a> Comment<'a> {
    fn start(&self) -> Pos {
        self.pos
    }

    fn end(&self) -> Pos {
        if let Some(i) = self.content.rfind('\n') {
            Pos {
                lineno: self.pos.lineno
                    + self
                        .content
                        .as_bytes()
                        .iter()
                        .map(|b| if *b == b'\n' { 1 } else { 0 })
                        .sum::<u32>(),
                col: i as u32 + 1,
            }
        } else {
            Pos {
                lineno: self.pos.lineno,
                col: self.pos.col + self.content.len() as u32,
            }
        }
    }
}

impl<'a> Display for Comment<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "pos:{:?}, content:{}, tok:{}",
            self.pos, self.content, self.tok
        ))
    }
}

#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct CommentGroup<'a> {
    pub comments: Vec<Comment<'a>>,
}

impl<'a> ToOwned for CommentGroup<'a> {
    type Owned = CommentGroup<'a>;
    fn to_owned(&self) -> Self::Owned {
        CommentGroup {
            comments: self.comments.clone(),
        }
    }
}

impl<'a> Display for CommentGroup<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("comments:{:?}", self.comments))
    }
}

impl<'a> CommentGroup<'a> {
    pub(crate) fn start(&self) -> Pos {
        self.comments[0].start()
    }

    pub(crate) fn end(&self) -> Pos {
        self.comments[self.comments.len() - 1].end()
    }
}

/// PackageClause  = "package" PackageName .
/// PackageName    = identifier .
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct PackageClause<'a> {
    pub name: Identifier<'a>,
    pub pos: Pos,
}

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Identifier<'a> {
    pub name: &'a str,
    pub pos: Pos,
}

impl<'a> Identifier<'a> {
    #[inline(always)]
    pub(crate) fn start(&self) -> Pos {
        self.pos
    }

    #[inline(always)]
    pub(crate) fn end(&self) -> Pos {
        Pos {
            col: self.pos.col + self.name.len() as u32,
            lineno: self.pos.lineno,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct BasicLit<'a> {
    pub kind: LiteralKind,
    pub value: &'a str,
    pub pos: Pos,
}

impl<'a> Range for BasicLit<'a> {
    #[inline(always)]
    fn start(&self) -> Pos {
        self.pos
    }

    #[inline(always)]
    fn end(&self) -> Pos {
        let col = if let Some(n) = self.value.rfind('\n') {
            (self.value.len() - n) as u32 - 1
        } else {
            self.pos.col + self.value.len() as u32 - 1
        };
        Pos {
            col,
            // col: self.pos.col + (self.value.find('\n').unwrap_or(self.value.len()) - 1) as u32,
            lineno: self.pos.lineno + self.value.lines().count() as u32 - 1,
        }
    }
}

/// ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
/// ImportSpec       = [ "." | PackageName ] ImportPath .
/// ImportPath       = string_lit .
#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct ImportDecl<'a> {
    pub import: ImportOption<'a>,
    pub pos: Pos,
}

impl<'a> ImportDecl<'a> {}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum ImportOption<'a> {
    Group(ImportGroup<'a>),
    Spec(ImportSpec<'a>),
}

impl<'a> ImportOption<'a> {
    pub(crate) fn end(&self) -> Pos {
        match self {
            Self::Group(g) => g.specs[g.specs.len() - 1].end(),
            Self::Spec(s) => s.end(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct ImportGroup<'a> {
    pub lparen: Pos,
    pub specs: Vec<ImportSpec<'a>>,
    pub rparen: Pos,
}

impl<'a> ImportGroup<'a> {
    pub(crate) fn start(&self) -> Pos {
        self.lparen
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct ImportSpec<'a> {
    pub pkg_name: Option<Identifier<'a>>,
    pub path: BasicLit<'a>,
    pub doc: Option<CommentIndex>,
    pub line_comment: Option<CommentIndex>,
}

impl<'a> ImportSpec<'a> {
    pub(crate) fn start(&self) -> Pos {
        if let Some(p) = self.pkg_name {
            p.start()
        } else {
            self.path.start()
        }
    }

    pub(crate) fn end(&self) -> Pos {
        self.path.end()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct BlockStmt<'a> {
    pub lbrace: Pos,
    pub stmts: Vec<Stmt<'a>>,
    pub rbrace: Pos,
}

impl<'a> Range for BlockStmt<'a> {
    #[inline(always)]
    fn start(&self) -> Pos {
        self.lbrace
    }

    #[inline(always)]
    fn end(&self) -> Pos {
        self.rbrace
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct VarDecl<'a> {
    pub pos: Pos,
    pub decl: VarDeclOption<'a>,
}

impl<'a> Range for VarDecl<'a> {
    fn start(&self) -> Pos {
        self.pos
    }

    fn end(&self) -> Pos {
        match &self.decl {
            VarDeclOption::Spec(s) => s.end(),
            VarDeclOption::Group(g) => g.end(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum VarDeclOption<'a> {
    Group(VarGroup<'a>),
    Spec(VarSpec<'a>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct VarGroup<'a> {
    pub lparen: Pos,
    pub rparen: Pos,
    pub specs: Vec<(VarSpec<'a>, Pos /*semi lit*/)>,
}

impl<'a> Range for VarGroup<'a> {
    fn start(&self) -> Pos {
        self.lparen
    }

    fn end(&self) -> Pos {
        self.rparen
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct VarSpec<'a> {
    pub ident_list: IdentifierList<'a>,
    pub typ: Option<Expression<'a>>,
    pub eq: Option<Pos>,
    pub expr_list: Option<Expression<'a>>,
    pub doc: Option<CommentIndex>,
    pub line_comment: Option<CommentIndex>,
}

impl<'a> Range for VarSpec<'a> {
    fn start(&self) -> Pos {
        self.ident_list.start()
    }

    fn end(&self) -> Pos {
        if let Some(el) = &self.expr_list {
            return el.end();
        }

        if let Some(t) = &self.typ {
            return t.end();
        }

        self.ident_list.end()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TypeDecl<'a> {
    pub decl: TypeDeclOption<'a>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum TypeDeclOption<'a> {
    Group(TypeGroup<'a>),
    Spec(TypeSpec<'a>),
}

impl<'a> TypeDeclOption<'a> {
    fn end(&self) -> Pos {
        match self {
            Self::Group(g) => g.rparen,
            Self::Spec(s) => s.end(),
        }
    }
}

pub(crate) type CommentIndex = i32;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TypeGroup<'a> {
    pub lparen: Pos,
    pub rparen: Pos,
    pub specs: Vec<(TypeSpec<'a>, Pos /*semi lit*/)>,
    pub doc: Option<CommentIndex>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum TypeSpec<'a> {
    AliasDecl(AliasDecl<'a>),
    TypeDef(TypeDef<'a>),
}

impl<'a> Range for TypeSpec<'a> {
    fn start(&self) -> Pos {
        match self {
            Self::TypeDef(t) => t.ident.start(),
            Self::AliasDecl(a) => a.ident.start(),
        }
    }

    fn end(&self) -> Pos {
        match self {
            Self::TypeDef(t) => t.ident.end(),
            Self::AliasDecl(a) => a.ident.end(),
        }
    }
}

// TypeDef = identifier [ TypeParameters ] Type
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TypeDef<'a> {
    pub ident: Identifier<'a>,
    pub params: Option<TypeParameters<'a>>,
    pub typ: Expression<'a>,
    pub doc: Option<CommentIndex>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct AliasDecl<'a> {
    pub ident: Identifier<'a>,
    pub params: Option<TypeParameters<'a>>,
    pub eq: Pos,
    pub typ: Expression<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TypeParameters<'a> {
    pub lbrack: Pos,
    pub param_list: Vec<(TypeParamDecl<'a>, Pos /*comma*/)>,
    pub rbrack: Pos,
}

impl<'a> Range for TypeParameters<'a> {
    fn start(&self) -> Pos {
        self.lbrack
    }
    fn end(&self) -> Pos {
        self.rbrack
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TypeParamDecl<'a> {
    pub idents: IdentifierList<'a>,
    pub type_constraint: TypeElem<'a>,
}

impl<'a> Range for TypeParamDecl<'a> {
    fn start(&self) -> Pos {
        self.idents.start()
    }

    fn end(&self) -> Pos {
        if let Some(c) = self.type_constraint.followers.last() {
            c.end()
        } else {
            self.type_constraint.term.end()
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct IdentifierList<'a> {
    pub ident: Identifier<'a>,
    pub followers: Vec<CommaAndIdentifier<'a>>,
}

impl<'a> Range for IdentifierList<'a> {
    fn start(&self) -> Pos {
        self.ident.start()
    }

    fn end(&self) -> Pos {
        if self.followers.is_empty() {
            self.ident.end()
        } else {
            self.followers[self.followers.len() - 1].ident.end()
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct CommaAndIdentifier<'a> {
    pub comma: Pos,
    pub ident: Identifier<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TypeElem<'a> {
    pub term: Expression<'a>,
    pub followers: Vec<OrTypeTerm<'a>>,
}

impl<'a> Range for TypeElem<'a> {
    fn start(&self) -> Pos {
        self.term.start()
    }

    fn end(&self) -> Pos {
        if self.followers.is_empty() {
            self.term.end()
        } else {
            self.followers[0].end()
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct OrTypeTerm<'a> {
    pub or: Pos,
    pub term: Expression<'a>,
}

impl<'a> Range for OrTypeTerm<'a> {
    #[inline(always)]
    fn start(&self) -> Pos {
        self.or
    }

    #[inline(always)]
    fn end(&self) -> Pos {
        self.term.end()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct UnderlyingType<'a> {
    pub pos: Pos,
    pub typ: Expression<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ChanType<'a> {
    pub ch: Pos,
    pub typ: Expression<'a>,
    pub dir: ChanDir,
}

impl<'a> ChanType<'a> {
    pub(crate) fn start(&self) -> Pos {
        match self.dir {
            ChanDir::Recv(_) => self.ch,
            ChanDir::Send(p) => p,
            _ => self.ch,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ChanDir {
    Recv(Pos),
    Send(Pos),
    Both,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct SliceType<'a> {
    pub lbrack: Pos,
    pub rbrack: Pos,
    pub typ: Expression<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ArrayType<'a> {
    pub lbrack: Pos,
    pub len: Expression<'a>,
    pub rbrack: Pos,
    pub typ: Expression<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct MapType<'a> {
    pub pos: Pos,
    pub lbrack: Pos,
    pub rbrack: Pos,
    pub key_type: Expression<'a>,
    pub ele_type: Expression<'a>,
}

impl<'a> MapType<'a> {
    pub(crate) fn start(&self) -> Pos {
        self.pos
    }
    pub(crate) fn end(&self) -> Pos {
        self.rbrack
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct QualifiedName<'a> {
    pub pkg: Identifier<'a>,
    pub dot: Pos,
    pub ident: Identifier<'a>,
}

// TypeName [ TypeArgs ]
// TypeArgs  = "[" TypeList [ "," ] "]" .
// TypeList  = Type { "," Type } .
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TypeInstance<'a> {
    pub lbrack: Pos,
    pub rbrack: Pos,
    pub type_args: Vec<(Expression<'a>, Pos /*comma pos, zero mean empty*/)>,
    pub type_name: Expression<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct StructType<'a> {
    pub lbrace: Pos,
    pub rbrace: Pos,
    pub field_decls: Vec<(FieldDeclOption<'a>, Pos /*semi*/)>,
    pub pos: Pos,
}

impl<'a> StructType<'a> {
    pub(crate) fn start(&self) -> Pos {
        self.lbrace
    }
}
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum FieldDeclOption<'a> {
    Fields(Fields<'a>),
    Embedded(EmbeddedFieldTypeField<'a>),
}

impl<'a> FieldDeclOption<'a> {
    pub(crate) fn start(&self) -> Pos {
        match self {
            FieldDeclOption::Fields(f) => f.idents.start(),
            FieldDeclOption::Embedded(e) => e.typ.start(),
        }
    }

    pub(crate) fn end(&self) -> Pos {
        match self {
            FieldDeclOption::Fields(f) => f.idents.end(),
            FieldDeclOption::Embedded(e) => e.typ.end(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Fields<'a> {
    pub doc: Option<CommentIndex>,
    pub line_comment: Option<CommentIndex>,
    pub idents: IdentifierList<'a>,
    pub typ: Expression<'a>,
    pub tag: Option<BasicLit<'a>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct EmbeddedFieldTypeField<'a> {
    pub doc: Option<CommentIndex>,
    pub line_comment: Option<CommentIndex>,
    pub typ: Expression<'a>,
    pub tag: Option<BasicLit<'a>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct MethodDecl<'a> {
    pub doc: Option<CommentIndex>,
    pub func: Pos,
    pub receiver: Params<'a>,
    pub name: Identifier<'a>,
    pub sign: FuncSign<'a>,
    pub body: Option<BlockStmt<'a>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct FuncDecl<'a> {
    pub doc: Option<CommentIndex>,
    pub func: Pos,
    pub name: Identifier<'a>,
    pub type_params: Option<TypeParameters<'a>>,
    pub sign: FuncSign<'a>,
    pub body: Option<BlockStmt<'a>>,
}

impl<'a> Range for FuncDecl<'a> {
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct FuncLit<'a> {
    pub func: Pos,
    pub sign: FuncSign<'a>,
    pub body: BlockStmt<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct FuncType<'a> {
    pub func: Pos,
    pub params: Params<'a>,
    pub ret: Option<FuncResult<'a>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct FuncSign<'a> {
    pub params: Params<'a>,
    pub ret: Option<FuncResult<'a>>,
}

impl<'a> Range for FuncSign<'a> {
    #[inline(always)]
    fn start(&self) -> Pos {
        self.params.start()
    }

    #[inline(always)]
    fn end(&self) -> Pos {
        if let Some(fr) = &self.ret {
            fr.start()
        } else {
            self.params.end()
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum FuncResult<'a> {
    Params(Params<'a>),
    Type(Expression<'a>),
}

impl<'a> Range for FuncResult<'a> {
    #[inline(always)]
    fn start(&self) -> Pos {
        match self {
            Self::Params(p) => p.start(),
            Self::Type(e) => e.start(),
        }
    }

    #[inline(always)]
    fn end(&self) -> Pos {
        match self {
            Self::Params(p) => p.end(),
            Self::Type(e) => e.end(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Params<'a> {
    pub lparen: Pos,
    pub list: Option<ParamList<'a>>,
    pub rparen: Pos,
}

impl<'a> Range for Params<'a> {
    #[inline(always)]
    fn start(&self) -> Pos {
        self.lparen
    }

    #[inline(always)]
    fn end(&self) -> Pos {
        self.rparen
    }
}

pub(crate) type ParamList<'a> = Vec<(ParamDecl<'a>, Pos)>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ParamDecl<'a> {
    pub idents: Option<IdentifierList<'a>>,
    pub dotdotdot: Option<Pos>,
    pub typ: Expression<'a>,
}

impl<'a> Range for ParamDecl<'a> {
    fn start(&self) -> Pos {
        if let Some(id) = &self.idents {
            return id.start();
        }

        if let Some(dot) = self.dotdotdot {
            return dot;
        }

        self.typ.start()
    }

    fn end(&self) -> Pos {
        self.typ.end()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct InterfaceType<'a> {
    pub pos: Pos,
    pub lbrace: Pos,
    pub elems: Vec<InterfaceElemAndSemi<'a>>,
    pub rbrace: Pos,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct InterfaceElemAndSemi<'a> {
    pub line_comment: Option<CommentIndex>,
    pub elem: InterfaceElem<'a>,
    pub semi: Pos, // zero if followed by '}'
}

impl<'a> Range for InterfaceElemAndSemi<'a> {
    fn start(&self) -> Pos {
        self.elem.start()
    }

    fn end(&self) -> Pos {
        self.elem.end()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum InterfaceElem<'a> {
    MethodElem(MethodElem<'a>),
    TypeElem(TypeElem<'a>),
}

impl<'a> Range for InterfaceElem<'a> {
    fn start(&self) -> Pos {
        match self {
            InterfaceElem::TypeElem(t) => t.start(),
            InterfaceElem::MethodElem(m) => m.start(),
        } //nolint:rustc
    }

    fn end(&self) -> Pos {
        match self {
            InterfaceElem::TypeElem(t) => t.end(),
            InterfaceElem::MethodElem(m) => m.end(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct MethodElem<'a> {
    pub name: Identifier<'a>,
    pub sign: FuncSign<'a>,
}

impl<'a> Range for MethodElem<'a> {
    fn start(&self) -> Pos {
        self.name.start()
    }

    fn end(&self) -> Pos {
        self.sign.end()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Stmt<'a> {
    ConstDecl(Box<VarDecl<'a>>),
    VarDecl(Box<VarDecl<'a>>),
    TypeDecl(Box<TypeDecl<'a>>),
    Block(Box<BlockStmt<'a>>),
    Label(Box<LabelStmt<'a>>),
    Expr(Expression<'a>),
    Send(Box<SendStmt<'a>>),
    IncDec(Box<IncDecStmt<'a>>),
    Assign(Box<AssignStmt<'a>>),
    Semi(Pos),
    Empty,
    Call(Box<CallStmt<'a>>),
    Return(Box<ReturnStmt<'a>>),
    Branch(Box<BranchStmt<'a>>),
    If(Box<IfStmt<'a>>),
    Switch(Box<SwitchStmt<'a>>),
    Select(Box<SelectStmt<'a>>),
    For(Box<ForStmt<'a>>),
}

impl<'a> Range for Stmt<'a> {
    fn start(&self) -> Pos {
        match self {
            Self::ConstDecl(s) => s.start(),
            Self::VarDecl(s) => s.start(),
            Self::TypeDecl(s) => s.pos,
            Self::Block(s) => s.lbrace,
            Self::Label(s) => s.label.start(),
            Self::Expr(s) => s.start(),
            Self::Send(s) => s.arrow,
            Self::IncDec(s) => s.expr.start(),
            Self::Assign(s) => s.expr.start(),
            Self::Semi(s) => *s,
            Self::Empty => Pos::zero(),
            Self::Call(c) => c.tok.pos,
            Self::Return(r) => r.pos,
            Self::Branch(b) => b.tok.pos,
            Self::If(r) => r.pos,
            Self::Switch(s) => s.pos,
            Self::Select(s) => s.pos,
            Self::For(s) => s.pos,
        }
    }

    fn end(&self) -> Pos {
        match self {
            Self::ConstDecl(s) => s.end(),
            Self::VarDecl(s) => s.end(),
            Self::TypeDecl(s) => s.pos,
            Self::Block(s) => s.rbrace,
            Self::Label(s) => s.label.end(),
            Self::Expr(s) => s.end(),
            Self::Send(s) => s.value.end(),
            Self::IncDec(s) => s.expr.end(),
            Self::Assign(s) => s.value.end(),
            Self::Semi(s) => *s,
            Self::Empty => Pos::zero(),
            Self::Call(c) => c.stmt.end(),
            Self::Return(r) => {
                if let Some(e) = &r.expr {
                    e.end()
                } else {
                    r.pos
                }
            }
            Self::Branch(b) => {
                if let Some(i) = b.label {
                    i.end()
                } else {
                    b.tok.pos
                }
            }
            Self::If(r) => {
                if let Some((_, e)) = &r.r#else {
                    e.end()
                } else {
                    r.block.end()
                }
            }
            Self::Switch(s) => s.rbrace,
            Self::Select(s) => s.rbrace,
            Self::For(s) => s.body.rbrace,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ForStmt<'a> {
    pub pos: Pos,
    pub opt: ForOption<'a>,
    pub body: BlockStmt<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ForOption<'a> {
    Empty,
    Cond(Expression<'a>),
    Range(RangeClause<'a>),
    ForClause(ForClause<'a>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct RangeClause<'a> {
    pub value: Option<(Expression<'a>, TokenPos)>, // [ ExpressionList "=" | IdentifierList ":=" ]
    pub range: Pos,
    pub expr: Expression<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ForClause<'a> {
    pub init: Stmt<'a>,
    pub semi0: Pos,
    pub cond: Option<Expression<'a>>,
    pub semi1: Pos,
    pub post: Stmt<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct SelectStmt<'a> {
    pub pos: Pos,
    pub lbrace: Pos,
    pub body: Vec<CommClause<'a>>,
    pub rbrace: Pos,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct CommClause<'a> {
    pub comm: (Stmt<'a>, TokenPos), // when .0 == Stmt::Empty means token `case`, .1 == `,`&`case`
    pub colon: Pos,
    pub body: Vec<Stmt<'a>>,
}

impl<'a> Range for CommClause<'a> {
    fn start(&self) -> Pos {
        self.comm.1.pos
    }
    fn end(&self) -> Pos {
        if self.body.is_empty() {
            self.colon
        } else {
            self.body[self.body.len() - 1].end()
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct SwitchStmt<'a> {
    pub pos: Pos,
    pub init: Option<(Stmt<'a>, Pos)>,
    pub tag: Option<Expression<'a>>,
    pub lbrace: Pos,
    pub body: Vec<CaseCause<'a>>,
    pub rbrace: Pos,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct CaseCause<'a> {
    pub tok_pos: TokenPos,
    pub expr: Option<Expression<'a>>,
    pub colon: Pos,
    pub body: Vec<Stmt<'a>>,
}

impl<'a> Range for CaseCause<'a> {
    fn start(&self) -> Pos {
        self.tok_pos.pos
    }

    fn end(&self) -> Pos {
        if self.body.is_empty() {
            self.colon
        } else {
            self.body[self.body.len() - 1].end()
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct IfStmt<'a> {
    pub pos: Pos,
    pub init: Option<(Stmt<'a>, Pos)>,
    pub cond: Expression<'a>,
    pub block: BlockStmt<'a>,
    pub r#else: Option<(Pos, Stmt<'a>)>, // else [IfStmt | Block]
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct BranchStmt<'a> {
    pub tok: TokenPos, // break or continue
    pub label: Option<Identifier<'a>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ReturnStmt<'a> {
    pub pos: Pos,
    pub expr: Option<Expression<'a>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct CallStmt<'a> {
    pub tok: TokenPos, // defer or go
    pub stmt: Expression<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct LabelStmt<'a> {
    pub label: Identifier<'a>,
    pub colon: Pos,
    pub stmt: Option<Stmt<'a>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct SendStmt<'a> {
    pub chan: Expression<'a>,
    pub arrow: Pos,
    pub value: Expression<'a>,
}

pub(crate) type OpLitPos<'a> = Identifier<'a>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct AssignStmt<'a> {
    pub expr: Expression<'a>,
    pub op: OpLitPos<'a>,
    pub value: Expression<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct IncDecStmt<'a> {
    pub expr: Expression<'a>,
    pub op: OpLitPos<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Expression<'a> {
    CompositeLit(Box<CompositeLit<'a>>),
    LiteralValue(Box<LiteralValue<'a>>),
    TypeInstance(Box<TypeInstance<'a>>),
    Struct(Box<StructType<'a>>),
    SliceType(Box<SliceType<'a>>),
    MapType(Box<MapType<'a>>),
    ChanType(Box<ChanType<'a>>),
    FuncType(Box<FuncType<'a>>),
    InterfaceType(Box<InterfaceType<'a>>),
    ArrayType(Box<ArrayType<'a>>),
    KeyedElement(Box<KeyedElement<'a>>),
    BasicLit(Box<BasicLit<'a>>),
    FuncLit(Box<FuncLit<'a>>),

    UnderlyingType(Box<UnderlyingType<'a>>),
    Index(Box<Index<'a>>),
    ListExpr(ListExpr<'a>),

    Operation(Box<Operation<'a>>),

    Token(Box<TokenPos>),
    Ident(Box<Identifier<'a>>),
    QualifiedName(Box<QualifiedName<'a>>),
    ParenedExpr(Box<ParenedExpr<'a>>),
    CallExpr(Box<CallExpr<'a>>),
    Selector(Box<Selector<'a>>),
    TypeAssert(Box<TypeAssert<'a>>),
    TypeSwitchGuard(Box<TypeSwitchGuard<'a>>),
    SliceExpr(Box<SliceExpr<'a>>),

    DotDotDotArgs(Box<DotDotDotArgs<'a>>),

    // util expr
    Expr(&'a Expression<'a>),
    ReplaceMe,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct DotDotDotArgs<'a> {
    pub expr: Expression<'a>,
    pub dotdotdot: Pos,
}

impl<'a> Range for Expression<'a> {
    fn start(&self) -> Pos {
        match self {
            Self::Ident(e) => e.start(),
            Self::Operation(op) => op.x.start(),
            Self::ParenedExpr(p) => p.lparen,
            Self::ListExpr(le) => le[0].0.start(),
            Self::BasicLit(b) => b.start(),
            Self::KeyedElement(k) => k.start(),
            Self::UnderlyingType(u) => u.pos,
            Self::TypeInstance(i) => i.type_name.start(),
            Self::CompositeLit(c) => c.start(),
            Self::MapType(m) => m.start(),
            Self::Token(t) => t.pos,
            Self::QualifiedName(q) => q.pkg.start(),
            Self::CallExpr(c) => c.pexpr.start(),
            Self::Selector(s) => s.pexpr.start(),
            Self::TypeAssert(t) => t.pexpr.start(),
            Self::TypeSwitchGuard(t) => {
                if let Some((lhs, _)) = &t.lhs {
                    lhs.start()
                } else {
                    t.x.start()
                }
            }
            Self::SliceExpr(s) => s.expr.start(),
            Self::FuncType(f) => f.func,
            Self::InterfaceType(i) => i.pos,
            Self::ArrayType(a) => a.lbrack,
            Self::FuncLit(f) => f.func,
            Self::LiteralValue(l) => l.lbrace,
            Self::Index(i) => i.expr.start(),
            Self::ChanType(c) => c.start(),
            Self::Struct(s) => s.start(),
            Self::SliceType(s) => s.lbrack,
            Self::DotDotDotArgs(a) => a.expr.start(),
            Self::Expr(e) => e.start(),
            _ => panic!("impossible start of {:?}", self),
        }
    }

    fn end(&self) -> Pos {
        match self {
            Self::Ident(e) => e.end(),
            Self::Operation(op) => {
                if let Some(y) = &op.y {
                    y.end()
                } else {
                    op.x.end()
                }
            }
            Self::ParenedExpr(p) => p.rparen,
            Self::ListExpr(le) => le[le.len() - 1].0.start(),
            Self::BasicLit(b) => b.end(),
            Self::KeyedElement(k) => k.elem.end(),
            Self::UnderlyingType(u) => u.typ.end(),
            Self::TypeInstance(i) => i.rbrack,
            Self::CompositeLit(c) => c.end(),
            Self::MapType(m) => m.end(),
            Self::SliceType(s) => s.typ.end(),
            Self::ChanType(c) => c.typ.end(),
            Self::FuncType(f) => {
                if let Some(r) = &f.ret {
                    r.end()
                } else {
                    f.params.rparen
                }
            }
            Self::InterfaceType(i) => i.rbrace,
            Self::ArrayType(a) => a.typ.end(),
            Self::FuncLit(f) => f.body.rbrace,
            Self::LiteralValue(l) => l.rbrace,
            Self::Index(i) => i.rbrack,
            Self::Struct(s) => s.rbrace,
            Self::QualifiedName(q) => q.ident.end(),
            Self::CallExpr(c) => c.rparen,
            Self::Selector(s) => s.name.end(),
            Self::TypeAssert(t) => t.rparen,
            Self::TypeSwitchGuard(t) => t.rparen,
            Self::SliceExpr(s) => s.expr.end(),
            Self::Token(t) => t.pos,
            Self::DotDotDotArgs(a) => a.expr.end(),
            Self::Expr(e) => e.end(),
            _ => panic!("impossible end of {:?}", self),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Operation<'a> {
    pub op: OperatorPos,
    pub x: Expression<'a>,
    pub y: Option<Expression<'a>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct OperatorPos {
    pub op: Operator,
    pub pos: Pos,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct SliceExpr<'a> {
    pub lbrack: Pos,
    pub rbrack: Pos,
    pub expr: Expression<'a>,
    pub indice: Vec<Expression<'a>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Index<'a> {
    pub lbrack: Pos,
    pub rbrack: Pos,
    pub indices: Expression<'a>,
    pub expr: Expression<'a>,
}

pub(crate) type ListExpr<'a> = Vec<(Expression<'a>, Pos /*comma*/)>;

impl<'a> Range for ListExpr<'a> {
    fn start(&self) -> Pos {
        self[0].0.start()
    }

    fn end(&self) -> Pos {
        let (expr, p) = &self[self.len() - 1];
        if p.is_zero() { expr.end() } else { *p }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct CallExpr<'a> {
    pub pexpr: Expression<'a>,
    pub lparen: Pos,
    pub args: Vec<(Expression<'a>, TokenPos)>,
    pub rparen: Pos,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TokenPos {
    pub tok: Token,
    pub pos: Pos,
}

impl Range for TokenPos {
    fn start(&self) -> Pos {
        self.pos.start()
    }

    fn end(&self) -> Pos {
        self.pos.end()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TypeAssert<'a> {
    pub pexpr: Expression<'a>,
    pub dot: Pos,
    pub lparen: Pos,
    pub typ: Expression<'a>,
    pub rparen: Pos,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TypeSwitchGuard<'a> {
    pub lhs: Option<(Identifier<'a>, Pos /*pos of ':='*/)>,
    pub x: Expression<'a>,
    pub dot: Pos,
    pub lparen: Pos,
    pub typ: Pos,
    pub rparen: Pos,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Selector<'a> {
    pub pexpr: Expression<'a>,
    pub dot: Pos,
    pub name: Identifier<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ParenedExpr<'a> {
    pub lparen: Pos,
    pub expr: Expression<'a>,
    pub rparen: Pos,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct CompositeLit<'a> {
    pub typ: Expression<'a>,
    pub value: LiteralValue<'a>,
}

impl<'a> CompositeLit<'a> {
    pub(crate) fn start(&self) -> Pos {
        self.typ.start()
    }

    pub(crate) fn end(&self) -> Pos {
        self.value.end()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct LiteralValue<'a> {
    pub lbrace: Pos,
    pub elem_list: ListExpr<'a>,
    pub rbrace: Pos,
}

impl<'a> LiteralValue<'a> {
    pub(crate) fn end(&self) -> Pos {
        self.rbrace
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct KeyedElement<'a> {
    pub key_and_colon: Option<(Expression<'a>, Pos)>,
    pub elem: Expression<'a>,
}

impl<'a> KeyedElement<'a> {
    pub(crate) fn start(&self) -> Pos {
        if let Some((kc, _)) = &self.key_and_colon {
            kc.start()
        } else {
            self.elem.start()
        }
    }
}
