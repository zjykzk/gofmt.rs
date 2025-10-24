use std::collections::HashMap;

use crate::ast::*;
use crate::scanner::Scanner;
use crate::source::Source;
use crate::tokens::{LiteralKind, Operator, Precedence, Token};

use anyhow::{Result, anyhow};

use thiserror;

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
enum Error {
    #[error("expected {expected:?}, found {found:?}")]
    UnExpectedToken { expected: Token, found: Token },
    #[error("expected literal: `{0}`")]
    ExpectedLitKind(LiteralKind),
    #[error("expected {expected:?}, found {found:?}")]
    UnExpectedOp { expected: Operator, found: Operator },
    #[error("cannot process token:{token:?} when parsing {context:?}")]
    CannotProcToken { token: Token, context: &'static str },
    #[error("cannot process operator:{op:?} when parsing {context:?}")]
    CannotProcOp { op: Operator, context: &'static str },
}

#[derive(Debug)]
pub(crate) struct Parser<'a, T: FnMut(usize, usize, &str), S: Source<'a>> {
    xnest: i16, // expression nesting level (for complit ambiguity resolution)
    scanner: Scanner<'a, T, S>,
    parser_of_tok: HashMap<Token, fn(&mut Parser<'a, T, S>) -> Result<AST<'a>>>,
    pub comments: Vec<CommentGroup<'a>>,

    line_comment_index: Option<CommentIndex>,
    lead_comment_index: Option<CommentIndex>,
}

#[inline]
fn expected_token(found: Token, expected: Token) -> Result<()> {
    (found == expected)
        .then_some(())
        .ok_or(anyhow!(Error::UnExpectedToken { expected, found }))
}

#[inline]
fn expected_kind(cur_kind: LiteralKind, exp_kind: LiteralKind) -> Result<()> {
    (cur_kind == exp_kind)
        .then_some(())
        .ok_or(anyhow!(Error::ExpectedLitKind(exp_kind)))
}

#[inline]
fn expected_op(found: Operator, expected: Operator) -> Result<()> {
    (found == expected)
        .then_some(())
        .ok_or(anyhow!(Error::UnExpectedOp { expected, found }))
}

impl<'a, T: FnMut(usize, usize, &str), S: Source<'a>> Parser<'a, T, S> {
    pub fn new(source: S, errh: T) -> Self {
        Parser {
            line_comment_index: None,
            lead_comment_index: None,
            comments: vec![],
            xnest: 0,
            scanner: Scanner::new(source, errh),
            parser_of_tok: HashMap::from([
                (
                    Token::Package,
                    Parser::package as fn(&mut Parser<'a, T, S>) -> Result<AST<'a>>,
                ),
                (
                    Token::Import,
                    Parser::import_decl as fn(&mut Parser<'a, T, S>) -> Result<AST<'a>>,
                ),
                (
                    Token::Type,
                    Parser::type_decl as fn(&mut Parser<'a, T, S>) -> Result<AST<'a>>,
                ),
                (
                    Token::Func,
                    Parser::method_or_func_decl as fn(&mut Parser<'a, T, S>) -> Result<AST<'a>>,
                ),
                (
                    Token::Const,
                    Parser::const_decl as fn(&mut Parser<'a, T, S>) -> Result<AST<'a>>,
                ),
                (
                    Token::Var,
                    Parser::var_decl as fn(&mut Parser<'a, T, S>) -> Result<AST<'a>>,
                ),
            ]),
        }
    }

    fn semi(&mut self) -> Option<Pos> {
        if self.tok() != Token::Semi {
            return None;
        }

        if self.scanner.lit == "semicolon" {
            Some(self.get_pos_then_next())
        } else {
            self.next(); // skip ; from the newline
            None
        }
    }

    fn calc_endline_of_comment(tok: Token, mut endline: u32, c: &'a str) -> u32 {
        if tok == Token::FullComment {
            for b in c.as_bytes() {
                if *b == b'\n' {
                    endline += 1;
                }
            }
        }

        return endline;
    }

    fn consume_comment_group(&mut self, n: u32) -> u32 {
        let mut endline = self.pos().lineno;
        let mut cg = CommentGroup { comments: vec![] };
        let (mut tok, mut pos, mut content) = (self.tok(), self.pos(), self.scanner.lit);
        while (tok == Token::FullComment || tok == Token::LineComment) && pos.lineno <= endline + n
        {
            endline = Self::calc_endline_of_comment(tok, pos.lineno, self.scanner.lit);
            cg.comments.push(Comment { pos, content, tok });
            self.scanner.next();
            (tok, pos, content) = (self.tok(), self.pos(), self.scanner.lit);
        }

        self.comments.push(cg);
        endline
    }

    #[inline(always)]
    fn next(&mut self) -> Token {
        self.line_comment_index = None;
        self.lead_comment_index = None;
        let prev_pos = self.pos();

        self.scanner.next();

        let tok = self.scanner.tok;
        if tok != Token::LineComment && tok != Token::FullComment {
            return tok;
        }

        let mut endline_of_comment = 0;
        if prev_pos.lineno == self.pos().lineno {
            endline_of_comment = self.consume_comment_group(0);
            let tok = self.tok();
            if endline_of_comment != self.pos().lineno || tok == Token::Semi || tok == Token::EOF {
                self.line_comment_index = Some((self.comments.len() - 1) as CommentIndex);
            }
        }

        while let tok = self.tok()
            && (tok == Token::LineComment || tok == Token::FullComment)
        {
            endline_of_comment = self.consume_comment_group(1);
        }

        if endline_of_comment + 1 == self.pos().lineno {
            self.lead_comment_index = Some((self.comments.len() - 1) as CommentIndex);
        }

        self.scanner.tok
    }

    #[inline(always)]
    fn pos(&self) -> Pos {
        Pos::new(self.scanner.lineno, self.scanner.col)
    }

    #[inline(always)]
    fn tok(&self) -> Token {
        self.scanner.tok
    }

    fn package(&mut self) -> Result<AST<'a>> {
        let pos = self.pos();
        self.next();
        Ok(AST::PackageClause(Box::new(PackageClause {
            name: self.name()?,
            pos,
        })))
    }

    fn want(&mut self, tok: Token) -> Result<()> {
        self.got(tok)
            .then_some(())
            .ok_or(anyhow!(Error::UnExpectedToken {
                expected: tok,
                found: self.tok()
            }))
    }

    /// returns `true` if the current token equals to the specified `tok`, `false` otherwise.
    ///
    /// it does scan next token when it is true.
    fn got(&mut self, tok: Token) -> bool {
        if self.scanner.tok == tok {
            self.next();
            true
        } else {
            false
        }
    }

    pub(crate) fn parse(&mut self) -> Result<Vec<AST<'a>>> {
        let mut ast = vec![];

        self.next();
        while self.tok() != Token::EOF {
            let h = self
                .parser_of_tok
                .get(&self.scanner.tok)
                .unwrap_or(&(Parser::stmt as fn(&mut Parser<'a, T, S>) -> Result<AST<'a>>));
            ast.push(h(self)?);
            if let Some(semi) = self.semi() {
                ast.push(AST::Semi(semi));
            }
        }

        Ok(ast)
    }

    fn method_or_func_decl(&mut self) -> Result<AST<'a>> {
        let doc = self.lead_comment_index;
        let func = self.get_pos_then_next();
        if self.tok() == Token::Name {
            // func name(params) result body
            return self.func_decl(func, doc);
        }

        // 1. func (params) [ result ] body
        // 2. func (params) name(params) result body
        let lparen = self.want_tok_pos(Token::Lparen)?;
        let params = self.params(lparen)?;
        match self.tok() {
            Token::Lparen => {
                // now: result
                let lparen = self.get_pos_then_next();
                let ret = self.params(lparen)?;
                let expr = if self.tok() == Token::Lbrace {
                    Expression::FuncLit(Box::new(FuncLit {
                        func,
                        sign: FuncSign {
                            params,
                            ret: Some(FuncResult::Params(ret)),
                        },
                        body: self.block()?,
                    }))
                } else {
                    Expression::FuncType(Box::new(FuncType {
                        func,
                        params,
                        ret: Some(FuncResult::Params(ret)),
                    }))
                };
                Ok(AST::Stmt(Stmt::Expr(self.pexpr(expr)?)))
            }
            Token::Lbrace => {
                // func (params) body
                let body = self.block()?;
                Ok(AST::Stmt(Stmt::Expr(self.pexpr(Expression::FuncLit(
                    Box::new(FuncLit {
                        func,
                        sign: FuncSign { params, ret: None },
                        body,
                    }),
                ))?)))
            }
            _ => {
                let ret = self.r#type(self.tok())?; // try get result
                if self.tok() == Token::Lparen {
                    // method decl:func (params) name(params) [result] body
                    if let Expression::Ident(name) = ret {
                        let receiver = params;
                        let lparen = self.want_tok_pos(Token::Lparen)?;
                        let sign = self.func_sign(lparen)?;
                        let body = if self.tok() == Token::Lbrace {
                            Some(self.block()?)
                        } else {
                            None
                        };
                        Ok(AST::MethodDecl(Box::new(MethodDecl {
                            doc,
                            func,
                            receiver,
                            name: *name,
                            sign,
                            body,
                        })))
                    } else {
                        Err(anyhow!("method name missing GOT .."))
                    }
                } else {
                    // func (params) result body
                    let body = self.block()?;
                    Ok(AST::Stmt(Stmt::Expr(self.pexpr(Expression::FuncLit(
                        Box::new(FuncLit {
                            func,
                            sign: FuncSign {
                                params,
                                ret: Some(FuncResult::Type(ret)),
                            },
                            body,
                        }),
                    ))?)))
                }
            }
        }
    }

    fn func_decl(&mut self, func: Pos, doc: Option<CommentIndex>) -> Result<AST<'a>> {
        let name = self.name()?;
        let type_params = if self.tok() == Token::Lbrack {
            let lbrack = self.get_pos_then_next();
            let param_list = self.type_param_list(None, None)?;
            let rbrack = self.want_tok_pos(Token::Rbrack)?;
            Some(TypeParameters {
                lbrack,
                param_list,
                rbrack,
            })
        } else {
            None
        };

        let lparen = self.want_tok_pos(Token::Lparen)?;
        let sign = self.func_sign(lparen)?;
        let body = if self.tok() == Token::Lbrace {
            Some(self.block()?)
        } else {
            None
        };
        Ok(AST::FuncDecl(Box::new(FuncDecl {
            doc,
            func,
            name,
            type_params,
            sign,
            body,
        })))
    }

    fn block(&mut self) -> Result<BlockStmt<'a>> {
        let lbrace = self.get_pos_then_next();
        let stmts = self.stmt_list()?;
        let rbrace = self.want_tok_pos(Token::Rbrace)?;

        Ok(BlockStmt {
            lbrace,
            stmts,
            rbrace,
        })
    }

    fn func_type(&mut self) -> Result<FuncType<'a>> {
        let func = self.get_pos_then_next();
        let lparen = self.want_tok_pos(Token::Lparen)?;
        let sign = self.func_sign(lparen)?;

        Ok(FuncType {
            func,
            params: sign.params,
            ret: sign.ret,
        })
    }

    fn func_sign(&mut self, lparen: Pos) -> Result<FuncSign<'a>> {
        let params = self.params(lparen)?;
        let tok = self.tok();
        let ret = match tok {
            Token::Lparen => {
                let lparen = self.get_pos_then_next();
                Some(FuncResult::Params(self.params(lparen)?))
            }
            Token::Lbrace => None, // for func&method decl to consume
            Token::Semi => None,
            _ => {
                if let Ok(typ) = self.r#type(tok) {
                    Some(FuncResult::Type(typ))
                } else {
                    None // ignore the err, it will be caught later
                }
            }
        };
        Ok(FuncSign { params, ret })
    }

    fn params(&mut self, lparen: Pos) -> Result<Params<'a>> {
        match self.tok() {
            Token::Rparen => Ok(Params {
                lparen,
                list: None,
                rparen: self.get_pos_then_next(),
            }),
            _ => {
                let list = self.param_list()?;
                Ok(Params {
                    lparen,
                    list: Some(list),
                    rparen: self.want_tok_pos(Token::Rparen)?,
                })
            }
        }
    }

    fn param_list(&mut self) -> Result<ParamList<'a>> {
        let mut list = vec![];
        while self.tok() != Token::Rparen {
            let dl = self.param_decl_or_param_list(!list.is_empty())?;
            match dl {
                (Some(decl), None) => list.push((
                    decl,
                    if self.tok() == Token::Comma {
                        self.get_pos_then_next()
                    } else {
                        Pos::zero()
                    },
                )),
                (None, Some(l)) => return Ok(l),
                _ => (), // impossible
            }
        }

        Ok(list)
    }

    // returns the param list when only types.
    fn param_decl_or_param_list(
        &mut self,
        need_decl: bool,
    ) -> Result<(Option<ParamDecl<'a>>, Option<ParamList<'a>>)> {
        match self.tok() {
            Token::Name => {
                let ident = self.name()?;
                match self.tok() {
                    Token::Rparen => Ok((
                        Some(ParamDecl {
                            idents: None,
                            dotdotdot: None,
                            typ: Expression::Ident(Box::new(ident)),
                        }),
                        None,
                    )),
                    Token::Lbrack => {
                        let expr = self.array_or_type_args()?;
                        Ok((
                            Some(if let Expression::Index(mut expr) = expr {
                                expr.expr = Expression::Ident(Box::new(ident));
                                ParamDecl {
                                    idents: None,
                                    dotdotdot: None,
                                    typ: Expression::Index(expr),
                                }
                            } else {
                                ParamDecl {
                                    idents: Some(IdentifierList {
                                        ident,
                                        followers: vec![],
                                    }),
                                    dotdotdot: None,
                                    typ: expr,
                                }
                            }),
                            None,
                        ))
                    }
                    Token::Dot => Ok((
                        Some(ParamDecl {
                            idents: None,
                            dotdotdot: None,
                            typ: self.qualified_name(ident)?,
                        }),
                        None,
                    )),
                    Token::Comma if need_decl => Ok((
                        Some(ParamDecl {
                            idents: None,
                            dotdotdot: None,
                            typ: Expression::Ident(Box::new(ident)),
                        }),
                        None,
                    )),
                    _ => {
                        let idents = self.ident_list_or_type_list(ident)?;
                        if idents.followers.len() == 1
                            && idents.followers[idents.followers.len() - 1].ident.pos.col == 0
                        {
                            return Ok((
                                Some(ParamDecl {
                                    idents: None,
                                    dotdotdot: None,
                                    typ: Expression::Ident(Box::new(ident)),
                                }),
                                None,
                            ));
                        }
                        let pl = self.idents_to_param_list(&idents);
                        if pl.is_some() {
                            return Ok((None, pl));
                        }
                        let dotdotdot = if self.tok() == Token::DotDotDot {
                            Some(self.get_pos_then_next())
                        } else {
                            None
                        };
                        Ok((
                            Some(ParamDecl {
                                idents: Some(idents),
                                dotdotdot,
                                typ: self.r#type(self.tok())?,
                            }),
                            None,
                        ))
                    }
                }
            }
            Token::DotDotDot => {
                let dotdotdot = Some(self.get_pos_then_next());
                let tok = self.tok();
                Ok((
                    Some(ParamDecl {
                        idents: None,
                        dotdotdot,
                        typ: self.r#type(tok)?,
                    }),
                    None,
                ))
            }
            t @ (Token::Star
            | Token::Interface
            | Token::Func
            | Token::Arrow
            | Token::Chan
            | Token::Map
            | Token::Struct
            | Token::Lparen) => Ok((
                Some(ParamDecl {
                    idents: None,
                    dotdotdot: None,
                    typ: self.r#type(t)?,
                }),
                None,
            )),
            Token::Lbrack => Ok((
                Some(ParamDecl {
                    idents: None,
                    dotdotdot: None,
                    typ: self.array_or_type_args()?,
                }),
                None,
            )),
            // t => panic!("{}", t),
            t => Err(anyhow!(Error::CannotProcToken {
                token: t,
                context: "param decl"
            })),
        }
    }

    fn idents_to_param_list(&self, idents: &IdentifierList<'a>) -> Option<ParamList<'a>> {
        let followers = &idents.followers;
        let follower_count = followers.len();

        if follower_count <= 0 {
            return None;
        }

        let is_last_comma = followers[follower_count - 1].ident.pos.col == 0;
        if self.tok() == Token::Rparen || is_last_comma {
            let mut param_lists = vec![(
                ParamDecl {
                    idents: None,
                    dotdotdot: None,
                    typ: Expression::Ident(Box::new(idents.ident)),
                },
                followers[0].comma,
            )];
            for (i, id) in followers[..follower_count - 1].iter().enumerate() {
                param_lists.push((
                    ParamDecl {
                        idents: None,
                        dotdotdot: None,
                        typ: Expression::Ident(Box::new(id.ident)),
                    },
                    followers[i + 1].comma,
                ));
            }
            if !is_last_comma {
                param_lists.push((
                    ParamDecl {
                        idents: None,
                        dotdotdot: None,
                        typ: Expression::Ident(Box::new(followers[follower_count - 1].ident)),
                    },
                    Pos::zero(),
                ));
            }
            return Some(param_lists);
        }
        None
    }

    // array&slice&type args type
    fn array_or_type_args(&mut self) -> Result<Expression<'a>> {
        let lbrack = self.want_tok_pos(Token::Lbrack)?;
        if self.tok() == Token::Rbrack {
            let rbrack = self.get_pos_then_next();
            let typ = self.r#type(self.tok())?;
            return Ok(Expression::SliceType(Box::new(SliceType {
                lbrack,
                rbrack,
                typ,
            })));
        }

        // x [n]E or x[n,], x[n1, n2], ...
        let arg = self.expr()?;
        let n = self.type_list(arg)?;
        let rbrack = self.want_tok_pos(Token::Rbrack)?;

        if let Some((_, Pos { col: 0, lineno: 0 })) = n.first() {
            if let Ok(typ) = self.r#type(self.tok()) {
                return Ok(Expression::ArrayType(Box::new(ArrayType {
                    lbrack,
                    rbrack,
                    len: Expression::ListExpr(n),
                    typ,
                })));
            }
        }

        // x[n,], x[n1, n2], ...
        Ok(Expression::Index(Box::new(Index {
            lbrack,
            rbrack,
            indices: Expression::ListExpr(n),
            // temp expr, it will be override, by the caller
            expr: Expression::ReplaceMe,
        })))
    }

    fn qualified_name(&mut self, pkg: Identifier<'a>) -> Result<Expression<'a>> {
        let mut x = if self.tok() == Token::Dot {
            let dot = self.get_pos_then_next();
            let ident = self.name()?;
            Expression::QualifiedName(Box::new(QualifiedName { pkg, dot, ident }))
        } else {
            Expression::Ident(Box::new(pkg))
        };

        if self.tok() == Token::Lbrack {
            x = Expression::TypeInstance(Box::new(self.type_instance(x)?));
        }

        Ok(x)
    }

    fn import_decl(&mut self) -> Result<AST<'a>> {
        let pos = self.pos();
        let import = if self.next() == Token::Lparen {
            let lparen = self.pos();
            let mut specs = vec![];

            self.next();
            let mut doc = self.lead_comment_index;
            while self.tok() != Token::Rparen {
                specs.push(self.import_spec(self.tok(), doc)?);
                self.got(Token::Semi);
                doc = self.lead_comment_index;
            }
            let rparen = self.pos();
            expected_token(self.next(), Token::Semi)?;
            ImportOption::Group(ImportGroup {
                lparen,
                specs,
                rparen,
            })
        } else {
            let s = self.import_spec(self.tok(), None)?;
            self.got(Token::Semi);
            ImportOption::Spec(s)
        };
        Ok(AST::ImportDecl(Box::new(ImportDecl { import, pos })))
    }

    fn import_spec(&mut self, tok: Token, doc: Option<CommentIndex>) -> Result<ImportSpec<'a>> {
        let pkg_name = match tok {
            Token::Name | Token::Dot => {
                let n = Some(Identifier {
                    name: self.scanner.segment(),
                    pos: self.get_pos_then_next(),
                });
                n
            }
            _ => None,
        };

        expected_token(self.tok(), Token::Literal)?;
        let path = self.string_lit()?;
        let line_comment = self.line_comment_index;
        Ok(ImportSpec {
            pkg_name,
            path,
            doc,
            line_comment,
        })
    }

    /// ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
    /// ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
    fn const_decl(&mut self) -> Result<AST<'a>> {
        return Ok(AST::ConstDecl(self.var_decl0()?));
    }

    /// VarDecl = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
    /// VarSpec = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
    fn var_decl(&mut self) -> Result<AST<'a>> {
        return Ok(AST::VarDecl(self.var_decl0()?));
    }

    fn var_decl0(&mut self) -> Result<Box<VarDecl<'a>>> {
        let doc = self.lead_comment_index;
        let pos = self.get_pos_then_next();
        if self.tok() == Token::Lparen {
            let lparen = self.get_pos_then_next();
            let mut specs = vec![];
            while self.tok() != Token::Rparen {
                let spec = self.var_spec(doc)?;
                specs.push((
                    spec,
                    if let Some(pos) = self.semi() {
                        pos
                    } else {
                        Pos::zero()
                    },
                ));
            }
            let rparen = self.get_pos_then_next();
            Ok(Box::new(VarDecl {
                pos,
                decl: VarDeclOption::Group(VarGroup {
                    lparen,
                    rparen,
                    specs,
                }),
            }))
        } else {
            Ok(Box::new(VarDecl {
                pos,
                decl: VarDeclOption::Spec(self.var_spec(doc)?),
            }))
        }
    }

    fn var_spec(&mut self, doc: Option<CommentIndex>) -> Result<VarSpec<'a>> {
        let ident = self.name()?;
        let ident_list = self.ident_list(ident)?;
        match self.tok() {
            Token::EOF | Token::Semi | Token::Rparen => Ok(VarSpec {
                doc,
                line_comment: self.line_comment_index,
                ident_list,
                typ: None,
                eq: None,
                expr_list: None,
            }),
            tok => {
                let typ = self.r#type(tok).ok();
                let eq = if self.tok() == Token::Assign {
                    Some(self.get_pos_then_next())
                } else {
                    None
                };
                let expr_list = if eq.is_some() {
                    self.expr_list().ok()
                } else {
                    None
                };

                Ok(VarSpec {
                    doc,
                    line_comment: self.line_comment_index,
                    ident_list,
                    typ,
                    eq,
                    expr_list,
                })
            }
        }
    }

    // TypeDecl = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
    // TypeSpec = AliasDecl | TypeDef .
    fn type_decl(&mut self) -> Result<AST<'a>> {
        Ok(AST::TypeDecl(self.type_decl0()?))
    }

    fn type_decl0(&mut self) -> Result<Box<TypeDecl<'a>>> {
        let pos = self.pos();

        let doc = self.lead_comment_index;
        match self.next() {
            Token::Lparen => {
                let lparen = self.get_pos_then_next();
                let mut specs = vec![];
                while self.tok() != Token::Rparen {
                    let spec_doc = self.lead_comment_index;
                    let spec = self.type_spec(spec_doc)?;
                    specs.push((
                        spec,
                        if self.tok() == Token::Semi && self.scanner.lit == "semicolon" {
                            self.pos()
                        } else {
                            Pos::zero()
                        },
                    ));
                    self.want(Token::Semi)?;
                }
                Ok(Box::new(TypeDecl {
                    pos,
                    decl: TypeDeclOption::Group(TypeGroup {
                        lparen,
                        rparen: self.get_pos_then_next(),
                        specs,
                        doc,
                    }),
                }))
            }
            Token::Name => Ok(Box::new(TypeDecl {
                pos,
                decl: TypeDeclOption::Spec(self.type_spec(doc)?),
            })),
            _ => Err(anyhow!("type decl name not implemented")),
        }
    }

    fn type_spec(&mut self, doc: Option<CommentIndex>) -> Result<TypeSpec<'a>> {
        let ident = self.name()?;
        match self.tok() {
            Token::Assign => {
                let eq = self.get_pos_then_next();
                let tok = self.tok();
                Ok(TypeSpec::AliasDecl(AliasDecl {
                    ident,
                    params: None,
                    eq,
                    typ: self.r#type(tok)?,
                }))
            }
            Token::Lbrack => {
                let lbrack = self.get_pos_then_next();
                match self.tok() {
                    Token::Rbrack => Ok(TypeSpec::TypeDef(TypeDef {
                        doc,
                        ident,
                        params: None,
                        typ: self.slice_type(lbrack)?,
                    })),
                    Token::Name => {
                        let mut x = Expression::Ident(Box::new(self.name()?));
                        if self.tok() != Token::Lbrack {
                            self.xnest += 1;
                            x = self.pexpr(x)?;
                            x = self.binary_expr(x, Precedence::None)?;
                            self.xnest -= 1;
                        }

                        let (opt_name, opt_type) = Self::extract(x, self.tok() == Token::Comma);
                        if opt_name.is_some() && (opt_type.is_some() || self.tok() != Token::Rbrack)
                        {
                            // 1. name + ","
                            // 2. name type ","
                            // 3. name type "]"
                            let params = Some(self.type_params(opt_name, opt_type, lbrack)?);
                            let typ = self.r#type(self.tok())?;
                            Ok(TypeSpec::TypeDef(TypeDef {
                                doc,
                                ident,
                                params,
                                typ,
                            }))
                        } else {
                            Ok(TypeSpec::TypeDef(TypeDef {
                                doc,
                                ident,
                                params: None,
                                typ: self.array(
                                    opt_type.or(opt_name.map(|id| Expression::Ident(Box::new(id)))),
                                    lbrack,
                                )?,
                            }))
                        }
                    }
                    _ => Ok(TypeSpec::TypeDef(TypeDef {
                        doc,
                        ident,
                        params: None,
                        typ: self.array(None, lbrack)?,
                    })),
                }
            }
            r => Ok(TypeSpec::TypeDef(TypeDef {
                doc,
                params: None,
                ident,
                typ: self.r#type(r)?,
            })),
        }
    }

    // extract splits the expression x into (name, expr) if syntactically
    // x can be written as name expr. The split only happens if expr is a type
    // element (per the isTypeElem predicate) or if force is set.
    // If x is just a name, the result is (name, nil). If the split succeeds,
    // the result is (name, expr). Otherwise the result is (nil, x).
    // Examples:
    //
    //	x           force    name    expr
    //	------------------------------------
    //	P*[]int     T/F      P       *[]int
    //	P*E         T        P       *E
    //	P*E         F        nil     P*E
    //	P([]int)    T/F      P       []int
    //	P(E)        T        P       E
    //	P(E)        F        nil     P(E)
    //	P*E|F|~G    T/F      P       *E|F|~G
    //	P*E|F|G     T        P       *E|F|G
    //	P*E|F|G     F        nil     P*E|F|G
    fn extract(
        expr: Expression<'a>,
        force: bool,
    ) -> (Option<Identifier<'a>>, Option<Expression<'a>>) {
        match expr {
            Expression::Ident(id) => (Some(*id), None),
            Expression::Operation(mut opt) => match *opt {
                Operation { y: None, .. } => (None, Some(Expression::Operation(opt))),
                Operation {
                    x: optx,
                    y: Some(opty),
                    op:
                        OperatorPos {
                            op: Operator::Mul, ..
                        },
                    ..
                } => match optx {
                    Expression::Ident(id) if (force || Self::is_type_elem(&opty)) => {
                        (opt.x, opt.y) = (opty, None);
                        (Some(*id), Some(Expression::Operation(opt)))
                    }
                    _ => {
                        (opt.x, opt.y) = (optx, Some(opty));
                        (None, Some(Expression::Operation(opt)))
                    }
                },
                Operation {
                    x: optx,
                    y: Some(opty),
                    op:
                        OperatorPos {
                            op: Operator::Or, ..
                        },
                    ..
                } => match Self::extract(optx, force || Self::is_type_elem(&opty)) {
                    (Some(name), Some(lhs)) => {
                        (opt.x, opt.y) = (lhs, Some(opty));
                        (Some(name), Some(Expression::Operation(opt)))
                    }
                    (Some(name), None) => {
                        let optx = Expression::Ident(Box::new(name));
                        let opty = Some(opty);
                        (opt.x, opt.y) = (optx, opty);
                        (None, Some(Expression::Operation(opt)))
                    }
                    (None, Some(expr)) => {
                        let optx = expr;
                        let opty = Some(opty);
                        (opt.x, opt.y) = (optx, opty);
                        (None, Some(Expression::Operation(opt)))
                    }
                    _ => panic!("extract lost"),
                },
                _ => (None, Some(Expression::Operation(opt))),
            },
            Expression::CallExpr(mut call) => {
                if let Expression::Ident(id) = call.pexpr {
                    let mut args = call.args;
                    match (args.pop(), args.pop()) {
                        (Some((arg0, tok_pos)), None) => {
                            if tok_pos.tok != Token::DotDotDot
                                && (force || Self::is_type_elem(&arg0))
                            {
                                return (
                                    Some(*id),
                                    Some(Expression::ParenedExpr(Box::new(ParenedExpr {
                                        lparen: call.lparen,
                                        expr: arg0,
                                        rparen: call.rparen,
                                    }))),
                                );
                            }
                            args.push((arg0, tok_pos));
                        }
                        (arg0, arg1) => {
                            if let Some(arg) = arg1 {
                                args.push(arg)
                            }
                            if let Some(arg) = arg0 {
                                args.push(arg)
                            }
                        }
                    };

                    call.args = args;
                    call.pexpr = Expression::Ident(id);
                }
                (None, Some(Expression::CallExpr(call)))
            }
            _ => (None, Some(expr)),
        }
    }

    // isTypeElem reports whether x is a (possibly parenthesized) type element expression.
    // The result is false if x could be a type element OR an ordinary (value) expression.
    fn is_type_elem(x: &Expression<'a>) -> bool {
        match x {
            Expression::ArrayType(_)
            | Expression::Struct(_)
            | Expression::FuncType(_)
            | Expression::InterfaceType(_)
            | Expression::SliceType(_)
            | Expression::MapType(_)
            | Expression::ChanType(_) => true,
            Expression::Operation(op) => match **op {
                Operation {
                    op:
                        OperatorPos {
                            op: Operator::Mul, ..
                        },
                    y: None,
                    ..
                } => false, // *Type
                Operation {
                    op:
                        OperatorPos {
                            op: Operator::Tilde,
                            ..
                        },
                    ..
                } => true,
                Operation { y: Some(ref y), .. } => Self::is_type_elem(y),
                Operation { ref x, .. } => Self::is_type_elem(x),
            },
            Expression::ParenedExpr(expr) => Self::is_type_elem(&expr.expr),
            _ => false,
        }
    }

    fn slice_type(&mut self, lbrack: Pos) -> Result<Expression<'a>> {
        let rbrack = self.get_pos_then_next();
        let tok = self.tok();
        Ok(Expression::SliceType(Box::new(SliceType {
            lbrack,
            rbrack,
            typ: self.r#type(tok)?,
        })))
    }

    fn type_params(
        &mut self,
        proceed_name: Option<Identifier<'a>>,
        proceed_typ: Option<Expression<'a>>,
        lbrack: Pos,
    ) -> Result<TypeParameters<'a>> {
        let param_list = self.type_param_list(proceed_name, proceed_typ)?;

        Ok(TypeParameters {
            lbrack,
            param_list,
            rbrack: self.want_tok_pos(Token::Rbrack)?,
        })
    }

    fn type_param_list(
        &mut self,
        proceed_ident: Option<Identifier<'a>>,
        proceed_typ: Option<Expression<'a>>,
    ) -> Result<Vec<(TypeParamDecl<'a>, Pos)>> {
        let mut decl = self.type_param_decl(proceed_ident, proceed_typ)?;

        let mut params = vec![];
        loop {
            let is_comma = self.tok() == Token::Comma;
            params.push((
                decl,
                if is_comma {
                    self.get_pos_then_next()
                } else {
                    Pos::zero()
                },
            ));

            if !is_comma || self.tok() == Token::Rbrack {
                break;
            }

            decl = self.type_param_decl(None, None)?;
        }

        Ok(params)
    }

    fn type_param_decl(
        &mut self,
        proceed_ident: Option<Identifier<'a>>,
        proceed_typ: Option<Expression<'a>>,
    ) -> Result<TypeParamDecl<'a>> {
        let ident = if let Some(ident) = proceed_ident {
            ident
        } else {
            self.name()?
        };

        let idents = if proceed_typ.is_some() {
            IdentifierList {
                ident,
                followers: vec![],
            }
        } else {
            self.ident_list(ident)?
        };

        let type_constraint = self.type_elem(proceed_typ)?;

        Ok(TypeParamDecl {
            idents,
            type_constraint,
        })
    }

    fn type_elem(&mut self, typ: Option<Expression<'a>>) -> Result<TypeElem<'a>> {
        let term = if let Some(typ) = typ {
            typ
        } else {
            self.type_term()?
        };
        let followers = self.type_or_terms()?;
        Ok(TypeElem { term, followers })
    }

    fn type_or_terms(&mut self) -> Result<Vec<OrTypeTerm<'a>>> {
        let mut or_terms = vec![];
        while self.tok() == Token::Operator {
            expected_op(self.scanner.op, Operator::Or)?;
            let or = self.get_pos_then_next();
            or_terms.push(OrTypeTerm {
                or,
                term: self.type_term()?,
            });
        }
        Ok(or_terms)
    }

    fn type_term(&mut self) -> Result<Expression<'a>> {
        Ok(self.r#type(self.tok())?)
    }

    fn r#type(&mut self, tok: Token) -> Result<Expression<'a>> {
        match tok {
            Token::Interface => Ok(Expression::InterfaceType(Box::new(self.interface()?))),
            Token::Func => Ok(Expression::FuncType(Box::new(self.func_type()?))),
            Token::Arrow => {
                let recv = self.get_pos_then_next();
                let ch = self.want_tok_pos(Token::Chan)?;
                let tok = self.tok();
                Ok(Expression::ChanType(Box::new(ChanType {
                    ch,
                    dir: ChanDir::Recv(recv),
                    typ: self.r#type(tok)?,
                })))
            }
            Token::Chan => {
                let ch = self.get_pos_then_next();
                let dir = if self.tok() == Token::Arrow {
                    ChanDir::Send(self.get_pos_then_next())
                } else {
                    ChanDir::Both
                };
                let typ = self.r#type(self.tok())?;
                Ok(Expression::ChanType(Box::new(ChanType { ch, typ, dir })))
            }
            Token::Map => {
                let pos = self.get_pos_then_next();
                let lbrack = self.want_tok_pos(Token::Lbrack)?;
                let tok = self.tok();
                let key_type = self.r#type(tok)?;
                let rbrack = self.want_tok_pos(Token::Rbrack)?;
                let tok = self.tok();
                let ele_type = self.r#type(tok)?;
                Ok(Expression::MapType(Box::new(MapType {
                    pos,
                    lbrack,
                    rbrack,
                    key_type,
                    ele_type,
                })))
            }
            Token::Star => {
                let star = self.get_pos_then_next();
                let tok = self.tok();
                Ok(Expression::Operation(Box::new(Operation {
                    op: OperatorPos {
                        op: Operator::Mul,
                        pos: star,
                    },
                    x: self.r#type(tok)?,
                    y: None,
                })))
            }
            Token::Name => {
                let name = self.name()?;
                Ok(self.qualified_name(name)?)
            }
            Token::Struct => Ok(Expression::Struct(self.struct_decl()?)),
            Token::Lparen => {
                let lparen = self.get_pos_then_next();
                let tok = self.tok();
                let typ = self.r#type(tok)?;
                let rparen = self.want_tok_pos(Token::Rparen)?;
                Ok(Expression::ParenedExpr(Box::new(ParenedExpr {
                    lparen,
                    rparen,
                    expr: typ,
                })))
            }
            Token::Lbrack => {
                let lbrack = self.get_pos_then_next();
                if self.tok() == Token::Rbrack {
                    self.slice_type(lbrack)
                } else {
                    self.array(None, lbrack)
                }
            }
            Token::Operator => {
                expected_op(self.scanner.op, Operator::Tilde)?;
                let pos = self.get_pos_then_next();
                let tok = self.tok();

                Ok(Expression::UnderlyingType(Box::new(UnderlyingType {
                    pos,
                    typ: self.r#type(tok)?,
                })))
            }
            // t => panic!("cannot process {}", t),
            t => Err(anyhow!(Error::CannotProcToken {
                token: t,
                context: "type"
            })),
        }
    }

    fn array(&mut self, len: Option<Expression<'a>>, lbrack: Pos) -> Result<Expression<'a>> {
        let len = if let Some(len) = len {
            len
        } else {
            if self.tok() == Token::DotDotDot {
                Expression::Token(Box::new(TokenPos {
                    tok: Token::DotDotDot,
                    pos: self.get_pos_then_next(),
                }))
            } else {
                self.expr()?
            }
        };

        let rbrack = self.want_tok_pos(Token::Rbrack)?;
        let typ = self.r#type(self.tok())?;

        Ok(Expression::ArrayType(Box::new(ArrayType {
            lbrack,
            len,
            rbrack,
            typ,
        })))
    }

    fn type_instance(&mut self, type_name: Expression<'a>) -> Result<TypeInstance<'a>> {
        let lbrack = self.get_pos_then_next();

        let mut type_args = vec![];
        loop {
            let typ = self.r#type(self.tok())?;
            let pos = self.pos();
            let is_next_tok_comma = self.got(Token::Comma);
            type_args.push((typ, if is_next_tok_comma { pos } else { Pos::zero() }));
            if is_next_tok_comma && self.tok() != Token::Rbrack {
                continue;
            }

            let rbrack = self.want_tok_pos(Token::Rbrack)?;
            break Ok(TypeInstance {
                lbrack,
                rbrack,
                type_args,
                type_name,
            });
        }
    }

    // StructType = "struct" "{" { FieldDecl ";" } "}" .
    fn struct_decl(&mut self) -> Result<Box<StructType<'a>>> {
        let pos = self.want_tok_pos(Token::Struct)?;
        let lbrace = self.want_tok_pos(Token::Lbrace)?;

        let mut field_decls = vec![];
        loop {
            match self.tok() {
                Token::Name | Token::Star => {
                    let decl = self.field_decl()?;
                    field_decls.push(decl);
                    // field_decls.push((
                    //     decl,
                    //     if self.tok() == Token::Semi && self.scanner.lit == "semicolon" {
                    //         self.get_pos_then_next()
                    //     } else {
                    //         Pos::zero()
                    //     },
                    // ));
                }
                Token::Semi => {
                    // for simplity processing the sub-element
                    self.next();
                    continue;
                }
                Token::Rbrace => {
                    break;
                }
                t => {
                    return Err(anyhow!(Error::CannotProcToken {
                        token: t,
                        context: "decl struct"
                    }));
                }
            };
        }

        Ok(Box::new(StructType {
            lbrace,
            pos,
            field_decls,
            rbrace: self.want_tok_pos(Token::Rbrace)?,
        }))
    }

    // FieldDecl     = (IdentifierList Type | EmbeddedField) [ Tag ] .
    // EmbeddedField = [ "*" ] TypeName [ TypeArgs ] .
    // Tag            = string_lit .
    fn field_decl(&mut self) -> Result<(FieldDeclOption<'a>, Pos /*semi*/)> {
        let doc = self.lead_comment_index;
        match self.tok() {
            Token::Name => {
                let name = self.name()?;
                match self.tok() {
                    Token::Name
                    | Token::Comma
                    | Token::Star
                    | Token::Interface
                    | Token::Func
                    | Token::Arrow
                    | Token::Chan
                    | Token::Map
                    | Token::Struct => {
                        let (f, pos) = self.field_names(name, doc)?;
                        Ok((FieldDeclOption::Fields(f), pos))
                    }
                    Token::Lbrack => {
                        let expr = self.array_or_type_args()?;
                        let tag = if self.tok() == Token::Literal {
                            Some(self.string_lit()?)
                        } else {
                            None
                        };
                        let line_comment = self.line_comment_index;
                        let pos = self.semi().unwrap_or(Pos::zero());
                        if let Expression::Index(mut expr) = expr {
                            expr.expr = Expression::Ident(Box::new(name));
                            Ok((
                                FieldDeclOption::Embedded(EmbeddedFieldTypeField {
                                    doc,
                                    line_comment,
                                    typ: Expression::Index(expr),
                                    tag,
                                }),
                                pos,
                            ))
                        } else {
                            Ok((
                                FieldDeclOption::Fields(Fields {
                                    doc,
                                    line_comment,
                                    idents: IdentifierList {
                                        ident: name,
                                        followers: vec![],
                                    },
                                    typ: expr,
                                    tag,
                                }),
                                pos,
                            ))
                        }
                    }
                    _ => {
                        let mut line_comment = self.line_comment_index;
                        let (typ, tag) = self.embedded_field(&name)?;
                        if line_comment.is_none() {
                            line_comment = self.line_comment_index
                        }
                        let pos = self.semi().unwrap_or(Pos::zero());
                        Ok((
                            FieldDeclOption::Embedded(EmbeddedFieldTypeField {
                                doc,
                                typ,
                                tag,
                                line_comment: line_comment,
                            }),
                            pos,
                        ))
                    }
                }
            }
            Token::Star => {
                let pos = self.get_pos_then_next();
                let name = self.name()?;
                let line_comment = self.line_comment_index;
                let (typ, tag) = self.embedded_field(&name)?;
                Ok((
                    FieldDeclOption::Embedded(EmbeddedFieldTypeField {
                        tag,
                        typ: Expression::Operation(Box::new(Operation {
                            op: OperatorPos {
                                pos,
                                op: Operator::Mul,
                            },
                            x: typ,
                            y: None,
                        })),
                        doc,
                        line_comment: line_comment,
                    }),
                    self.semi().unwrap_or(Pos::zero()),
                ))
            }
            _ => Err(anyhow!("field_decls not implemented:{}", self.tok())),
        }
    }

    fn embedded_field(
        &mut self,
        ident: &Identifier<'a>,
    ) -> Result<(Expression<'a>, Option<BasicLit<'a>>)> {
        let ident = Identifier { ..*ident };
        match self.tok() {
            Token::Lbrack => {
                let type_name = Expression::Ident(Box::new(ident));
                let inst = self.type_instance(type_name)?;
                Ok((
                    Expression::TypeInstance(Box::new(inst)),
                    if self.tok() == Token::Literal {
                        Some(self.string_lit()?)
                    } else {
                        None
                    },
                ))
            }
            Token::Dot => {
                let dot = self.get_pos_then_next();
                let qual_name = Expression::QualifiedName(Box::new(QualifiedName {
                    pkg: ident,
                    dot,
                    ident: self.name()?,
                }));

                let type_def = if self.tok() == Token::Lbrack {
                    Expression::TypeInstance(Box::new(self.type_instance(qual_name)?))
                } else {
                    qual_name
                };

                Ok((
                    type_def,
                    if self.tok() == Token::Literal {
                        Some(self.string_lit()?)
                    } else {
                        None
                    },
                ))
            }
            Token::Literal => {
                let lit = self.string_lit()?;
                Ok((Expression::Ident(Box::new(ident)), Some(lit)))
            }
            Token::Semi => {
                self.next();
                Ok((Expression::Ident(Box::new(ident)), None))
            }
            Token::Rbrace => Ok((Expression::Ident(Box::new(ident)), None)),
            Token::LineComment | Token::FullComment => {
                Ok((Expression::Ident(Box::new(ident)), None))
            }
            Token::Name => Ok((Expression::Ident(Box::new(ident)), None)), // Next field declaration
            r => Err(anyhow!("EmbeddedField not implemented: {}", r)),
        }
    }

    fn string_lit(&mut self) -> Result<BasicLit<'a>> {
        expected_kind(self.scanner.kind, LiteralKind::String)?;
        let lit = BasicLit {
            kind: self.scanner.kind,
            pos: self.pos(),
            value: self.scanner.segment(),
        };
        self.next();
        Ok(lit)
    }

    #[allow(dead_code)]
    fn debug(&self, s: &str) {
        println!(
            "{}, tok:[{}], lit:[{}], pos:{:?}",
            s,
            self.tok(),
            self.scanner.segment(),
            self.pos()
        );
    }

    fn name(&mut self) -> Result<Identifier<'a>> {
        expected_token(self.tok(), Token::Name)?;
        let ident = Identifier {
            pos: self.pos(),
            name: self.scanner.segment(),
        };
        self.next();
        Ok(ident)
    }

    fn ident_list(&mut self, ident: Identifier<'a>) -> Result<IdentifierList<'a>> {
        let mut followers = vec![];
        while self.tok() == Token::Comma {
            let comma = self.get_pos_then_next();
            followers.push(CommaAndIdentifier {
                comma,
                ident: self.name()?,
            });
        }
        Ok(IdentifierList { ident, followers })
    }

    // fn is_type_list(idents: &IdentifierList<'a>) -> bool {
    //     !idents.followers.is_empty()
    //         && idents.followers[idents.followers.len() - 1]
    //             .ident
    //             .pos
    //             .is_zero()
    // }

    // `pos` in last elements in the `followers` is zero means type list.
    fn ident_list_or_type_list(&mut self, ident: Identifier<'a>) -> Result<IdentifierList<'a>> {
        let mut followers = vec![];
        while self.tok() == Token::Comma {
            let comma = self.get_pos_then_next();
            if self.tok() == Token::Name {
                followers.push(CommaAndIdentifier {
                    comma,
                    ident: self.name()?,
                });
            } else {
                // case `f(a,
                // )`
                followers.push(CommaAndIdentifier {
                    comma,
                    ident: Identifier {
                        name: "",
                        pos: Pos::zero(),
                    },
                });
                break;
            }
        }
        Ok(IdentifierList { ident, followers })
    }

    fn field_names(
        &mut self,
        ident: Identifier<'a>,
        doc: Option<CommentIndex>,
    ) -> Result<(Fields<'a>, Pos)> {
        let idents = self.ident_list(ident)?;
        let typ = self.r#type(self.tok())?;
        let tag = match self.tok() {
            Token::Literal => {
                expected_kind(self.scanner.kind, LiteralKind::String)?;
                let lit = BasicLit {
                    kind: self.scanner.kind,
                    pos: self.pos(),
                    value: self.scanner.segment(),
                };
                self.next();
                Some(lit)
            }
            _ => None,
        };

        let line_comment = self.line_comment_index; // when no semic literal
        let pos = if let Some(pos) = self.semi() {
            pos
        } else {
            Pos::zero()
        };

        let line_comment = if line_comment.is_some() {
            line_comment
        } else {
            self.line_comment_index // after read the semic
        };
        Ok((
            Fields {
                doc,
                line_comment,
                idents,
                typ,
                tag,
            },
            pos,
        ))
    }

    fn interface(&mut self) -> Result<InterfaceType<'a>> {
        let pos = self.get_pos_then_next();
        let lbrace = self.want_tok_pos(Token::Lbrace)?;

        let mut elems = vec![];
        loop {
            let elem = match self.tok() {
                Token::Name => {
                    let ident = self.name()?;
                    let tok = self.tok();
                    if tok == Token::Lparen {
                        let lparen = self.want_tok_pos(Token::Lparen)?;
                        let sign = self.func_sign(lparen)?;
                        InterfaceElem::MethodElem(MethodElem { name: ident, sign })
                    } else {
                        let type_name = if tok == Token::Dot {
                            let dot = self.get_pos_then_next();
                            Expression::QualifiedName(Box::new(QualifiedName {
                                pkg: ident,
                                dot,
                                ident: self.name()?,
                            }))
                        } else {
                            Expression::Ident(Box::new(ident))
                        };
                        let typ = if self.tok() == Token::Lbrack {
                            Expression::TypeInstance(Box::new(self.type_instance(type_name)?))
                        } else {
                            type_name
                        };
                        InterfaceElem::TypeElem(TypeElem {
                            term: typ,
                            followers: self.type_or_terms()?,
                        })
                    }
                }
                Token::Rbrace => break,
                _ => InterfaceElem::TypeElem(self.type_elem(None)?),
            };

            let line_comment = self.line_comment_index;
            let semi = match self.tok() {
                Token::Semi => self.get_pos_then_next(),
                Token::Rbrace => Pos::zero(),
                token => {
                    return Err(anyhow!(Error::CannotProcToken {
                        token,
                        context: "interface element"
                    }));
                }
            };
            elems.push(InterfaceElemAndSemi {
                line_comment,
                elem,
                semi,
            });
        }

        let rbrace = self.get_pos_then_next();
        Ok(InterfaceType {
            pos,
            lbrace,
            elems,
            rbrace,
        })
    }

    fn stmt(&mut self) -> Result<AST<'a>> {
        Ok(AST::Stmt(self.try_stmt()?))
    }

    // ExpressionList = Expression { "," Expression } .
    fn expr_list(&mut self) -> Result<Expression<'a>> {
        let mut expr = self.expr()?;
        if self.tok() != Token::Comma {
            return Ok(expr);
        }

        let mut r = vec![];
        while self.tok() == Token::Comma {
            r.push((expr, self.get_pos_then_next()));
            expr = self.expr()?;
        }

        r.push((expr, Pos::zero()));

        Ok(Expression::ListExpr(r))
    }

    fn stmt_list(&mut self) -> Result<Vec<Stmt<'a>>> {
        let mut r = vec![];
        let mut tok = self.tok();
        while tok != Token::EOF
            && tok != Token::Default
            && tok != Token::Rbrace
            && tok != Token::Case
        {
            r.push(self.try_stmt()?);

            // ; or }
            // if let Some(s) = self.semi() {
            //     r.push(Stmt::Semi(s));
            // }
            tok = self.tok();
        }
        Ok(r)
    }

    fn try_stmt(&mut self) -> Result<Stmt<'a>> {
        match self.tok() {
            Token::Name => {
                let lhs = self.expr_list()?;
                if let Expression::Ident(ref label) = lhs {
                    if self.tok() == Token::Colon {
                        return self.try_label_stmt(**label);
                    }
                }
                self.simple_stmt(lhs, Token::EOF)
            }
            Token::Operator | Token::Star => match self.scanner.op {
                Operator::Add
                | Operator::Sub
                | Operator::Mul
                | Operator::And
                | Operator::Xor
                | Operator::Not => {
                    let lhs = self.expr_list()?;
                    self.simple_stmt(lhs, Token::EOF)
                }
                _ => Err(anyhow!(Error::CannotProcOp {
                    op: self.scanner.op,
                    context: "try stmt"
                })),
            },
            Token::Const => Ok(Stmt::ConstDecl(self.var_decl0()?)),
            Token::Var => Ok(Stmt::VarDecl(self.var_decl0()?)),
            Token::Type => Ok(Stmt::TypeDecl(self.type_decl0()?)),
            Token::Lbrace => Ok(Stmt::Block(Box::new(self.block()?))),
            tok @ (Token::Go | Token::Defer) => {
                let pos = self.get_pos_then_next();
                let oper = self.operand()?;
                Ok(Stmt::Call(Box::new(CallStmt {
                    tok: TokenPos { tok, pos },
                    stmt: self.pexpr(oper)?,
                })))
            }
            Token::Return => {
                let pos = self.get_pos_then_next();
                let tok = self.tok();
                let expr = if tok != Token::Semi && tok != Token::EOF && tok != Token::Rbrace {
                    Some(self.expr_list()?)
                } else {
                    None
                };

                Ok(Stmt::Return(Box::new(ReturnStmt { pos, expr })))
            }
            Token::For => self.for_stmt(),
            Token::Select => self.select_stmt(),
            Token::Switch => self.switch_stmt(),
            Token::If => self.if_stmt(),
            tok @ Token::Goto => {
                let pos = self.get_pos_then_next();
                let tok_pos = TokenPos { tok, pos };
                let label = Some(self.name()?);

                Ok(Stmt::Branch(Box::new(BranchStmt {
                    tok: tok_pos,
                    label,
                })))
            }
            tok @ Token::Fallthrough => {
                let pos = self.get_pos_then_next();
                Ok(Stmt::Branch(Box::new(BranchStmt {
                    tok: TokenPos { tok, pos },
                    label: None,
                })))
            }
            tok @ (Token::Break | Token::Continue) => {
                let pos = self.get_pos_then_next();
                let tok_pos = TokenPos { tok, pos };
                let label = if self.tok() == Token::Name {
                    Some(self.name()?)
                } else {
                    None
                };

                Ok(Stmt::Branch(Box::new(BranchStmt {
                    tok: tok_pos,
                    label,
                })))
            }
            Token::Semi => Ok(Stmt::Semi(self.get_pos_then_next())),
            _ => {
                let lhs = self.expr_list()?;
                self.simple_stmt(lhs, Token::EOF)
            }
        }
    }

    fn for_stmt(&mut self) -> Result<Stmt<'a>> {
        let out = self.xnest;
        self.xnest = -1;

        let pos = self.get_pos_then_next();
        let opt = self.for_option()?;
        expected_token(self.tok(), Token::Lbrace)?;
        self.xnest = out;

        let body = self.block()?;

        Ok(Stmt::For(Box::new(ForStmt { pos, opt, body })))
    }

    fn for_option(&mut self) -> Result<ForOption<'a>> {
        if self.tok() == Token::Range {
            return Ok(ForOption::Range(RangeClause {
                value: None,
                range: self.get_pos_then_next(),
                expr: self.expr()?,
            }));
        }

        if self.tok() == Token::Semi {
            return Ok(ForOption::ForClause(self.for_clause(Stmt::Empty)?));
        }

        if self.tok() == Token::Lbrace {
            return Ok(ForOption::Empty);
        }

        let expr = self.expr_list()?;
        match self.tok() {
            Token::Lbrace => Ok(ForOption::Cond(expr)),
            Token::IncOp => {
                let name = self.scanner.lit;
                let pos = self.get_pos_then_next();
                Ok(ForOption::ForClause(self.for_clause(Stmt::IncDec(
                    Box::new(IncDecStmt {
                        expr,
                        op: OpLitPos { pos, name },
                    }),
                ))?))
            }
            tok @ (Token::Assign | Token::Define) => {
                let op_lit = self.scanner.segment();
                let pos = self.get_pos_then_next();
                if self.tok() == Token::Range {
                    let range = self.get_pos_then_next();
                    Ok(ForOption::Range(RangeClause {
                        value: Some((expr, TokenPos { pos, tok })),
                        range,
                        expr: self.expr()?,
                    }))
                } else {
                    let rhs = self.expr_list()?;
                    let init = Stmt::Assign(Box::new(AssignStmt {
                        expr,
                        op: OpLitPos { name: op_lit, pos },
                        value: rhs,
                    }));
                    Ok(ForOption::ForClause(self.for_clause(init)?))
                }
            }
            tok => Err(anyhow!(Error::CannotProcToken {
                token: tok,
                context: "for option"
            })),
        }
    }

    fn for_clause(&mut self, init: Stmt<'a>) -> Result<ForClause<'a>> {
        let semi0 = self.get_pos_then_next();
        let (cond, semi1) = if self.tok() == Token::Semi {
            (None, self.get_pos_then_next())
        } else {
            (Some(self.expr()?), self.want_tok_pos(Token::Semi)?)
        };
        let post = if self.tok() == Token::Lbrace {
            Stmt::Empty
        } else {
            self.try_stmt()?
        };
        return Ok(ForClause {
            init,
            semi0,
            cond,
            semi1,
            post,
        });
    }

    fn select_stmt(&mut self) -> Result<Stmt<'a>> {
        let pos = self.get_pos_then_next();
        let lbrace = self.want_tok_pos(Token::Lbrace)?;

        let mut body = vec![];
        loop {
            match self.tok() {
                tok @ (Token::Case | Token::Default) => {
                    let pos = self.get_pos_then_next();
                    let comm = if tok == Token::Case {
                        (self.try_stmt()?, TokenPos { tok, pos })
                    } else {
                        (Stmt::Empty, TokenPos { tok, pos })
                    };
                    let colon = self.want_tok_pos(Token::Colon)?;
                    let stmts = self.stmt_list()?;
                    body.push(CommClause {
                        comm,
                        colon,
                        body: stmts,
                    })
                }
                _ => break,
            }
        }

        let rbrace = self.want_tok_pos(Token::Rbrace)?;

        Ok(Stmt::Select(Box::new(SelectStmt {
            pos,
            lbrace,
            body,
            rbrace,
        })))
    }

    fn switch_stmt(&mut self) -> Result<Stmt<'a>> {
        let out = self.xnest;
        self.xnest = -1;

        let pos = self.get_pos_then_next();
        let (init, tag) = self.switch_init_tag()?;

        self.xnest = out;

        let lbrace = self.want_tok_pos(Token::Lbrace)?;
        let mut body = vec![];
        loop {
            match self.tok() {
                tok @ (Token::Case | Token::Default) => {
                    let tok_pos = TokenPos {
                        tok,
                        pos: self.get_pos_then_next(),
                    };
                    let expr = if tok == Token::Case {
                        Some(self.expr_list()?)
                    } else {
                        None
                    };
                    let colon = self.want_tok_pos(Token::Colon)?;
                    let stmts = self.stmt_list()?;
                    body.push(CaseCause {
                        tok_pos,
                        expr,
                        colon,
                        body: stmts,
                    })
                }
                _ => break,
            }
        }
        let rbrace = self.want_tok_pos(Token::Rbrace)?;

        Ok(Stmt::Switch(Box::new(SwitchStmt {
            pos,
            init,
            tag,
            lbrace,
            body,
            rbrace,
        })))
    }

    fn switch_init_tag(&mut self) -> Result<(Option<(Stmt<'a>, Pos)>, Option<Expression<'a>>)> {
        if self.tok() == Token::Lbrace {
            // switch {}
            return Ok((None, None));
        }

        let stmt = if self.tok() == Token::Semi {
            Stmt::Empty
        } else {
            let init = self.expr_list()?;
            if self.tok() == Token::Lbrace {
                // switch expr {}
                return Ok((None, Some(init)));
            }

            let stmt = self.simple_stmt(init, Token::Switch)?;
            if self.tok() == Token::Lbrace {
                // switch stmt {}
                return Ok((Some((stmt, Pos::zero())), None));
            }
            stmt
        };

        let semi = self.want_tok_pos(Token::Semi)?;
        if self.tok() == Token::Lbrace {
            // switch stmt; {}
            return Ok((Some((stmt, semi)), None));
        }

        // switch init; tag {}
        let expr = self.expr()?;
        let Stmt::Expr(tag) = self.simple_stmt(expr, Token::Switch)? else {
            return Err(anyhow!("switch tag need expr"));
        };
        Ok((Some((stmt, semi)), Some(tag)))
    }

    fn if_stmt(&mut self) -> Result<Stmt<'a>> {
        let out = self.xnest;
        self.xnest = -1;

        let pos = self.get_pos_then_next();
        let (init, cond) = if self.tok() == Token::Semi {
            let semi = self.want_tok_pos(Token::Semi)?;
            (Some((Stmt::Empty, semi)), self.expr()?)
        } else {
            let init = self.expr_list()?;
            if self.tok() == Token::Lbrace {
                (None, init)
            } else {
                let stmt = self.simple_stmt(init, Token::EOF)?;
                let semi = self.want_tok_pos(Token::Semi)?;
                (Some((stmt, semi)), self.expr()?)
            }
        };

        self.xnest = out;
        expected_token(self.tok(), Token::Lbrace)?;
        let block = self.block()?;

        let r#else = if self.tok() == Token::Else {
            let pos = self.get_pos_then_next();
            let stmt = match self.tok() {
                Token::Lbrace => Stmt::Block(Box::new(self.block()?)),
                Token::If => self.if_stmt()?,
                tok => {
                    return Err(anyhow!(Error::CannotProcToken {
                        token: tok,
                        context: "else"
                    }));
                }
            };
            Some((pos, stmt))
        } else {
            None
        };
        Ok(Stmt::If(Box::new(IfStmt {
            pos,
            init,
            cond,
            block,
            r#else,
        })))
    }

    fn try_label_stmt(&mut self, label: Identifier<'a>) -> Result<Stmt<'a>> {
        let colon = self.get_pos_then_next();
        let stmt = Some(if self.tok() == Token::Rbrace {
            Stmt::Empty
        } else {
            self.try_stmt()?
        });

        Ok(Stmt::Label(Box::new(LabelStmt { label, colon, stmt })))
    }

    fn simple_stmt(&mut self, lhs: Expression<'a>, keyword: Token) -> Result<Stmt<'a>> {
        match self.tok() {
            Token::AssignOp => Ok(Stmt::Assign(Box::new(AssignStmt {
                expr: lhs,
                op: OpLitPos {
                    name: self.scanner.lit,
                    pos: self.get_pos_then_next(),
                },
                value: self.expr()?,
            }))),
            Token::IncOp => {
                let name = self.scanner.lit;
                let pos = self.get_pos_then_next();
                Ok(Stmt::IncDec(Box::new(IncDecStmt {
                    expr: lhs,
                    op: OpLitPos { pos, name },
                })))
            }
            Token::Arrow => Ok(Stmt::Send(Box::new(SendStmt {
                chan: lhs,
                arrow: self.get_pos_then_next(),
                value: self.expr()?,
            }))),
            tok @ (Token::Assign | Token::Define) => {
                let name = self.scanner.lit;
                let pos = self.get_pos_then_next();
                let rhs = self.expr_list()?;

                match (lhs, rhs) {
                    (Expression::Ident(id), Expression::TypeSwitchGuard(ts))
                        if tok == Token::Define && keyword == Token::Switch =>
                    {
                        Ok(Stmt::Expr(Expression::TypeSwitchGuard(Box::new(
                            TypeSwitchGuard {
                                lhs: Some((*id, pos)),
                                ..*ts
                            },
                        ))))
                    }
                    (Expression::Ident(id), Expression::TypeSwitchGuard(ts)) => {
                        Ok(Stmt::Assign(Box::new(AssignStmt {
                            expr: Expression::Ident(id),
                            op: OpLitPos { name, pos },
                            value: Expression::TypeSwitchGuard(ts),
                        })))
                    }
                    (Expression::Ident(id), rhs) => Ok(Stmt::Assign(Box::new(AssignStmt {
                        expr: Expression::Ident(id),
                        op: OpLitPos { name, pos },
                        value: rhs,
                    }))),
                    (lhs, rhs) => Ok(Stmt::Assign(Box::new(AssignStmt {
                        expr: lhs,
                        op: OpLitPos { name, pos },
                        value: rhs,
                    }))),
                }
            }
            _ => Ok(Stmt::Expr(lhs)), // expression stmt
        }
    }

    #[inline(always)]
    fn get_pos_then_next(&mut self) -> Pos {
        let pos = self.pos();
        self.next();
        pos
    }

    fn operand(&mut self) -> Result<Expression<'a>> {
        match self.tok() {
            Token::Name => Ok(Expression::Ident(Box::new(self.name()?))),
            Token::Literal => Ok(Expression::BasicLit(Box::new(BasicLit {
                kind: self.scanner.kind,
                value: self.scanner.segment(),
                pos: self.get_pos_then_next(),
            }))),
            Token::Lparen => {
                let lparen = self.get_pos_then_next();
                self.xnest += 1;
                let expr = self.expr()?;
                self.xnest -= 1;
                let rparen = self.want_tok_pos(Token::Rparen)?;
                Ok(Expression::ParenedExpr(Box::new(ParenedExpr {
                    lparen,
                    rparen,
                    expr,
                })))
            }
            Token::Func => {
                let func = self.get_pos_then_next();
                let lparen = self.want_tok_pos(Token::Lparen)?;
                let sign = self.func_sign(lparen)?;
                if self.tok() == Token::Lbrace {
                    self.xnest += 1;
                    let body = self.block()?;
                    self.xnest -= 1;
                    Ok(Expression::FuncLit(Box::new(FuncLit { func, sign, body })))
                } else {
                    Ok(Expression::FuncType(Box::new(FuncType {
                        func,
                        params: sign.params,
                        ret: sign.ret,
                    })))
                }
            }
            Token::Lbrack | Token::Map | Token::Struct | Token::Chan | Token::Interface => {
                Ok(self.r#type(self.tok())?)
            }
            // t => panic!("{}", t),
            t => Err(anyhow!(Error::CannotProcToken {
                token: t,
                context: "operand"
            })),
        }
    }

    fn append_slice_index(&mut self, indice: &mut Vec<Expression<'a>>) -> Result<()> {
        let colon = Expression::Token(Box::new(TokenPos {
            tok: Token::Colon,
            pos: self.want_tok_pos(Token::Colon)?,
        }));
        indice.push(colon);

        let tok = self.tok();
        if tok != Token::Colon && tok != Token::Rbrack {
            indice.push(self.expr()?);
        }

        if self.tok() == Token::Colon {
            indice.push(Expression::Token(Box::new(TokenPos {
                tok: Token::Colon,
                pos: self.get_pos_then_next(),
            })));
            if tok != Token::Rbrack {
                indice.push(self.expr()?);
            }
        }

        Ok(())
    }

    fn pexpr(&mut self, x: Expression<'a>) -> Result<Expression<'a>> {
        let mut x = x;
        loop {
            match self.tok() {
                Token::Dot => {
                    let dot = self.pos();
                    match self.next() {
                        Token::Name => {
                            x = Expression::Selector(Box::new(Selector {
                                pexpr: x,
                                dot,
                                name: self.name()?,
                            }))
                        }
                        Token::Lparen => {
                            let lparen = self.get_pos_then_next();
                            if self.tok() == Token::Type {
                                let typ = self.get_pos_then_next();
                                let rparen = self.want_tok_pos(Token::Rparen)?;
                                x = Expression::TypeSwitchGuard(Box::new(TypeSwitchGuard {
                                    lhs: None,
                                    x,
                                    dot,
                                    lparen,
                                    typ,
                                    rparen,
                                }))
                            } else {
                                let typ = self.expr()?;
                                let rparen = self.want_tok_pos(Token::Rparen)?;
                                x = Expression::TypeAssert(Box::new(TypeAssert {
                                    pexpr: x,
                                    dot,
                                    lparen,
                                    typ,
                                    rparen,
                                }))
                            }
                        }
                        t => panic!("pexpr:{}", t),
                        // t => {
                        //     return Err(anyhow!(Error::CannotProcToken {
                        //         token: t,
                        //         context: "pexpr dot"
                        //     }));
                        // }
                    }
                }
                Token::Lbrack => {
                    let lbrack = self.get_pos_then_next();

                    // try parse index expr
                    if self.tok() != Token::Colon {
                        let mut indices = self.expr()?;
                        if self.tok() == Token::Comma {
                            indices = Expression::ListExpr(self.type_list(indices)?);
                        }

                        if self.tok() == Token::Rbrack {
                            x = Expression::Index(Box::new(Index {
                                lbrack,
                                rbrack: self.want_tok_pos(Token::Rbrack)?,
                                indices,
                                expr: x,
                            }));
                        } else {
                            expected_token(self.tok(), Token::Colon)?;
                            let mut indice = vec![indices];
                            self.append_slice_index(&mut indice)?;
                            x = Expression::SliceExpr(Box::new(SliceExpr {
                                lbrack,
                                rbrack: self.want_tok_pos(Token::Rbrack)?,
                                expr: x,
                                indice,
                            }))
                        }
                    } else {
                        self.xnest += 1;
                        let mut indice = vec![];
                        self.append_slice_index(&mut indice)?;
                        x = Expression::SliceExpr(Box::new(SliceExpr {
                            lbrack,
                            rbrack: self.want_tok_pos(Token::Rbrack)?,
                            expr: x,
                            indice,
                        }));
                        self.xnest -= 1;
                    }
                }
                Token::Lparen => {
                    let lparen = self.get_pos_then_next();
                    let mut args = vec![];
                    self.xnest += 1;
                    while self.tok() != Token::Rparen {
                        let mut expr = self.expr()?;
                        match self.tok() {
                            tok @ Token::Comma => args.push((
                                expr,
                                TokenPos {
                                    tok,
                                    pos: self.get_pos_then_next(),
                                },
                            )),
                            Token::DotDotDot => {
                                expr = Expression::DotDotDotArgs(Box::new(DotDotDotArgs {
                                    expr,
                                    dotdotdot: self.get_pos_then_next(),
                                }));

                                match self.tok() {
                                    tok @ (Token::Comma | Token::Rparen) => {
                                        args.push((
                                            expr,
                                            TokenPos {
                                                tok,
                                                pos: if tok == Token::Comma {
                                                    self.get_pos_then_next()
                                                } else {
                                                    self.pos()
                                                },
                                            },
                                        ));
                                    }
                                    tok => {
                                        return Err(anyhow!(Error::CannotProcToken {
                                            token: tok,
                                            context: "after ... in the CallExpr",
                                        }));
                                    }
                                }
                            }
                            tok @ Token::Rparen => args.push((
                                expr,
                                TokenPos {
                                    tok,
                                    pos: self.pos(),
                                },
                            )),
                            tok => {
                                return Err(anyhow!(Error::CannotProcToken {
                                    token: tok,
                                    context: "CallExpr",
                                }));
                            }
                        }
                    }
                    self.xnest -= 1;
                    let rparen = self.want_tok_pos(Token::Rparen)?;
                    x = Expression::CallExpr(Box::new(CallExpr {
                        pexpr: x,
                        lparen,
                        args,
                        rparen,
                    }))
                }
                Token::Lbrace => {
                    let is_complit = match Self::unparen(&x) {
                        Expression::Selector(_) | Expression::Ident(_) => self.xnest >= 0,
                        Expression::Index(_) => self.xnest >= 0 && !Self::is_value(&x),
                        Expression::ArrayType(_)
                        | Expression::SliceType(_)
                        | Expression::MapType(_)
                        | Expression::Struct(_) => true,
                        _ => false,
                    };

                    if !is_complit {
                        break Ok(x);
                    }
                    x = Expression::CompositeLit(Box::new(CompositeLit {
                        typ: x,
                        value: self.literal_value()?,
                    }))
                }
                _ => break Ok(x), // NOTE: unknown the current tok, maybe lost
            }
        }
    }

    fn unparen<'b>(mut x: &'b Expression<'a>) -> &'b Expression<'a> {
        while let Expression::ParenedExpr(y) = x {
            x = &y.expr
        }
        x
    }

    fn is_value(x: &Expression<'a>) -> bool {
        match x {
            Expression::BasicLit(_)
            | Expression::CompositeLit(_)
            | Expression::FuncLit(_)
            | Expression::SliceExpr(_)
            | Expression::TypeAssert(_)
            | Expression::TypeSwitchGuard(_)
            | Expression::CallExpr(_) => true,
            Expression::Operation(x) => x.op.op != Operator::Mul || x.y.is_none(), // *T maybe a type
            Expression::ParenedExpr(x) => Self::is_value(&(**x).expr),
            Expression::Index(x) => Self::is_value(&x.expr) || Self::is_value(&x.indices),
            _ => false,
        }
    }

    fn type_list(&mut self, x: Expression<'a>) -> Result<ListExpr<'a>> {
        let is_comma = self.tok() == Token::Comma;
        let mut list = vec![(
            x,
            if is_comma {
                self.get_pos_then_next()
            } else {
                Pos::zero()
            },
        )];

        if is_comma {
            while self.tok() != Token::Rbrack {
                let typ = self.r#type(self.tok())?;
                list.push((
                    typ,
                    if self.tok() == Token::Comma {
                        self.get_pos_then_next()
                    } else {
                        Pos { col: 0, lineno: 0 }
                    },
                ));
            }
        }

        Ok(list)
    }

    fn expr(&mut self) -> Result<Expression<'a>> {
        let x = self.unary_expr()?;
        self.binary_expr(x, Precedence::None)
    }

    fn binary_expr(&mut self, x: Expression<'a>, prec: Precedence) -> Result<Expression<'a>> {
        let mut x = x;
        while (self.tok() == Token::Operator || self.tok() == Token::Star)
            && self.scanner.prec > prec
        {
            let tprec = self.scanner.prec;
            let op = OperatorPos {
                op: self.scanner.op,
                pos: self.get_pos_then_next(),
            };
            let tx = self.unary_expr()?;
            x = Expression::Operation(Box::new(Operation {
                op,
                x,
                y: Some(self.binary_expr(tx, tprec)?),
            }));
        }
        Ok(x)
    }

    fn unary_expr(&mut self) -> Result<Expression<'a>> {
        match self.tok() {
            Token::Operator | Token::Star => match self.scanner.op {
                op @ (Operator::Mul
                | Operator::Add
                | Operator::Sub
                | Operator::Not
                | Operator::Xor
                | Operator::And
                | Operator::Tilde) => Ok(Expression::Operation(Box::new(Operation {
                    op: OperatorPos {
                        op,
                        pos: self.get_pos_then_next(),
                    },
                    x: self.unary_expr()?,
                    y: None,
                }))),
                op => Err(anyhow!(Error::CannotProcOp {
                    op: op,
                    context: "unary_expr operator"
                })),
            },
            Token::Arrow => {
                let pos = self.get_pos_then_next();
                let mut x = self.unary_expr()?;

                // The <- operator associates with the leftmost chan possible:
                // chan<- chan int    // same as chan<- (chan int)
                // chan<- <-chan int  // same as chan<- (<-chan int)
                // <-chan <-chan int  // same as <-chan (<-chan int)
                // chan (<-chan int)
                Ok(if let Expression::ChanType(ref mut ch) = x {
                    Self::update_chandir_send_to_recv(ch, pos);
                    x
                } else {
                    Expression::Operation(Box::new(Operation {
                        op: OperatorPos {
                            op: Operator::Recv,
                            pos,
                        },
                        x,
                        y: None,
                    }))
                })
            }
            _ => {
                let expr = self.operand()?;
                Ok(self.pexpr(expr)?)
            }
        }
    }

    fn update_chandir_send_to_recv(ch: &mut ChanType<'a>, pos: Pos) {
        let dir = ch.dir;

        ch.ch = pos;
        ch.dir = ChanDir::Recv(pos);

        if let Expression::ChanType(ref mut ch) = ch.typ {
            Self::update_chandir_send_to_recv(ch, if let ChanDir::Send(p) = dir { p } else { pos });
        }
    }

    #[inline]
    fn want_tok_pos(&mut self, tok: Token) -> Result<Pos> {
        expected_token(self.tok(), tok)?;
        let pos = self.pos();
        self.next();
        Ok(pos)
    }

    fn literal_value(&mut self) -> Result<LiteralValue<'a>> {
        self.xnest += 1;
        let lbrace = self.want_tok_pos(Token::Lbrace)?;
        let mut elem_list = vec![];
        while self.tok() != Token::Rbrace {
            let elem = self.keyed_elem()?;
            match self.tok() {
                Token::Rbrace => {
                    elem_list.push((Expression::KeyedElement(Box::new(elem)), Pos::zero()));
                    break;
                }
                Token::Comma => {
                    elem_list.push((
                        Expression::KeyedElement(Box::new(elem)),
                        self.get_pos_then_next(),
                    ));
                    if self.tok() == Token::Rbrace {
                        break;
                    }
                }
                t => return Err(anyhow!("literal value cannot process tok {}", t)),
            }
        }
        let rbrace = self.want_tok_pos(Token::Rbrace)?;
        self.xnest -= 1;
        Ok(LiteralValue {
            lbrace,
            rbrace,
            elem_list,
        })
    }

    fn keyed_elem(&mut self) -> Result<KeyedElement<'a>> {
        let x = self.elem(self.tok())?;
        Ok(if self.tok() == Token::Colon {
            let colon = self.get_pos_then_next();
            KeyedElement {
                key_and_colon: Some((x, colon)),
                elem: self.elem(self.tok())?,
            }
        } else {
            KeyedElement {
                key_and_colon: None,
                elem: x,
            }
        })
    }

    fn elem(&mut self, tok: Token) -> Result<Expression<'a>> {
        if tok == Token::Lbrace {
            Ok(Expression::LiteralValue(Box::new(self.literal_value()?)))
        } else {
            Ok(self.expr()?)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{ast::*, parser::LiteralKind};
    use pretty_assertions::assert_eq;

    use super::{Error, Operator, Parser, Token};
    use crate::source::Block;

    macro_rules! test_ast_ok {
        ($source:literal $(,$exp_ast:expr)+ $(,)?) => {
            let mut parser = Parser::new(Block::new($source), errh);
            match parser.parse() {
                Err(err) => panic!("{}", err),
                Ok(p) => assert_eq!(vec![$($exp_ast,)+], p),
            }
        };
    }

    macro_rules! test_ast_err {
        ($source:literal, $err:expr) => {
            let mut parser = Parser::new(Block::new($source), errh);
            match parser.parse() {
                Err(err) => assert_eq!(err.downcast_ref::<Error>().unwrap(), &$err),
                Ok(_) => panic!("bad"),
            }
        };
    }

    fn errh(_lineno: usize, _col: usize, _lit: &str) {}

    #[test]
    fn package_smoke() {
        test_ast_ok!(
            "package test",
            AST::PackageClause(Box::new(PackageClause {
                name: Identifier {
                    name: "test",
                    pos: Pos { col: 9, lineno: 1 }
                },
                pos: Pos { col: 1, lineno: 1 },
            })),
        );
    }

    #[test]
    fn package_should_return_err_when_no_name() {
        test_ast_err!(
            "package ",
            Error::UnExpectedToken {
                expected: Token::Name,
                found: Token::EOF,
            }
        );
    }

    #[test]
    fn import_one_line() {
        test_ast_ok!(
            r#"import x "abc""#,
            AST::ImportDecl(Box::new(ImportDecl {
                pos: Pos { lineno: 1, col: 1 },
                import: ImportOption::Spec(ImportSpec {
                    doc: None,
                    line_comment: None,
                    pkg_name: Some(Identifier {
                        name: "x",
                        pos: Pos { lineno: 1, col: 8 },
                    }),
                    path: BasicLit {
                        kind: LiteralKind::String,
                        value: "\"abc\"",
                        pos: Pos { lineno: 1, col: 10 },
                    }
                }),
            })),
        );
        test_ast_ok!(
            r#"import . "abc""#,
            AST::ImportDecl(Box::new(ImportDecl {
                pos: Pos { lineno: 1, col: 1 },
                import: ImportOption::Spec(ImportSpec {
                    doc: None,
                    line_comment: None,
                    pkg_name: Some(Identifier {
                        name: ".",
                        pos: Pos { lineno: 1, col: 8 },
                    }),
                    path: BasicLit {
                        kind: LiteralKind::String,
                        value: "\"abc\"",
                        pos: Pos { lineno: 1, col: 10 },
                    }
                }),
            })),
        );
    }

    #[test]
    fn import_one_line_no_path() {
        test_ast_err!(
            r#"import "#,
            Error::UnExpectedToken {
                found: Token::EOF,
                expected: Token::Literal,
            }
        );
        test_ast_err!(
            r#"import x"#,
            Error::UnExpectedToken {
                expected: Token::Literal,
                found: Token::Semi,
            }
        );
    }

    #[test]
    fn import_one_line_not_string_literal() {
        test_ast_err!(r#"import 1"#, Error::ExpectedLitKind(LiteralKind::String));
    }

    #[test]
    fn import_group() {
        test_ast_ok!(
            r#"import (
 x "abc"
 y "def")"#,
            AST::ImportDecl(Box::new(ImportDecl {
                pos: Pos { lineno: 1, col: 1 },
                import: ImportOption::Group(ImportGroup {
                    lparen: Pos { lineno: 1, col: 8 },
                    rparen: Pos { lineno: 3, col: 9 },
                    specs: vec![
                        ImportSpec {
                            doc: None,
                            line_comment: None,
                            pkg_name: Some(Identifier {
                                name: "x",
                                pos: Pos { lineno: 2, col: 2 },
                            }),
                            path: BasicLit {
                                kind: LiteralKind::String,
                                value: "\"abc\"",
                                pos: Pos { lineno: 2, col: 4 },
                            },
                        },
                        ImportSpec {
                            doc: None,
                            line_comment: None,
                            pkg_name: Some(Identifier {
                                name: "y",
                                pos: Pos { lineno: 3, col: 2 },
                            }),
                            path: BasicLit {
                                kind: LiteralKind::String,
                                value: "\"def\"",
                                pos: Pos { lineno: 3, col: 4 },
                            },
                        }
                    ],
                }),
            })),
        );
    }

    #[test]
    fn type_struct_spec() {
        test_ast_ok!(
            r#"type s struct {
  x int `json:"X, omitty"`
  y,  z string;
}"#,
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { lineno: 1, col: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    params: None,
                    ident: Identifier {
                        name: "s",
                        pos: Pos { lineno: 1, col: 6 }
                    },
                    typ: (Expression::Struct(Box::new(StructType {
                        lbrace: Pos { lineno: 1, col: 15 },
                        rbrace: Pos { lineno: 4, col: 1 },
                        pos: Pos { lineno: 1, col: 8 },
                        field_decls: vec![
                            (
                                FieldDeclOption::Fields(Fields {
                                    doc: None,
                                    line_comment: None,
                                    idents: IdentifierList {
                                        ident: Identifier {
                                            name: "x",
                                            pos: Pos { col: 3, lineno: 2 },
                                        },
                                        followers: vec![],
                                    },
                                    typ: Expression::Ident(Box::new(Identifier {
                                        name: "int",
                                        pos: Pos { lineno: 2, col: 5 }
                                    }),),
                                    tag: Some(BasicLit {
                                        kind: LiteralKind::String,

                                        pos: Pos { lineno: 2, col: 9 },
                                        value: r#"`json:"X, omitty"`"#,
                                    }),
                                }),
                                Pos::zero()
                            ),
                            (
                                FieldDeclOption::Fields(Fields {
                                    doc: None,
                                    line_comment: None,
                                    idents: IdentifierList {
                                        ident: Identifier {
                                            name: "y",
                                            pos: Pos { col: 3, lineno: 3 },
                                        },
                                        followers: vec![CommaAndIdentifier {
                                            comma: Pos { col: 4, lineno: 3 },
                                            ident: Identifier {
                                                name: "z",
                                                pos: Pos { col: 7, lineno: 3 },
                                            }
                                        }],
                                    },
                                    typ: Expression::Ident(Box::new(Identifier {
                                        name: "string",
                                        pos: Pos { lineno: 3, col: 9 }
                                    })),
                                    tag: None,
                                }),
                                Pos { col: 15, lineno: 3 }
                            )
                        ],
                    }))),
                })),
            })),
        );
    }

    #[test]
    fn type_alias() {
        test_ast_ok!(
            "type x = y",
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::AliasDecl(AliasDecl {
                    params: None,
                    ident: Identifier {
                        pos: Pos { col: 6, lineno: 1 },
                        name: "x",
                    },
                    eq: Pos { col: 8, lineno: 1 },
                    typ: Expression::Ident(Box::new(Identifier {
                        name: "y",
                        pos: Pos { col: 10, lineno: 1 }
                    })),
                }))
            })),
        );
    }

    #[test]
    fn type_alias_args() {
        test_ast_ok!(
            "type x = y[int,struct{}]",
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::AliasDecl(AliasDecl {
                    params: None,
                    ident: Identifier {
                        pos: Pos { col: 6, lineno: 1 },
                        name: "x",
                    },
                    eq: Pos { col: 8, lineno: 1 },
                    typ: Expression::TypeInstance(Box::new(TypeInstance {
                        lbrack: Pos { col: 11, lineno: 1 },
                        rbrack: Pos { col: 24, lineno: 1 },
                        type_name: Expression::Ident(Box::new(Identifier {
                            name: "y",
                            pos: Pos { col: 10, lineno: 1 }
                        })),
                        type_args: vec![
                            (
                                Expression::Ident(Box::new(Identifier {
                                    name: "int",
                                    pos: Pos { col: 12, lineno: 1 }
                                })),
                                Pos { col: 15, lineno: 1 }
                            ),
                            (
                                Expression::Struct(Box::new(StructType {
                                    field_decls: vec![],
                                    lbrace: Pos { lineno: 1, col: 22 },
                                    rbrace: Pos { lineno: 1, col: 23 },
                                    pos: Pos { col: 16, lineno: 1 }
                                })),
                                Pos { col: 0, lineno: 0 }
                            )
                        ]
                    })),
                }))
            })),
        );
    }

    #[test]
    fn type_alias_of_qualified_dent() {
        test_ast_ok!(
            "type x = y.z",
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::AliasDecl(AliasDecl {
                    params: None,
                    ident: Identifier {
                        pos: Pos { col: 6, lineno: 1 },
                        name: "x",
                    },
                    eq: Pos { col: 8, lineno: 1 },
                    typ: Expression::QualifiedName(Box::new(QualifiedName {
                        pkg: Identifier {
                            name: "y",
                            pos: Pos { col: 10, lineno: 1 }
                        },
                        dot: Pos { col: 11, lineno: 1 },
                        ident: Identifier {
                            name: "z",
                            pos: Pos { col: 12, lineno: 1 }
                        },
                    }),)
                }))
            })),
        );
    }

    #[test]
    fn embedded_field() {
        test_ast_ok!(
            r#"type s struct {
T `json:"t"`
*T
y.z
X[int]
X.Y[int]
}"#,
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { lineno: 1, col: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    params: None,
                    ident: Identifier {
                        name: "s",
                        pos: Pos { lineno: 1, col: 6 }
                    },
                    typ: Expression::Struct(Box::new(StructType {
                        lbrace: Pos { lineno: 1, col: 15 },
                        rbrace: Pos { lineno: 7, col: 1 },
                        pos: Pos { lineno: 1, col: 8 },
                        field_decls: vec![
                            (
                                FieldDeclOption::Embedded(EmbeddedFieldTypeField {
                                    doc: None,
                                    line_comment: None,
                                    tag: Some(BasicLit {
                                        kind: LiteralKind::String,
                                        pos: Pos { col: 3, lineno: 2 },
                                        value: r#"`json:"t"`"#,
                                    }),
                                    typ: Expression::Ident(Box::new(Identifier {
                                        name: "T",
                                        pos: Pos { col: 1, lineno: 2 }
                                    }),)
                                },),
                                Pos::zero()
                            ),
                            (
                                FieldDeclOption::Embedded(EmbeddedFieldTypeField {
                                    doc: None,
                                    line_comment: None,
                                    tag: None,
                                    typ: Expression::Operation(Box::new(Operation {
                                        op: OperatorPos {
                                            pos: Pos { col: 1, lineno: 3 },
                                            op: Operator::Mul,
                                        },
                                        x: Expression::Ident(Box::new(Identifier {
                                            name: "T",
                                            pos: Pos { col: 2, lineno: 3 }
                                        }),),
                                        y: None,
                                    })),
                                }),
                                Pos::zero()
                            ),
                            (
                                FieldDeclOption::Embedded(EmbeddedFieldTypeField {
                                    doc: None,
                                    line_comment: None,
                                    tag: None,
                                    typ: Expression::QualifiedName(Box::new(QualifiedName {
                                        pkg: Identifier {
                                            name: "y",
                                            pos: Pos { col: 1, lineno: 4 }
                                        },
                                        dot: Pos { col: 2, lineno: 4 },
                                        ident: Identifier {
                                            name: "z",
                                            pos: Pos { col: 3, lineno: 4 }
                                        }
                                    })),
                                }),
                                Pos::zero()
                            ),
                            (
                                FieldDeclOption::Embedded(EmbeddedFieldTypeField {
                                    doc: None,
                                    line_comment: None,
                                    tag: None,
                                    typ: Expression::Index(Box::new(Index {
                                        lbrack: Pos { col: 2, lineno: 5 },
                                        rbrack: Pos { col: 6, lineno: 5 },
                                        indices: Expression::ListExpr(vec![(
                                            Expression::Ident(Box::new(Identifier {
                                                name: "int",
                                                pos: Pos { col: 3, lineno: 5 }
                                            })),
                                            Pos::zero()
                                        )]),
                                        expr: Expression::Ident(Box::new(Identifier {
                                            name: "X",
                                            pos: Pos { col: 1, lineno: 5 }
                                        }))
                                    })),
                                }),
                                Pos::zero()
                            ),
                            (
                                FieldDeclOption::Embedded(EmbeddedFieldTypeField {
                                    doc: None,
                                    line_comment: None,
                                    tag: None,
                                    typ: Expression::TypeInstance(Box::new(TypeInstance {
                                        lbrack: Pos { col: 4, lineno: 6 },
                                        rbrack: Pos { col: 8, lineno: 6 },
                                        type_args: vec![(
                                            Expression::Ident(Box::new(Identifier {
                                                name: "int",
                                                pos: Pos { col: 5, lineno: 6 }
                                            })),
                                            Pos { col: 0, lineno: 0 }
                                        )],
                                        type_name: Expression::QualifiedName(Box::new(
                                            QualifiedName {
                                                pkg: Identifier {
                                                    name: "X",
                                                    pos: Pos { col: 1, lineno: 6 }
                                                },
                                                dot: Pos { col: 2, lineno: 6 },
                                                ident: Identifier {
                                                    name: "Y",
                                                    pos: Pos { col: 3, lineno: 6 }
                                                }
                                            }
                                        ))
                                    })),
                                }),
                                Pos::zero()
                            ),
                        ],
                    })),
                })),
            })),
        );
    }

    #[test]
    fn pointer_type() {
        test_ast_ok!(
            "type p *int",
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    params: None,
                    ident: Identifier {
                        name: "p",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    typ: Expression::Operation(Box::new(Operation {
                        op: OperatorPos {
                            pos: Pos { col: 8, lineno: 1 },
                            op: Operator::Mul
                        },
                        x: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 9, lineno: 1 }
                        })),
                        y: None,
                    }))
                })),
            })),
        );
    }

    #[test]
    fn slice_type() {
        test_ast_ok!(
            "type p [ ]int",
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    params: None,
                    ident: Identifier {
                        name: "p",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    typ: Expression::SliceType(Box::new(SliceType {
                        lbrack: Pos { col: 8, lineno: 1 },
                        rbrack: Pos { col: 10, lineno: 1 },
                        typ: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 11, lineno: 1 }
                        }))
                    }))
                })),
            })),
        );
    }

    #[test]
    fn map_test() {
        test_ast_ok!(
            "type p map[s]int",
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    params: None,
                    ident: Identifier {
                        name: "p",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    typ: Expression::MapType(Box::new(MapType {
                        pos: Pos { col: 8, lineno: 1 },
                        lbrack: Pos { col: 11, lineno: 1 },
                        rbrack: Pos { col: 13, lineno: 1 },
                        key_type: Expression::Ident(Box::new(Identifier {
                            name: "s",
                            pos: Pos { col: 12, lineno: 1 }
                        })),
                        ele_type: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 14, lineno: 1 }
                        }))
                    }))
                })),
            })),
        );
    }

    #[test]
    fn channel_test() {
        test_ast_ok!(
            r#"type (
p chan int;
p <-chan int
p chan<- int
)"#,
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: TypeDeclOption::Group(TypeGroup {
                    lparen: Pos { col: 6, lineno: 1 },
                    rparen: Pos { col: 1, lineno: 5 },
                    specs: vec![
                        (
                            TypeSpec::TypeDef(TypeDef {
                                doc: None,
                                params: None,
                                ident: Identifier {
                                    name: "p",
                                    pos: Pos { col: 1, lineno: 2 }
                                },
                                typ: Expression::ChanType(Box::new(ChanType {
                                    ch: Pos { col: 3, lineno: 2 },
                                    dir: ChanDir::Both,
                                    typ: Expression::Ident(Box::new(Identifier {
                                        name: "int",
                                        pos: Pos { col: 8, lineno: 2 }
                                    }))
                                }))
                            }),
                            Pos { col: 11, lineno: 2 },
                        ),
                        (
                            TypeSpec::TypeDef(TypeDef {
                                doc: None,
                                params: None,
                                ident: Identifier {
                                    name: "p",
                                    pos: Pos { col: 1, lineno: 3 }
                                },
                                typ: Expression::ChanType(Box::new(ChanType {
                                    ch: Pos { col: 5, lineno: 3 },
                                    dir: ChanDir::Recv(Pos { col: 3, lineno: 3 }),
                                    typ: Expression::Ident(Box::new(Identifier {
                                        name: "int",
                                        pos: Pos { col: 10, lineno: 3 }
                                    }))
                                }))
                            }),
                            Pos::zero()
                        ),
                        (
                            TypeSpec::TypeDef(TypeDef {
                                doc: None,
                                params: None,
                                ident: Identifier {
                                    name: "p",
                                    pos: Pos { col: 1, lineno: 4 }
                                },
                                typ: Expression::ChanType(Box::new(ChanType {
                                    ch: Pos { col: 3, lineno: 4 },
                                    dir: ChanDir::Send(Pos { col: 7, lineno: 4 }),
                                    typ: Expression::Ident(Box::new(Identifier {
                                        name: "int",
                                        pos: Pos { col: 10, lineno: 4 }
                                    }))
                                }))
                            }),
                            Pos::zero()
                        ),
                    ],
                    doc: None,
                }),
            })),
        );
    }

    #[test]
    fn type_params() {
        test_ast_ok!(
            "type s[X, Y any,
Z int,
] struct {
    t X
}",
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { lineno: 1, col: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    ident: Identifier {
                        name: "s",
                        pos: Pos { lineno: 1, col: 6 }
                    },
                    params: Some(TypeParameters {
                        lbrack: Pos { col: 7, lineno: 1 },
                        rbrack: Pos { col: 1, lineno: 3 },
                        param_list: vec![
                            (
                                TypeParamDecl {
                                    idents: IdentifierList {
                                        ident: Identifier {
                                            name: "X",
                                            pos: Pos { col: 8, lineno: 1 }
                                        },
                                        followers: vec![CommaAndIdentifier {
                                            comma: Pos { col: 9, lineno: 1 },
                                            ident: Identifier {
                                                name: "Y",
                                                pos: Pos { col: 11, lineno: 1 }
                                            }
                                        }]
                                    },
                                    type_constraint: TypeElem {
                                        term: Expression::Ident(Box::new(Identifier {
                                            name: "any",
                                            pos: Pos { col: 13, lineno: 1 }
                                        })),
                                        followers: vec![]
                                    }
                                },
                                Pos { col: 16, lineno: 1 }
                            ),
                            (
                                TypeParamDecl {
                                    idents: IdentifierList {
                                        ident: Identifier {
                                            pos: Pos { col: 1, lineno: 2 },
                                            name: "Z",
                                        },
                                        followers: vec![]
                                    },
                                    type_constraint: TypeElem {
                                        followers: vec![],
                                        term: Expression::Ident(Box::new(Identifier {
                                            name: "int",
                                            pos: Pos { col: 3, lineno: 2 }
                                        }))
                                    },
                                },
                                Pos { col: 6, lineno: 2 },
                            )
                        ],
                    }),
                    typ: Expression::Struct(Box::new(StructType {
                        lbrace: Pos { lineno: 3, col: 10 },
                        rbrace: Pos { lineno: 5, col: 1 },
                        pos: Pos { lineno: 3, col: 3 },
                        field_decls: vec![(
                            FieldDeclOption::Fields(Fields {
                                doc: None,
                                line_comment: None,
                                idents: IdentifierList {
                                    ident: Identifier {
                                        name: "t",
                                        pos: Pos { col: 5, lineno: 4 },
                                    },
                                    followers: vec![],
                                },
                                typ: Expression::Ident(Box::new(Identifier {
                                    name: "X",
                                    pos: Pos { lineno: 4, col: 7 }
                                }),),
                                tag: None,
                            }),
                            Pos::zero()
                        ),],
                    })),
                })),
            })),
        );

        test_ast_ok!(
            "type S[a*int,]int",
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    ident: Identifier {
                        name: "S",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    params: Some(TypeParameters {
                        lbrack: Pos { col: 7, lineno: 1 },
                        rbrack: Pos { col: 14, lineno: 1 },
                        param_list: vec![(
                            TypeParamDecl {
                                idents: IdentifierList {
                                    ident: Identifier {
                                        name: "a",
                                        pos: Pos { col: 8, lineno: 1 }
                                    },
                                    followers: vec![]
                                },
                                type_constraint: TypeElem {
                                    term: Expression::Operation(Box::new(Operation {
                                        op: OperatorPos {
                                            op: Operator::Mul,
                                            pos: Pos { col: 9, lineno: 1 }
                                        },
                                        x: Expression::Ident(Box::new(Identifier {
                                            name: "int",
                                            pos: Pos { col: 10, lineno: 1 }
                                        })),
                                        y: None
                                    })),
                                    followers: vec![],
                                }
                            },
                            Pos { col: 13, lineno: 1 },
                        )],
                    }),
                    typ: Expression::Ident(Box::new(Identifier {
                        name: "int",
                        pos: Pos { col: 15, lineno: 1 }
                    }))
                }))
            }))
        );
    }

    #[test]
    fn func_generic_type_param() {
        test_ast_ok!(
            "type f func(p.n[int],int)",
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    ident: Identifier {
                        name: "f",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    params: None,
                    typ: Expression::FuncType(Box::new(FuncType {
                        func: Pos { col: 8, lineno: 1 },
                        params: Params {
                            lparen: Pos { col: 12, lineno: 1 },
                            list: Some(vec![
                                (
                                    ParamDecl {
                                        idents: None,
                                        dotdotdot: None,
                                        typ: Expression::TypeInstance(Box::new(TypeInstance {
                                            lbrack: Pos { col: 16, lineno: 1 },
                                            rbrack: Pos { col: 20, lineno: 1 },
                                            type_args: vec![(
                                                Expression::Ident(Box::new(Identifier {
                                                    name: "int",
                                                    pos: Pos { col: 17, lineno: 1 }
                                                })),
                                                Pos::zero()
                                            ),],
                                            type_name: Expression::QualifiedName(Box::new(
                                                QualifiedName {
                                                    pkg: Identifier {
                                                        name: "p",
                                                        pos: Pos { col: 13, lineno: 1 }
                                                    },
                                                    dot: Pos { col: 14, lineno: 1 },
                                                    ident: Identifier {
                                                        name: "n",
                                                        pos: Pos { col: 15, lineno: 1 }
                                                    }
                                                }
                                            ),)
                                        }))
                                    },
                                    Pos { col: 21, lineno: 1 }
                                ),
                                (
                                    ParamDecl {
                                        idents: None,
                                        dotdotdot: None,
                                        typ: Expression::Ident(Box::new(Identifier {
                                            name: "int",
                                            pos: Pos { col: 22, lineno: 1 }
                                        }))
                                    },
                                    Pos::zero()
                                )
                            ]),
                            rparen: Pos { col: 25, lineno: 1 }
                        },
                        ret: None
                    }))
                }))
            }))
        );
    }

    #[test]
    fn func_params() {
        test_ast_ok!(
            "type f func(p[int],int)",
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    ident: Identifier {
                        name: "f",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    params: None,
                    typ: Expression::FuncType(Box::new(FuncType {
                        func: Pos { col: 8, lineno: 1 },
                        params: Params {
                            lparen: Pos { col: 12, lineno: 1 },
                            list: Some(vec![
                                (
                                    ParamDecl {
                                        idents: None,
                                        dotdotdot: None,
                                        typ: Expression::Index(Box::new(Index {
                                            lbrack: Pos { col: 14, lineno: 1 },
                                            rbrack: Pos { col: 18, lineno: 1 },
                                            indices: Expression::ListExpr(vec![(
                                                Expression::Ident(Box::new(Identifier {
                                                    name: "int",
                                                    pos: Pos { col: 15, lineno: 1 }
                                                })),
                                                Pos::zero()
                                            ),]),
                                            expr: Expression::Ident(Box::new(Identifier {
                                                name: "p",
                                                pos: Pos { col: 13, lineno: 1 }
                                            }))
                                        }))
                                    },
                                    Pos { col: 19, lineno: 1 }
                                ),
                                (
                                    ParamDecl {
                                        idents: None,
                                        dotdotdot: None,
                                        typ: Expression::Ident(Box::new(Identifier {
                                            name: "int",
                                            pos: Pos { col: 20, lineno: 1 }
                                        }))
                                    },
                                    Pos::zero()
                                )
                            ]),
                            rparen: Pos { col: 23, lineno: 1 }
                        },
                        ret: None
                    }))
                }))
            }))
        );
    }

    #[test]
    fn func_body() {
        test_ast_ok!(
            "func f(){+x}",
            AST::FuncDecl(Box::new(FuncDecl {
                doc: None,
                func: Pos { col: 1, lineno: 1 },
                name: Identifier {
                    name: "f",
                    pos: Pos { col: 6, lineno: 1 }
                },
                type_params: None,
                sign: FuncSign {
                    params: Params {
                        lparen: Pos { col: 7, lineno: 1 },
                        list: None,
                        rparen: Pos { col: 8, lineno: 1 }
                    },
                    ret: None
                },
                body: Some(BlockStmt {
                    lbrace: Pos { col: 9, lineno: 1 },
                    stmts: vec![Stmt::Expr(Expression::Operation(Box::new(Operation {
                        op: OperatorPos {
                            op: Operator::Add,
                            pos: Pos { col: 10, lineno: 1 }
                        },
                        x: Expression::Ident(Box::new(Identifier {
                            name: "x",
                            pos: Pos { col: 11, lineno: 1 }
                        })),
                        y: None
                    })))],
                    rbrace: Pos { col: 12, lineno: 1 }
                })
            }))
        );
    }

    #[test]
    fn function_type_params() {
        test_ast_ok!(
            "func f[T*int]()",
            AST::FuncDecl(Box::new(FuncDecl {
                doc: None,
                func: Pos { col: 1, lineno: 1 },
                name: Identifier {
                    name: "f",
                    pos: Pos { col: 6, lineno: 1 }
                },
                type_params: Some(TypeParameters {
                    lbrack: Pos { col: 7, lineno: 1 },
                    param_list: vec![(
                        TypeParamDecl {
                            idents: IdentifierList {
                                ident: Identifier {
                                    name: "T",
                                    pos: Pos { col: 8, lineno: 1 }
                                },
                                followers: vec![]
                            },
                            type_constraint: TypeElem {
                                term: Expression::Operation(Box::new(Operation {
                                    op: OperatorPos {
                                        op: Operator::Mul,
                                        pos: Pos { col: 9, lineno: 1 },
                                    },
                                    x: Expression::Ident(Box::new(Identifier {
                                        name: "int",
                                        pos: Pos { col: 10, lineno: 1 }
                                    })),
                                    y: None
                                })),
                                followers: vec![]
                            }
                        },
                        Pos::zero()
                    )],
                    rbrack: Pos { col: 13, lineno: 1 }
                }),
                sign: FuncSign {
                    params: Params {
                        lparen: Pos { col: 14, lineno: 1 },
                        list: None,
                        rparen: Pos { col: 15, lineno: 1 }
                    },
                    ret: None
                },
                body: None,
            }))
        );
    }

    #[test]
    fn function_signature_smoke() {
        test_ast_ok!(
            r#"type f func(a,b T,c int,
d ...T,)(x, y int,
)"#,
            AST::TypeDecl(Box::new(TypeDecl {
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    ident: Identifier {
                        name: "f",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    params: None,
                    typ: Expression::FuncType(Box::new(FuncType {
                        func: Pos { col: 8, lineno: 1 },
                        params: Params {
                            lparen: Pos { col: 12, lineno: 1 },
                            rparen: Pos { col: 8, lineno: 2 },
                            list: Some(vec![
                                (
                                    ParamDecl {
                                        idents: Some(IdentifierList {
                                            ident: Identifier {
                                                name: "a",
                                                pos: Pos { col: 13, lineno: 1 }
                                            },
                                            followers: vec![CommaAndIdentifier {
                                                comma: Pos { col: 14, lineno: 1 },
                                                ident: Identifier {
                                                    name: "b",
                                                    pos: Pos { col: 15, lineno: 1 }
                                                }
                                            }]
                                        }),
                                        dotdotdot: None,
                                        typ: Expression::Ident(Box::new(Identifier {
                                            name: "T",
                                            pos: Pos { col: 17, lineno: 1 }
                                        }))
                                    },
                                    Pos { col: 18, lineno: 1 }
                                ),
                                (
                                    ParamDecl {
                                        idents: Some(IdentifierList {
                                            ident: Identifier {
                                                name: "c",
                                                pos: Pos { col: 19, lineno: 1 }
                                            },
                                            followers: vec![]
                                        }),
                                        dotdotdot: None,
                                        typ: Expression::Ident(Box::new(Identifier {
                                            name: "int",
                                            pos: Pos { col: 21, lineno: 1 }
                                        }))
                                    },
                                    Pos { col: 24, lineno: 1 }
                                ),
                                (
                                    ParamDecl {
                                        idents: Some(IdentifierList {
                                            ident: Identifier {
                                                name: "d",
                                                pos: Pos { col: 1, lineno: 2 },
                                            },
                                            followers: vec![],
                                        },),
                                        dotdotdot: Some(Pos { col: 3, lineno: 2 },),
                                        typ: Expression::Ident(Box::new(Identifier {
                                            name: "T",
                                            pos: Pos { col: 6, lineno: 2 },
                                        })),
                                    },
                                    Pos { col: 7, lineno: 2 },
                                )
                            ])
                        },
                        ret: Some(FuncResult::Params(Params {
                            lparen: Pos { col: 9, lineno: 2 },
                            rparen: Pos { col: 1, lineno: 3 },
                            list: Some(vec![(
                                ParamDecl {
                                    idents: Some(IdentifierList {
                                        ident: Identifier {
                                            name: "x",
                                            pos: Pos { col: 10, lineno: 2 }
                                        },
                                        followers: vec![CommaAndIdentifier {
                                            comma: Pos { col: 11, lineno: 2 },
                                            ident: Identifier {
                                                name: "y",
                                                pos: Pos { col: 13, lineno: 2 }
                                            }
                                        }]
                                    }),
                                    dotdotdot: None,
                                    typ: Expression::Ident(Box::new(Identifier {
                                        name: "int",
                                        pos: Pos { col: 15, lineno: 2 }
                                    }))
                                },
                                Pos { col: 18, lineno: 2 }
                            )])
                        })),
                    }))
                })),
                pos: Pos { col: 1, lineno: 1 }
            })),
        );
    }

    #[test]
    fn func_def_simple() {
        test_ast_ok!(
            "type f func ()",
            AST::TypeDecl(Box::new(TypeDecl {
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    ident: Identifier {
                        name: "f",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    params: None,
                    typ: Expression::FuncType(Box::new(FuncType {
                        func: Pos { col: 8, lineno: 1 },
                        params: Params {
                            lparen: Pos { col: 13, lineno: 1 },
                            list: None,
                            rparen: Pos { col: 14, lineno: 1 }
                        },
                        ret: None
                    }))
                })),
                pos: Pos { col: 1, lineno: 1 }
            }))
        );
    }

    #[test]
    fn type_array() {
        test_ast_ok!(
            "type a [2]int",
            AST::TypeDecl(Box::new(TypeDecl {
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    ident: Identifier {
                        name: "a",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    params: None,
                    typ: Expression::ArrayType(Box::new(ArrayType {
                        lbrack: Pos { col: 8, lineno: 1 },
                        len: Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "2",
                            pos: Pos { col: 9, lineno: 1 }
                        })),
                        rbrack: Pos { col: 10, lineno: 1 },
                        typ: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 11, lineno: 1 }
                        }))
                    }))
                })),
                pos: Pos { col: 1, lineno: 1 }
            }))
        );
        test_ast_ok!(
            "type a [l]int",
            AST::TypeDecl(Box::new(TypeDecl {
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    ident: Identifier {
                        name: "a",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    params: None,
                    typ: Expression::ArrayType(Box::new(ArrayType {
                        lbrack: Pos { col: 8, lineno: 1 },
                        len: Expression::Ident(Box::new(Identifier {
                            name: "l",
                            pos: Pos { col: 9, lineno: 1 }
                        })),
                        rbrack: Pos { col: 10, lineno: 1 },
                        typ: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 11, lineno: 1 }
                        }))
                    }))
                })),
                pos: Pos { col: 1, lineno: 1 }
            }))
        );
    }

    #[test]
    fn interface_method() {
        test_ast_ok!(
            "type i interface{
f(int)int
}",
            AST::TypeDecl(Box::new(TypeDecl {
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    ident: Identifier {
                        name: "i",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    params: None,
                    typ: (Expression::InterfaceType(Box::new(InterfaceType {
                        pos: Pos { col: 8, lineno: 1 },
                        lbrace: Pos { col: 17, lineno: 1 },
                        elems: vec![InterfaceElemAndSemi {
                            line_comment: None,
                            semi: Pos { col: 0, lineno: 3 },
                            elem: InterfaceElem::MethodElem(MethodElem {
                                name: Identifier {
                                    name: "f",
                                    pos: Pos { col: 1, lineno: 2 }
                                },
                                sign: FuncSign {
                                    params: Params {
                                        lparen: Pos { col: 2, lineno: 2 },
                                        list: Some(vec![(
                                            ParamDecl {
                                                idents: None,
                                                dotdotdot: None,
                                                typ: Expression::Ident(Box::new(Identifier {
                                                    name: "int",
                                                    pos: Pos { col: 3, lineno: 2 }
                                                }))
                                            },
                                            Pos::zero()
                                        )]),
                                        rparen: Pos { col: 6, lineno: 2 }
                                    },
                                    ret: Some(FuncResult::Type(Expression::Ident(Box::new(
                                        Identifier {
                                            name: "int",
                                            pos: Pos { col: 7, lineno: 2 }
                                        }
                                    ))))
                                }
                            })
                        }],
                        rbrace: Pos { col: 1, lineno: 3 }
                    })))
                })),
                pos: Pos { col: 1, lineno: 1 }
            }))
        );
    }

    #[test]
    fn interface_type_elem() {
        test_ast_ok!(
            "type i interface{
int|~i
}",
            AST::TypeDecl(Box::new(TypeDecl {
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    ident: Identifier {
                        name: "i",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    params: None,
                    typ: Expression::InterfaceType(Box::new(InterfaceType {
                        pos: Pos { col: 8, lineno: 1 },
                        lbrace: Pos { col: 17, lineno: 1 },
                        elems: vec![InterfaceElemAndSemi {
                            line_comment: None,
                            semi: Pos { col: 0, lineno: 3 },
                            elem: InterfaceElem::TypeElem(TypeElem {
                                followers: vec![OrTypeTerm {
                                    or: Pos { col: 4, lineno: 2 },
                                    term: Expression::UnderlyingType(Box::new(UnderlyingType {
                                        pos: Pos { col: 5, lineno: 2 },
                                        typ: Expression::Ident(Box::new(Identifier {
                                            name: "i",
                                            pos: Pos { col: 6, lineno: 2 }
                                        }))
                                    }))
                                }],
                                term: Expression::Ident(Box::new(Identifier {
                                    name: "int",
                                    pos: Pos { col: 1, lineno: 2 }
                                }))
                            })
                        },],
                        rbrace: Pos { col: 1, lineno: 3 }
                    }))
                })),
                pos: Pos { col: 1, lineno: 1 }
            }))
        );
    }

    #[test]
    fn parened_type() {
        test_ast_ok!(
            "type i (int)",
            AST::TypeDecl(Box::new(TypeDecl {
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    ident: Identifier {
                        name: "i",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    params: None,
                    typ: Expression::ParenedExpr(Box::new(ParenedExpr {
                        lparen: Pos { col: 8, lineno: 1 },
                        rparen: Pos { col: 12, lineno: 1 },
                        expr: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 9, lineno: 1 }
                        }))
                    }))
                })),
                pos: Pos { col: 1, lineno: 1 }
            }))
        );
    }

    #[test]
    fn basic_lit() {
        test_ast_ok!(
            "1;0.1",
            AST::Stmt(Stmt::Expr(Expression::BasicLit(Box::new(BasicLit {
                kind: LiteralKind::Int,
                value: "1",
                pos: Pos { col: 1, lineno: 1 }
            })))),
            AST::Semi(Pos { col: 2, lineno: 1 }),
            AST::Stmt(Stmt::Expr(Expression::BasicLit(Box::new(BasicLit {
                kind: LiteralKind::Float,
                value: "0.1",
                pos: Pos { col: 3, lineno: 1 }
            }))))
        );
    }

    #[test]
    fn composite_struct_literal() {
        test_ast_ok!(
            "struct{x int}{1}",
            AST::Stmt(Stmt::Expr(Expression::CompositeLit(Box::new(
                CompositeLit {
                    typ: Expression::Struct(Box::new(StructType {
                        lbrace: Pos { lineno: 1, col: 7 },
                        rbrace: Pos { lineno: 1, col: 13 },
                        field_decls: vec![(
                            FieldDeclOption::Fields(Fields {
                                doc: None,
                                line_comment: None,
                                idents: IdentifierList {
                                    ident: Identifier {
                                        name: "x",
                                        pos: Pos { col: 8, lineno: 1 }
                                    },
                                    followers: vec![]
                                },
                                typ: Expression::Ident(Box::new(Identifier {
                                    name: "int",
                                    pos: Pos { col: 10, lineno: 1 }
                                })),
                                tag: None
                            }),
                            Pos::zero()
                        )],
                        pos: Pos { col: 1, lineno: 1 }
                    })),
                    value: LiteralValue {
                        lbrace: Pos { col: 14, lineno: 1 },
                        rbrace: Pos { col: 16, lineno: 1 },
                        elem_list: vec![(
                            Expression::KeyedElement(Box::new(KeyedElement {
                                key_and_colon: None,
                                elem: Expression::BasicLit(Box::new(BasicLit {
                                    kind: LiteralKind::Int,
                                    value: "1",
                                    pos: Pos { col: 15, lineno: 1 }
                                })),
                            })),
                            Pos { col: 0, lineno: 0 }
                        )],
                    },
                }
            ),)))
        );
    }

    #[test]
    fn composite_struct_literal_with_key() {
        test_ast_ok!(
            "struct{x int}{x:1,}",
            AST::Stmt(Stmt::Expr(Expression::CompositeLit(Box::new(
                CompositeLit {
                    typ: Expression::Struct(Box::new(StructType {
                        lbrace: Pos { lineno: 1, col: 7 },
                        rbrace: Pos { lineno: 1, col: 13 },
                        field_decls: vec![(
                            FieldDeclOption::Fields(Fields {
                                doc: None,
                                line_comment: None,
                                idents: IdentifierList {
                                    ident: Identifier {
                                        name: "x",
                                        pos: Pos { col: 8, lineno: 1 }
                                    },
                                    followers: vec![]
                                },
                                typ: Expression::Ident(Box::new(Identifier {
                                    name: "int",
                                    pos: Pos { col: 10, lineno: 1 }
                                })),
                                tag: None
                            }),
                            Pos::zero()
                        )],
                        pos: Pos { col: 1, lineno: 1 }
                    })),
                    value: LiteralValue {
                        lbrace: Pos { col: 14, lineno: 1 },
                        rbrace: Pos { col: 19, lineno: 1 },
                        elem_list: vec![(
                            Expression::KeyedElement(Box::new(KeyedElement {
                                key_and_colon: Some((
                                    Expression::Ident(Box::new(Identifier {
                                        name: "x",
                                        pos: Pos { col: 15, lineno: 1 }
                                    })),
                                    Pos { col: 16, lineno: 1 }
                                )),
                                elem: Expression::BasicLit(Box::new(BasicLit {
                                    kind: LiteralKind::Int,
                                    value: "1",
                                    pos: Pos { col: 17, lineno: 1 }
                                })),
                            })),
                            Pos { col: 18, lineno: 1 }
                        )]
                    },
                }
            ),)))
        );
    }

    #[test]
    fn composite_map() {
        test_ast_ok!(
            "map[int]int{1:1}",
            AST::Stmt(Stmt::Expr(Expression::CompositeLit(Box::new(
                CompositeLit {
                    typ: Expression::MapType(Box::new(MapType {
                        pos: Pos { col: 1, lineno: 1 },
                        lbrack: Pos { col: 4, lineno: 1 },
                        rbrack: Pos { col: 8, lineno: 1 },
                        key_type: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 5, lineno: 1 }
                        })),
                        ele_type: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 9, lineno: 1 }
                        }))
                    })),
                    value: LiteralValue {
                        lbrace: Pos { col: 12, lineno: 1 },
                        rbrace: Pos { col: 16, lineno: 1 },
                        elem_list: vec![(
                            Expression::KeyedElement(Box::new(KeyedElement {
                                key_and_colon: Some((
                                    Expression::BasicLit(Box::new(BasicLit {
                                        kind: LiteralKind::Int,
                                        value: "1",
                                        pos: Pos { col: 13, lineno: 1 }
                                    })),
                                    Pos { col: 14, lineno: 1 }
                                )),
                                elem: Expression::BasicLit(Box::new(BasicLit {
                                    kind: LiteralKind::Int,
                                    value: "1",
                                    pos: Pos { col: 15, lineno: 1 }
                                })),
                            })),
                            Pos::zero(),
                        )]
                    },
                }
            ),)))
        );
    }

    #[test]
    fn composite_slice() {
        test_ast_ok!(
            "[]int{1,1}",
            AST::Stmt(Stmt::Expr(Expression::CompositeLit(Box::new(
                CompositeLit {
                    typ: Expression::SliceType(Box::new(SliceType {
                        lbrack: Pos { col: 1, lineno: 1 },
                        rbrack: Pos { col: 2, lineno: 1 },
                        typ: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 3, lineno: 1 }
                        }))
                    })),
                    value: LiteralValue {
                        lbrace: Pos { col: 6, lineno: 1 },
                        rbrace: Pos { col: 10, lineno: 1 },
                        elem_list: vec![
                            (
                                Expression::KeyedElement(Box::new(KeyedElement {
                                    key_and_colon: None,
                                    elem: Expression::BasicLit(Box::new(BasicLit {
                                        kind: LiteralKind::Int,
                                        value: "1",
                                        pos: Pos { col: 7, lineno: 1 }
                                    })),
                                })),
                                Pos { col: 8, lineno: 1 }
                            ),
                            (
                                Expression::KeyedElement(Box::new(KeyedElement {
                                    key_and_colon: None,
                                    elem: Expression::BasicLit(Box::new(BasicLit {
                                        kind: LiteralKind::Int,
                                        value: "1",
                                        pos: Pos { col: 9, lineno: 1 }
                                    })),
                                })),
                                Pos::zero()
                            )
                        ]
                    },
                }
            ),)))
        );
    }

    #[test]
    fn composite_array_auto_slice() {
        test_ast_ok!(
            "[...]int{1,2}",
            AST::Stmt(Stmt::Expr(Expression::CompositeLit(Box::new(
                CompositeLit {
                    typ: Expression::ArrayType(Box::new(ArrayType {
                        lbrack: Pos { col: 1, lineno: 1 },
                        len: Expression::Token(Box::new(TokenPos {
                            pos: Pos { col: 2, lineno: 1 },
                            tok: Token::DotDotDot,
                        })),
                        rbrack: Pos { col: 5, lineno: 1 },
                        typ: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 6, lineno: 1 }
                        }),)
                    })),
                    value: LiteralValue {
                        lbrace: Pos { col: 9, lineno: 1 },
                        rbrace: Pos { col: 13, lineno: 1 },
                        elem_list: vec![
                            (
                                Expression::KeyedElement(Box::new(KeyedElement {
                                    key_and_colon: None,
                                    elem: Expression::BasicLit(Box::new(BasicLit {
                                        kind: LiteralKind::Int,
                                        value: "1",
                                        pos: Pos { col: 10, lineno: 1 }
                                    })),
                                }),),
                                Pos { col: 11, lineno: 1 }
                            ),
                            (
                                Expression::KeyedElement(Box::new(KeyedElement {
                                    key_and_colon: None,
                                    elem: Expression::BasicLit(Box::new(BasicLit {
                                        kind: LiteralKind::Int,
                                        value: "2",
                                        pos: Pos { col: 12, lineno: 1 }
                                    })),
                                })),
                                Pos::zero()
                            )
                        ]
                    },
                }
            ),)))
        );
    }

    #[test]
    fn composite_generic_instance_struct() {
        test_ast_ok!(
            "t[int]{x:1}",
            AST::Stmt(Stmt::Expr(Expression::CompositeLit(Box::new(
                CompositeLit {
                    typ: Expression::Index(Box::new(Index {
                        lbrack: Pos { col: 2, lineno: 1 },
                        rbrack: Pos { col: 6, lineno: 1 },
                        indices: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 3, lineno: 1 }
                        })),
                        expr: Expression::Ident(Box::new(Identifier {
                            name: "t",
                            pos: Pos { col: 1, lineno: 1 }
                        }))
                    })),
                    value: LiteralValue {
                        lbrace: Pos { col: 7, lineno: 1 },
                        rbrace: Pos { col: 11, lineno: 1 },
                        elem_list: vec![(
                            Expression::KeyedElement(Box::new(KeyedElement {
                                key_and_colon: Some((
                                    Expression::Ident(Box::new(Identifier {
                                        name: "x",
                                        pos: Pos { col: 8, lineno: 1 }
                                    })),
                                    Pos { col: 9, lineno: 1 }
                                )),
                                elem: Expression::BasicLit(Box::new(BasicLit {
                                    kind: LiteralKind::Int,
                                    value: "1",
                                    pos: Pos { col: 10, lineno: 1 }
                                })),
                            })),
                            Pos::zero()
                        )]
                    },
                }
            ),)))
        );
    }
    #[test]
    fn composite_array() {
        test_ast_ok!(
            "[1]int{2,}",
            AST::Stmt(Stmt::Expr(Expression::CompositeLit(Box::new(
                CompositeLit {
                    typ: Expression::ArrayType(Box::new(ArrayType {
                        lbrack: Pos { col: 1, lineno: 1 },
                        len: Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "1",
                            pos: Pos { col: 2, lineno: 1 }
                        })),
                        rbrack: Pos { col: 3, lineno: 1 },
                        typ: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 4, lineno: 1 }
                        }),)
                    })),
                    value: LiteralValue {
                        lbrace: Pos { col: 7, lineno: 1 },
                        rbrace: Pos { col: 10, lineno: 1 },
                        elem_list: vec![(
                            Expression::KeyedElement(Box::new(KeyedElement {
                                elem: Expression::BasicLit(Box::new(BasicLit {
                                    kind: LiteralKind::Int,
                                    value: "2",
                                    pos: Pos { col: 8, lineno: 1 }
                                })),
                                key_and_colon: None,
                            })),
                            Pos { col: 9, lineno: 1 },
                        )]
                    },
                }
            ),)))
        );
    }

    #[test]
    fn call_expr() {
        test_ast_ok!(
            "((int))(2,)",
            AST::Stmt(Stmt::Expr(Expression::CallExpr(Box::new(CallExpr {
                pexpr: Expression::ParenedExpr(Box::new(ParenedExpr {
                    lparen: Pos { col: 1, lineno: 1 },
                    rparen: Pos { col: 7, lineno: 1 },
                    expr: Expression::ParenedExpr(Box::new(ParenedExpr {
                        lparen: Pos { col: 2, lineno: 1 },
                        rparen: Pos { col: 6, lineno: 1 },
                        expr: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 3, lineno: 1 }
                        }))
                    }))
                })),
                lparen: Pos { col: 8, lineno: 1 },
                rparen: Pos { col: 11, lineno: 1 },
                args: vec![(
                    Expression::BasicLit(Box::new(BasicLit {
                        kind: LiteralKind::Int,
                        value: "2",
                        pos: Pos { col: 9, lineno: 1 }
                    })),
                    TokenPos {
                        tok: Token::Comma,
                        pos: Pos { col: 10, lineno: 1 }
                    }
                )],
            }))))
        );
        test_ast_ok!(
            "t(2,)",
            AST::Stmt(Stmt::Expr(Expression::CallExpr(Box::new(CallExpr {
                pexpr: Expression::Ident(Box::new(Identifier {
                    name: "t",
                    pos: Pos { col: 1, lineno: 1 }
                })),
                lparen: Pos { col: 2, lineno: 1 },
                rparen: Pos { col: 5, lineno: 1 },
                args: vec![(
                    Expression::BasicLit(Box::new(BasicLit {
                        kind: LiteralKind::Int,
                        value: "2",
                        pos: Pos { col: 3, lineno: 1 }
                    })),
                    TokenPos {
                        tok: Token::Comma,
                        pos: Pos { col: 4, lineno: 1 }
                    }
                )],
            }))))
        );
    }

    #[test]
    fn selector() {
        test_ast_ok!(
            "(S).name",
            AST::Stmt(Stmt::Expr(Expression::Selector(Box::new(Selector {
                pexpr: Expression::ParenedExpr(Box::new(ParenedExpr {
                    lparen: Pos { col: 1, lineno: 1 },
                    rparen: Pos { col: 3, lineno: 1 },
                    expr: Expression::Ident(Box::new(Identifier {
                        name: "S",
                        pos: Pos { col: 2, lineno: 1 }
                    }))
                })),
                dot: Pos { col: 4, lineno: 1 },
                name: Identifier {
                    name: "name",
                    pos: Pos { col: 5, lineno: 1 }
                }
            }))))
        );
        test_ast_ok!(
            "a.b.c.d",
            AST::Stmt(Stmt::Expr(Expression::Selector(Box::new(Selector {
                pexpr: Expression::Selector(Box::new(Selector {
                    pexpr: Expression::Selector(Box::new(Selector {
                        pexpr: Expression::Ident(Box::new(Identifier {
                            name: "a",
                            pos: Pos { col: 1, lineno: 1 }
                        })),
                        dot: Pos { col: 2, lineno: 1 },
                        name: Identifier {
                            name: "b",
                            pos: Pos { col: 3, lineno: 1 }
                        }
                    })),
                    dot: Pos { col: 4, lineno: 1 },
                    name: Identifier {
                        name: "c",
                        pos: Pos { col: 5, lineno: 1 }
                    }
                })),
                dot: Pos { col: 6, lineno: 1 },
                name: Identifier {
                    name: "d",
                    pos: Pos { col: 7, lineno: 1 }
                }
            }))))
        );
    }

    #[test]
    fn type_assert() {
        test_ast_ok!(
            "s.(x)",
            AST::Stmt(Stmt::Expr(Expression::TypeAssert(Box::new(TypeAssert {
                pexpr: Expression::Ident(Box::new(Identifier {
                    name: "s",
                    pos: Pos { col: 1, lineno: 1 }
                })),
                dot: Pos { col: 2, lineno: 1 },
                lparen: Pos { col: 3, lineno: 1 },
                typ: Expression::Ident(Box::new(Identifier {
                    name: "x",
                    pos: Pos { col: 4, lineno: 1 }
                })),
                rparen: Pos { col: 5, lineno: 1 }
            }))))
        );
    }

    #[test]
    fn slice_expr() {
        test_ast_ok!(
            "s[1:2:3]",
            AST::Stmt(Stmt::Expr(Expression::SliceExpr(Box::new(SliceExpr {
                expr: Expression::Ident(Box::new(Identifier {
                    name: "s",
                    pos: Pos { col: 1, lineno: 1 }
                })),
                indice: vec![
                    Expression::BasicLit(Box::new(BasicLit {
                        kind: LiteralKind::Int,
                        value: "1",
                        pos: Pos::new(1, 3)
                    })),
                    Expression::Token(Box::new(TokenPos {
                        tok: Token::Colon,
                        pos: Pos::new(1, 4)
                    })),
                    Expression::BasicLit(Box::new(BasicLit {
                        kind: LiteralKind::Int,
                        value: "2",
                        pos: Pos::new(1, 5)
                    })),
                    Expression::Token(Box::new(TokenPos {
                        tok: Token::Colon,
                        pos: Pos::new(1, 6)
                    })),
                    Expression::BasicLit(Box::new(BasicLit {
                        kind: LiteralKind::Int,
                        value: "3",
                        pos: Pos::new(1, 7)
                    }))
                ],
                lbrack: Pos { col: 2, lineno: 1 },
                rbrack: Pos { col: 8, lineno: 1 }
            }))))
        );
        test_ast_ok!(
            "s[:2:3]",
            AST::Stmt(Stmt::Expr(Expression::SliceExpr(Box::new(SliceExpr {
                expr: Expression::Ident(Box::new(Identifier {
                    name: "s",
                    pos: Pos { col: 1, lineno: 1 }
                })),
                indice: vec![
                    Expression::Token(Box::new(TokenPos {
                        tok: Token::Colon,
                        pos: Pos::new(1, 3)
                    })),
                    Expression::BasicLit(Box::new(BasicLit {
                        kind: LiteralKind::Int,
                        value: "2",
                        pos: Pos::new(1, 4)
                    })),
                    Expression::Token(Box::new(TokenPos {
                        tok: Token::Colon,
                        pos: Pos::new(1, 5)
                    })),
                    Expression::BasicLit(Box::new(BasicLit {
                        kind: LiteralKind::Int,
                        value: "3",
                        pos: Pos::new(1, 6)
                    }))
                ],
                lbrack: Pos { col: 2, lineno: 1 },
                rbrack: Pos { col: 7, lineno: 1 }
            }))))
        );
        test_ast_ok!(
            "s[2:3]",
            AST::Stmt(Stmt::Expr(Expression::SliceExpr(Box::new(SliceExpr {
                expr: Expression::Ident(Box::new(Identifier {
                    name: "s",
                    pos: Pos { col: 1, lineno: 1 }
                })),
                indice: vec![
                    Expression::BasicLit(Box::new(BasicLit {
                        kind: LiteralKind::Int,
                        value: "2",
                        pos: Pos::new(1, 3)
                    })),
                    Expression::Token(Box::new(TokenPos {
                        tok: Token::Colon,
                        pos: Pos::new(1, 4)
                    })),
                    Expression::BasicLit(Box::new(BasicLit {
                        kind: LiteralKind::Int,
                        value: "3",
                        pos: Pos::new(1, 5)
                    }))
                ],
                lbrack: Pos { col: 2, lineno: 1 },
                rbrack: Pos { col: 6, lineno: 1 }
            }))))
        );
        test_ast_ok!(
            "s[:]",
            AST::Stmt(Stmt::Expr(Expression::SliceExpr(Box::new(SliceExpr {
                expr: Expression::Ident(Box::new(Identifier {
                    name: "s",
                    pos: Pos { col: 1, lineno: 1 }
                })),
                indice: vec![Expression::Token(Box::new(TokenPos {
                    tok: Token::Colon,
                    pos: Pos::new(1, 3)
                })),],
                lbrack: Pos { col: 2, lineno: 1 },
                rbrack: Pos { col: 4, lineno: 1 }
            }))))
        );
    }

    #[test]
    fn unary_expr_simple() {
        test_ast_ok!(
            "+x",
            AST::Stmt(Stmt::Expr(Expression::Operation(Box::new(Operation {
                op: OperatorPos {
                    op: Operator::Add,
                    pos: Pos { col: 1, lineno: 1 },
                },
                y: None,
                x: Expression::Ident(Box::new(Identifier {
                    name: "x",
                    pos: Pos { col: 2, lineno: 1 }
                }))
            }))))
        );
        test_ast_ok!(
            "&x",
            AST::Stmt(Stmt::Expr(Expression::Operation(Box::new(Operation {
                op: OperatorPos {
                    op: Operator::And,
                    pos: Pos { col: 1, lineno: 1 },
                },
                x: Expression::Ident(Box::new(Identifier {
                    name: "x",
                    pos: Pos { col: 2, lineno: 1 }
                })),
                y: None,
            }))))
        );
    }

    #[test]
    fn unary_expr_arrow() {
        test_ast_ok!(
            "<-chan<-chan int",
            AST::Stmt(Stmt::Expr(Expression::ChanType(Box::new(ChanType {
                ch: Pos { col: 1, lineno: 1 },
                typ: Expression::ChanType(Box::new(ChanType {
                    ch: Pos { col: 7, lineno: 1 },
                    typ: Expression::Ident(Box::new(Identifier {
                        name: "int",
                        pos: Pos { col: 14, lineno: 1 }
                    })),
                    dir: ChanDir::Recv(Pos { col: 7, lineno: 1 }),
                })),
                dir: ChanDir::Recv(Pos { col: 1, lineno: 1 }),
            }))))
        );
        test_ast_ok!(
            "<-x",
            AST::Stmt(Stmt::Expr(Expression::Operation(Box::new(Operation {
                op: OperatorPos {
                    pos: Pos { col: 1, lineno: 1 },
                    op: Operator::Recv,
                },
                y: None,
                x: Expression::Ident(Box::new(Identifier {
                    name: "x",
                    pos: Pos { col: 3, lineno: 1 }
                }))
            }))))
        );
    }

    #[test]
    fn binary_expr() {
        test_ast_ok!(
            "x*1|1",
            AST::Stmt(Stmt::Expr(Expression::Operation(Box::new(Operation {
                op: OperatorPos {
                    op: Operator::Or,
                    pos: Pos { col: 4, lineno: 1 }
                },
                x: Expression::Operation(Box::new(Operation {
                    op: OperatorPos {
                        op: Operator::Mul,
                        pos: Pos { col: 2, lineno: 1 }
                    },
                    x: Expression::Ident(Box::new(Identifier {
                        name: "x",
                        pos: Pos { col: 1, lineno: 1 }
                    })),
                    y: Some(Expression::BasicLit(Box::new(BasicLit {
                        kind: LiteralKind::Int,
                        value: "1",
                        pos: Pos { col: 3, lineno: 1 }
                    })))
                })),
                y: Some(Expression::BasicLit(Box::new(BasicLit {
                    kind: LiteralKind::Int,
                    value: "1",
                    pos: Pos { col: 5, lineno: 1 }
                })))
            }))))
        );
    }

    #[test]
    fn type_guard() {
        test_ast_ok!(
            "x.(type)",
            AST::Stmt(Stmt::Expr(Expression::TypeSwitchGuard(Box::new(
                TypeSwitchGuard {
                    lhs: None,
                    x: Expression::Ident(Box::new(Identifier {
                        name: "x",
                        pos: Pos { col: 1, lineno: 1 }
                    })),
                    dot: Pos { col: 2, lineno: 1 },
                    lparen: Pos { col: 3, lineno: 1 },
                    typ: Pos { col: 4, lineno: 1 },
                    rparen: Pos { col: 8, lineno: 1 },
                }
            ))))
        );
    }

    #[test]
    fn func_lit() {
        test_ast_ok!(
            "func()int{}()",
            AST::Stmt(Stmt::Expr(Expression::CallExpr(Box::new(CallExpr {
                pexpr: Expression::FuncLit(Box::new(FuncLit {
                    func: Pos { col: 1, lineno: 1 },
                    sign: FuncSign {
                        params: Params {
                            lparen: Pos { col: 5, lineno: 1 },
                            list: None,
                            rparen: Pos { col: 6, lineno: 1 }
                        },
                        ret: Some(FuncResult::Type(Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 7, lineno: 1 }
                        }))))
                    },
                    body: BlockStmt {
                        lbrace: Pos { col: 10, lineno: 1 },
                        stmts: vec![],
                        rbrace: Pos { col: 11, lineno: 1 }
                    }
                })),
                lparen: Pos { col: 12, lineno: 1 },
                args: vec![],
                rparen: Pos { col: 13, lineno: 1 }
            }))))
        );
    }

    #[test]
    fn method_decl() {
        test_ast_ok!(
            "func (a int)f(){}",
            AST::MethodDecl(Box::new(MethodDecl {
                doc: None,
                func: Pos { col: 1, lineno: 1 },
                receiver: Params {
                    lparen: Pos { col: 6, lineno: 1 },
                    list: Some(vec![(
                        ParamDecl {
                            idents: Some(IdentifierList {
                                ident: Identifier {
                                    name: "a",
                                    pos: Pos { col: 7, lineno: 1 }
                                },
                                followers: vec![],
                            }),
                            dotdotdot: None,
                            typ: Expression::Ident(Box::new(Identifier {
                                name: "int",
                                pos: Pos { col: 9, lineno: 1 }
                            }))
                        },
                        Pos::zero()
                    )]),
                    rparen: Pos { col: 12, lineno: 1 }
                },
                name: Identifier {
                    name: "f",
                    pos: Pos { col: 13, lineno: 1 }
                },
                sign: FuncSign {
                    params: Params {
                        lparen: Pos { col: 14, lineno: 1 },
                        list: None,
                        rparen: Pos { col: 15, lineno: 1 }
                    },
                    ret: None
                },
                body: Some(BlockStmt {
                    lbrace: Pos { col: 16, lineno: 1 },
                    stmts: vec![],
                    rbrace: Pos { col: 17, lineno: 1 }
                })
            }))
        );
    }

    #[test]
    fn send_stmt() {
        test_ast_ok!(
            "x<-1",
            AST::Stmt(Stmt::Send(Box::new(SendStmt {
                chan: Expression::Ident(Box::new(Identifier {
                    name: "x",
                    pos: Pos { col: 1, lineno: 1 }
                })),
                arrow: Pos { col: 2, lineno: 1 },
                value: Expression::BasicLit(Box::new(BasicLit {
                    kind: LiteralKind::Int,
                    value: "1",
                    pos: Pos { col: 4, lineno: 1 }
                }))
            })))
        );
    }

    #[test]
    fn inc_dec_stmt() {
        test_ast_ok!(
            "x++",
            AST::Stmt(Stmt::IncDec(Box::new(IncDecStmt {
                expr: Expression::Ident(Box::new(Identifier {
                    name: "x",
                    pos: Pos { col: 1, lineno: 1 }
                })),
                op: OpLitPos {
                    pos: Pos { col: 2, lineno: 1 },
                    name: "++"
                }
            })))
        );
    }

    #[test]
    fn assign_op_stmt() {
        test_ast_ok!(
            "x+=1",
            AST::Stmt(Stmt::Assign(Box::new(AssignStmt {
                expr: Expression::Ident(Box::new(Identifier {
                    name: "x",
                    pos: Pos { col: 1, lineno: 1 }
                })),
                op: OpLitPos {
                    name: "+=",
                    pos: Pos { col: 2, lineno: 1 }
                },
                value: Expression::BasicLit(Box::new(BasicLit {
                    kind: LiteralKind::Int,
                    value: "1",
                    pos: Pos { col: 4, lineno: 1 }
                })),
            })))
        );
    }

    #[test]
    fn assign_stmt() {
        test_ast_ok!(
            "x:=1
a,b=2,3",
            AST::Stmt(Stmt::Assign(Box::new(AssignStmt {
                expr: Expression::Ident(Box::new(Identifier {
                    name: "x",
                    pos: Pos { col: 1, lineno: 1 }
                })),
                op: OpLitPos {
                    name: ":=",
                    pos: Pos { col: 2, lineno: 1 },
                },
                value: Expression::BasicLit(Box::new(BasicLit {
                    kind: LiteralKind::Int,
                    value: "1",
                    pos: Pos { col: 4, lineno: 1 }
                })),
            }))),
            AST::Stmt(Stmt::Assign(Box::new(AssignStmt {
                expr: Expression::ListExpr(vec![
                    (
                        Expression::Ident(Box::new(Identifier {
                            name: "a",
                            pos: Pos { col: 1, lineno: 2 }
                        })),
                        Pos { col: 2, lineno: 2 }
                    ),
                    (
                        Expression::Ident(Box::new(Identifier {
                            name: "b",
                            pos: Pos { col: 3, lineno: 2 }
                        })),
                        Pos::zero(),
                    )
                ]),
                op: OpLitPos {
                    name: "=",
                    pos: Pos { col: 4, lineno: 2 },
                },
                value: Expression::ListExpr(vec![
                    (
                        Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "2",
                            pos: Pos { col: 5, lineno: 2 },
                        })),
                        Pos { col: 6, lineno: 2 },
                    ),
                    (
                        Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "3",
                            pos: Pos { col: 7, lineno: 2 },
                        })),
                        Pos::zero(),
                    )
                ]),
            })))
        );
    }

    #[test]
    fn const_decl() {
        test_ast_ok!(
            "const x,y int=1,2",
            AST::ConstDecl(Box::new(VarDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: VarDeclOption::Spec(VarSpec {
                    doc: None,
                    line_comment: None,
                    ident_list: IdentifierList {
                        ident: Identifier {
                            name: "x",
                            pos: Pos { col: 7, lineno: 1 }
                        },
                        followers: vec![CommaAndIdentifier {
                            comma: Pos { col: 8, lineno: 1 },
                            ident: Identifier {
                                name: "y",
                                pos: Pos { col: 9, lineno: 1 }
                            }
                        }]
                    },
                    typ: Some(Expression::Ident(Box::new(Identifier {
                        name: "int",
                        pos: Pos { col: 11, lineno: 1 }
                    }))),
                    eq: Some(Pos { col: 14, lineno: 1 }),
                    expr_list: Some(Expression::ListExpr(vec![
                        (
                            Expression::BasicLit(Box::new(BasicLit {
                                kind: LiteralKind::Int,
                                value: "1",
                                pos: Pos { col: 15, lineno: 1 }
                            })),
                            Pos { col: 16, lineno: 1 }
                        ),
                        (
                            Expression::BasicLit(Box::new(BasicLit {
                                kind: LiteralKind::Int,
                                value: "2",
                                pos: Pos { col: 17, lineno: 1 }
                            })),
                            Pos::zero(),
                        )
                    ]))
                })
            }))
        );
    }

    #[test]
    fn const_group_decl() {
        test_ast_ok!(
            "const (x,y int=1,2;z)",
            AST::ConstDecl(Box::new(VarDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: VarDeclOption::Group(VarGroup {
                    lparen: Pos { col: 7, lineno: 1 },
                    rparen: Pos { col: 21, lineno: 1 },
                    specs: vec![
                        (
                            VarSpec {
                                doc: None,
                                line_comment: None,
                                ident_list: IdentifierList {
                                    ident: Identifier {
                                        name: "x",
                                        pos: Pos { col: 8, lineno: 1 }
                                    },
                                    followers: vec![CommaAndIdentifier {
                                        comma: Pos { col: 9, lineno: 1 },
                                        ident: Identifier {
                                            name: "y",
                                            pos: Pos { col: 10, lineno: 1 }
                                        }
                                    }]
                                },
                                typ: Some(Expression::Ident(Box::new(Identifier {
                                    name: "int",
                                    pos: Pos { col: 12, lineno: 1 }
                                }))),
                                eq: Some(Pos { col: 15, lineno: 1 }),
                                expr_list: Some(Expression::ListExpr(vec![
                                    (
                                        Expression::BasicLit(Box::new(BasicLit {
                                            kind: LiteralKind::Int,
                                            value: "1",
                                            pos: Pos { col: 16, lineno: 1 }
                                        })),
                                        Pos { col: 17, lineno: 1 }
                                    ),
                                    (
                                        Expression::BasicLit(Box::new(BasicLit {
                                            kind: LiteralKind::Int,
                                            value: "2",
                                            pos: Pos { col: 18, lineno: 1 }
                                        })),
                                        Pos::zero(),
                                    )
                                ]))
                            },
                            Pos { col: 19, lineno: 1 }
                        ),
                        (
                            VarSpec {
                                doc: None,
                                line_comment: None,
                                ident_list: IdentifierList {
                                    ident: Identifier {
                                        name: "z",
                                        pos: Pos { col: 20, lineno: 1 }
                                    },
                                    followers: vec![]
                                },
                                typ: None,
                                eq: None,
                                expr_list: None,
                            },
                            Pos::zero(),
                        )
                    ]
                })
            }))
        );
    }

    #[test]
    fn var_decl() {
        test_ast_ok!(
            "var x,y int=1,2",
            AST::VarDecl(Box::new(VarDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: VarDeclOption::Spec(VarSpec {
                    doc: None,
                    line_comment: None,
                    ident_list: IdentifierList {
                        ident: Identifier {
                            name: "x",
                            pos: Pos { col: 5, lineno: 1 }
                        },
                        followers: vec![CommaAndIdentifier {
                            comma: Pos { col: 6, lineno: 1 },
                            ident: Identifier {
                                name: "y",
                                pos: Pos { col: 7, lineno: 1 }
                            }
                        }]
                    },
                    typ: Some(Expression::Ident(Box::new(Identifier {
                        name: "int",
                        pos: Pos { col: 9, lineno: 1 }
                    }))),
                    eq: Some(Pos { col: 12, lineno: 1 }),
                    expr_list: Some(Expression::ListExpr(vec![
                        (
                            Expression::BasicLit(Box::new(BasicLit {
                                kind: LiteralKind::Int,
                                value: "1",
                                pos: Pos { col: 13, lineno: 1 }
                            })),
                            Pos { col: 14, lineno: 1 }
                        ),
                        (
                            Expression::BasicLit(Box::new(BasicLit {
                                kind: LiteralKind::Int,
                                value: "2",
                                pos: Pos { col: 15, lineno: 1 }
                            })),
                            Pos::zero(),
                        )
                    ]))
                })
            }))
        );
    }

    #[test]
    fn var_group_decl() {
        test_ast_ok!(
            "var (x,y int=1,2;z)",
            AST::VarDecl(Box::new(VarDecl {
                pos: Pos { col: 1, lineno: 1 },
                decl: VarDeclOption::Group(VarGroup {
                    lparen: Pos { col: 5, lineno: 1 },
                    rparen: Pos { col: 19, lineno: 1 },
                    specs: vec![
                        (
                            VarSpec {
                                doc: None,
                                line_comment: None,
                                ident_list: IdentifierList {
                                    ident: Identifier {
                                        name: "x",
                                        pos: Pos { col: 6, lineno: 1 }
                                    },
                                    followers: vec![CommaAndIdentifier {
                                        comma: Pos { col: 7, lineno: 1 },
                                        ident: Identifier {
                                            name: "y",
                                            pos: Pos { col: 8, lineno: 1 }
                                        }
                                    }]
                                },
                                typ: Some(Expression::Ident(Box::new(Identifier {
                                    name: "int",
                                    pos: Pos { col: 10, lineno: 1 }
                                }))),
                                eq: Some(Pos { col: 13, lineno: 1 }),
                                expr_list: Some(Expression::ListExpr(vec![
                                    (
                                        Expression::BasicLit(Box::new(BasicLit {
                                            kind: LiteralKind::Int,
                                            value: "1",
                                            pos: Pos { col: 14, lineno: 1 }
                                        })),
                                        Pos { col: 15, lineno: 1 }
                                    ),
                                    (
                                        Expression::BasicLit(Box::new(BasicLit {
                                            kind: LiteralKind::Int,
                                            value: "2",
                                            pos: Pos { col: 16, lineno: 1 }
                                        })),
                                        Pos::zero(),
                                    )
                                ]))
                            },
                            Pos { col: 17, lineno: 1 }
                        ),
                        (
                            VarSpec {
                                doc: None,
                                line_comment: None,
                                ident_list: IdentifierList {
                                    ident: Identifier {
                                        name: "z",
                                        pos: Pos { col: 18, lineno: 1 }
                                    },
                                    followers: vec![]
                                },
                                typ: None,
                                eq: None,
                                expr_list: None,
                            },
                            Pos::zero(),
                        )
                    ]
                })
            }))
        );
    }

    #[test]
    fn label() {
        test_ast_ok!(
            "L:x+=1",
            AST::Stmt(Stmt::Label(Box::new(LabelStmt {
                label: Identifier {
                    name: "L",
                    pos: Pos { col: 1, lineno: 1 }
                },
                colon: Pos { col: 2, lineno: 1 },
                stmt: Some(Stmt::Assign(Box::new(AssignStmt {
                    expr: Expression::Ident(Box::new(Identifier {
                        name: "x",
                        pos: Pos { col: 3, lineno: 1 }
                    })),
                    op: OpLitPos {
                        name: "+=",
                        pos: Pos { col: 4, lineno: 1 }
                    },
                    value: Expression::BasicLit(Box::new(BasicLit {
                        kind: LiteralKind::Int,
                        value: "1",
                        pos: Pos { col: 6, lineno: 1 }
                    }))
                })))
            })))
        );
    }

    #[test]
    fn const_decl_in_block() {
        test_ast_ok!(
            "{const x = 1}",
            AST::Stmt(Stmt::Block(Box::new(BlockStmt {
                lbrace: Pos { col: 1, lineno: 1 },
                stmts: vec![Stmt::ConstDecl(Box::new(VarDecl {
                    pos: Pos { col: 2, lineno: 1 },
                    decl: VarDeclOption::Spec(VarSpec {
                        doc: None,
                        line_comment: None,
                        ident_list: IdentifierList {
                            ident: Identifier {
                                name: "x",
                                pos: Pos { col: 8, lineno: 1 }
                            },
                            followers: vec![]
                        },
                        typ: None,
                        eq: Some(Pos { col: 10, lineno: 1 }),
                        expr_list: Some(Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "1",
                            pos: Pos { col: 12, lineno: 1 }
                        })))
                    })
                }))],
                rbrace: Pos { col: 13, lineno: 1 }
            })))
        );
    }

    #[test]
    fn var_decl_in_block() {
        test_ast_ok!(
            "{var x = 1}",
            AST::Stmt(Stmt::Block(Box::new(BlockStmt {
                lbrace: Pos { col: 1, lineno: 1 },
                stmts: vec![Stmt::VarDecl(Box::new(VarDecl {
                    pos: Pos { col: 2, lineno: 1 },
                    decl: VarDeclOption::Spec(VarSpec {
                        doc: None,
                        line_comment: None,
                        ident_list: IdentifierList {
                            ident: Identifier {
                                name: "x",
                                pos: Pos { col: 6, lineno: 1 }
                            },
                            followers: vec![]
                        },
                        typ: None,
                        eq: Some(Pos { col: 8, lineno: 1 }),
                        expr_list: Some(Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "1",
                            pos: Pos { col: 10, lineno: 1 }
                        })))
                    })
                }))],
                rbrace: Pos { col: 11, lineno: 1 }
            })))
        );
    }

    #[test]
    fn type_decl_in_block() {
        test_ast_ok!(
            "{type x = int}",
            AST::Stmt(Stmt::Block(Box::new(BlockStmt {
                lbrace: Pos { col: 1, lineno: 1 },
                stmts: vec![Stmt::TypeDecl(Box::new(TypeDecl {
                    decl: TypeDeclOption::Spec(TypeSpec::AliasDecl(AliasDecl {
                        ident: Identifier {
                            name: "x",
                            pos: Pos { col: 7, lineno: 1 }
                        },
                        params: None,
                        eq: Pos { col: 9, lineno: 1 },
                        typ: Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 11, lineno: 1 }
                        }))
                    })),
                    pos: Pos { col: 2, lineno: 1 }
                }))],
                rbrace: Pos { col: 14, lineno: 1 }
            })))
        );
    }

    #[test]
    fn go() {
        test_ast_ok!(
            "{go f()}",
            AST::Stmt(Stmt::Block(Box::new(BlockStmt {
                lbrace: Pos { col: 1, lineno: 1 },
                stmts: vec![Stmt::Call(Box::new(CallStmt {
                    tok: TokenPos {
                        tok: Token::Go,
                        pos: Pos { col: 2, lineno: 1 }
                    },
                    stmt: Expression::CallExpr(Box::new(CallExpr {
                        pexpr: Expression::Ident(Box::new(Identifier {
                            name: "f",
                            pos: Pos { col: 5, lineno: 1 }
                        })),
                        lparen: Pos { col: 6, lineno: 1 },
                        args: vec![],
                        rparen: Pos { col: 7, lineno: 1 }
                    }))
                })),],
                rbrace: Pos { col: 8, lineno: 1 },
            })))
        );
    }

    #[test]
    fn defer() {
        test_ast_ok!(
            "{defer f()}",
            AST::Stmt(Stmt::Block(Box::new(BlockStmt {
                lbrace: Pos { col: 1, lineno: 1 },
                stmts: vec![Stmt::Call(Box::new(CallStmt {
                    tok: TokenPos {
                        tok: Token::Defer,
                        pos: Pos { col: 2, lineno: 1 }
                    },
                    stmt: Expression::CallExpr(Box::new(CallExpr {
                        pexpr: Expression::Ident(Box::new(Identifier {
                            name: "f",
                            pos: Pos { col: 8, lineno: 1 }
                        })),
                        lparen: Pos { col: 9, lineno: 1 },
                        args: vec![],
                        rparen: Pos { col: 10, lineno: 1 }
                    }))
                })),],
                rbrace: Pos { col: 11, lineno: 1 },
            })))
        );
    }

    #[test]
    fn r#return() {
        test_ast_ok!(
            "return 1,2",
            AST::Stmt(Stmt::Return(Box::new(ReturnStmt {
                pos: Pos { col: 1, lineno: 1 },
                expr: Some(Expression::ListExpr(vec![
                    (
                        Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "1",
                            pos: Pos { col: 8, lineno: 1 }
                        })),
                        Pos { col: 9, lineno: 1 }
                    ),
                    (
                        Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "2",
                            pos: Pos { col: 10, lineno: 1 }
                        })),
                        Pos::zero(),
                    )
                ]))
            })))
        );
        test_ast_ok!(
            "return",
            AST::Stmt(Stmt::Return(Box::new(ReturnStmt {
                pos: Pos { col: 1, lineno: 1 },
                expr: None,
            })))
        );
    }

    #[test]
    fn r#break() {
        test_ast_ok!(
            "break L",
            AST::Stmt(Stmt::Branch(Box::new(BranchStmt {
                tok: TokenPos {
                    tok: Token::Break,
                    pos: Pos { col: 1, lineno: 1 }
                },
                label: Some(Identifier {
                    name: "L",
                    pos: Pos { col: 7, lineno: 1 }
                })
            })))
        );
        test_ast_ok!(
            "break",
            AST::Stmt(Stmt::Branch(Box::new(BranchStmt {
                tok: TokenPos {
                    tok: Token::Break,
                    pos: Pos { col: 1, lineno: 1 }
                },
                label: None,
            })))
        );
    }

    #[test]
    fn r#continue() {
        test_ast_ok!(
            "continue L",
            AST::Stmt(Stmt::Branch(Box::new(BranchStmt {
                tok: TokenPos {
                    tok: Token::Continue,
                    pos: Pos { col: 1, lineno: 1 }
                },
                label: Some(Identifier {
                    name: "L",
                    pos: Pos { col: 10, lineno: 1 }
                })
            })))
        );
        test_ast_ok!(
            "continue",
            AST::Stmt(Stmt::Branch(Box::new(BranchStmt {
                tok: TokenPos {
                    tok: Token::Continue,
                    pos: Pos { col: 1, lineno: 1 }
                },
                label: None,
            })))
        );
    }

    #[test]
    fn if_stmt_smoke() {
        test_ast_ok!(
            "if x:=f;x{}else{}",
            AST::Stmt(Stmt::If(Box::new(IfStmt {
                pos: Pos { col: 1, lineno: 1 },
                init: Some((
                    Stmt::Assign(Box::new(AssignStmt {
                        expr: Expression::Ident(Box::new(Identifier {
                            name: "x",
                            pos: Pos { col: 4, lineno: 1 }
                        })),
                        op: OpLitPos {
                            name: ":=",
                            pos: Pos { col: 5, lineno: 1 }
                        },
                        value: Expression::Ident(Box::new(Identifier {
                            name: "f",
                            pos: Pos { col: 7, lineno: 1 }
                        }))
                    })),
                    Pos { col: 8, lineno: 1 }
                )),
                cond: Expression::Ident(Box::new(Identifier {
                    name: "x",
                    pos: Pos { col: 9, lineno: 1 }
                })),
                block: BlockStmt {
                    lbrace: Pos { col: 10, lineno: 1 },
                    stmts: vec![],
                    rbrace: Pos { col: 11, lineno: 1 }
                },
                r#else: Some((
                    Pos { col: 12, lineno: 1 },
                    Stmt::Block(Box::new(BlockStmt {
                        lbrace: Pos { col: 16, lineno: 1 },
                        stmts: vec![],
                        rbrace: Pos { col: 17, lineno: 1 }
                    }))
                )),
            })))
        );
    }

    #[test]
    fn if_stmt_empty_init() {
        test_ast_ok!(
            "if      x{}else{}",
            AST::Stmt(Stmt::If(Box::new(IfStmt {
                pos: Pos { col: 1, lineno: 1 },
                init: None,
                cond: Expression::Ident(Box::new(Identifier {
                    name: "x",
                    pos: Pos { col: 9, lineno: 1 }
                })),
                block: BlockStmt {
                    lbrace: Pos { col: 10, lineno: 1 },
                    stmts: vec![],
                    rbrace: Pos { col: 11, lineno: 1 }
                },
                r#else: Some((
                    Pos { col: 12, lineno: 1 },
                    Stmt::Block(Box::new(BlockStmt {
                        lbrace: Pos { col: 16, lineno: 1 },
                        stmts: vec![],
                        rbrace: Pos { col: 17, lineno: 1 }
                    }))
                )),
            })))
        );
    }

    #[test]
    fn if_stmt_else_if() {
        test_ast_ok!(
            "if      x{}else if x{}",
            AST::Stmt(Stmt::If(Box::new(IfStmt {
                pos: Pos { col: 1, lineno: 1 },
                init: None,
                cond: Expression::Ident(Box::new(Identifier {
                    name: "x",
                    pos: Pos { col: 9, lineno: 1 }
                })),
                block: BlockStmt {
                    lbrace: Pos { col: 10, lineno: 1 },
                    stmts: vec![],
                    rbrace: Pos { col: 11, lineno: 1 }
                },
                r#else: Some((
                    Pos { col: 12, lineno: 1 },
                    Stmt::If(Box::new(IfStmt {
                        pos: Pos { col: 17, lineno: 1 },
                        init: None,
                        cond: Expression::Ident(Box::new(Identifier {
                            name: "x",
                            pos: Pos { col: 20, lineno: 1 }
                        })),
                        block: BlockStmt {
                            lbrace: Pos { col: 21, lineno: 1 },
                            stmts: vec![],
                            rbrace: Pos { col: 22, lineno: 1 }
                        },
                        r#else: None
                    }))
                )),
            })))
        );
    }

    #[test]
    fn goto() {
        test_ast_ok!(
            "goto L",
            AST::Stmt(Stmt::Branch(Box::new(BranchStmt {
                tok: TokenPos {
                    tok: Token::Goto,
                    pos: Pos { col: 1, lineno: 1 }
                },
                label: Some(Identifier {
                    name: "L",
                    pos: Pos { col: 6, lineno: 1 }
                })
            })))
        );
    }

    #[test]
    fn fallthrough() {
        test_ast_ok!(
            "fallthrough",
            AST::Stmt(Stmt::Branch(Box::new(BranchStmt {
                tok: TokenPos {
                    tok: Token::Fallthrough,
                    pos: Pos { col: 1, lineno: 1 }
                },
                label: None,
            })))
        );
    }

    #[test]
    fn switch_smoke() {
        test_ast_ok!(
            "switch x:=1;x{case 1:x++;default:x--}",
            AST::Stmt(Stmt::Switch(Box::new(SwitchStmt {
                pos: Pos { col: 1, lineno: 1 },
                init: Some((
                    Stmt::Assign(Box::new(AssignStmt {
                        expr: Expression::Ident(Box::new(Identifier {
                            name: "x",
                            pos: Pos { col: 8, lineno: 1 }
                        })),
                        op: OpLitPos {
                            name: ":=",
                            pos: Pos { col: 9, lineno: 1 }
                        },
                        value: Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "1",
                            pos: Pos { col: 11, lineno: 1 }
                        }))
                    })),
                    Pos { col: 12, lineno: 1 }
                )),
                tag: Some(Expression::Ident(Box::new(Identifier {
                    name: "x",
                    pos: Pos { col: 13, lineno: 1 }
                }))),
                lbrace: Pos { col: 14, lineno: 1 },
                body: vec![
                    CaseCause {
                        tok_pos: TokenPos {
                            tok: Token::Case,
                            pos: Pos { col: 15, lineno: 1 }
                        },
                        expr: Some(Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "1",
                            pos: Pos { col: 20, lineno: 1 }
                        }))),
                        colon: Pos { col: 21, lineno: 1 },
                        body: vec![
                            Stmt::IncDec(Box::new(IncDecStmt {
                                expr: Expression::Ident(Box::new(Identifier {
                                    name: "x",
                                    pos: Pos { col: 22, lineno: 1 }
                                })),
                                op: OpLitPos {
                                    pos: Pos { col: 23, lineno: 1 },
                                    name: "++"
                                },
                            },)),
                            Stmt::Semi(Pos { col: 25, lineno: 1 })
                        ],
                    },
                    CaseCause {
                        tok_pos: TokenPos {
                            tok: Token::Default,
                            pos: Pos { col: 26, lineno: 1 }
                        },
                        expr: None,
                        colon: Pos { col: 33, lineno: 1 },
                        body: vec![Stmt::IncDec(Box::new(IncDecStmt {
                            expr: Expression::Ident(Box::new(Identifier {
                                name: "x",
                                pos: Pos { col: 34, lineno: 1 }
                            })),
                            op: OpLitPos {
                                pos: Pos { col: 35, lineno: 1 },
                                name: "--"
                            }
                        }))],
                    }
                ],
                rbrace: Pos { col: 37, lineno: 1 }
            })))
        );
    }

    #[test]
    fn switch_empty() {
        test_ast_ok!(
            "switch {}",
            AST::Stmt(Stmt::Switch(Box::new(SwitchStmt {
                pos: Pos { col: 1, lineno: 1 },
                init: None,
                tag: None,
                lbrace: Pos { col: 8, lineno: 1 },
                body: vec![],
                rbrace: Pos { col: 9, lineno: 1 }
            })))
        );
    }

    #[test]
    fn switch_init_only() {
        test_ast_ok!(
            "switch x:=1;{}",
            AST::Stmt(Stmt::Switch(Box::new(SwitchStmt {
                pos: Pos { col: 1, lineno: 1 },
                init: Some((
                    Stmt::Assign(Box::new(AssignStmt {
                        expr: Expression::Ident(Box::new(Identifier {
                            name: "x",
                            pos: Pos { col: 8, lineno: 1 }
                        })),
                        op: OpLitPos {
                            name: ":=",
                            pos: Pos { col: 9, lineno: 1 }
                        },
                        value: Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "1",
                            pos: Pos { col: 11, lineno: 1 }
                        }))
                    })),
                    Pos { col: 12, lineno: 1 }
                )),
                tag: None,
                lbrace: Pos { col: 13, lineno: 1 },
                body: vec![],
                rbrace: Pos { col: 14, lineno: 1 }
            })))
        );
    }

    #[test]
    fn switch_type_guard() {
        test_ast_ok!(
            "switch x:=1;i:=x.(type){case int:x++;default:x--}",
            AST::Stmt(Stmt::Switch(Box::new(SwitchStmt {
                pos: Pos { col: 1, lineno: 1 },
                init: Some((
                    Stmt::Assign(Box::new(AssignStmt {
                        expr: Expression::Ident(Box::new(Identifier {
                            name: "x",
                            pos: Pos { col: 8, lineno: 1 }
                        })),
                        op: OpLitPos {
                            name: ":=",
                            pos: Pos { col: 9, lineno: 1 }
                        },
                        value: Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "1",
                            pos: Pos { col: 11, lineno: 1 }
                        }))
                    })),
                    Pos { col: 12, lineno: 1 }
                )),
                tag: Some(Expression::TypeSwitchGuard(Box::new(TypeSwitchGuard {
                    lhs: Some((
                        Identifier {
                            name: "i",
                            pos: Pos { col: 13, lineno: 1 }
                        },
                        Pos { col: 14, lineno: 1 }
                    )),
                    x: Expression::Ident(Box::new(Identifier {
                        name: "x",
                        pos: Pos { col: 16, lineno: 1 }
                    })),
                    dot: Pos { col: 17, lineno: 1 },
                    lparen: Pos { col: 18, lineno: 1 },
                    typ: Pos { col: 19, lineno: 1 },
                    rparen: Pos { col: 23, lineno: 1 },
                }))),
                lbrace: Pos { col: 24, lineno: 1 },
                body: vec![
                    CaseCause {
                        tok_pos: TokenPos {
                            tok: Token::Case,
                            pos: Pos { col: 25, lineno: 1 }
                        },
                        expr: Some(Expression::Ident(Box::new(Identifier {
                            name: "int",
                            pos: Pos { col: 30, lineno: 1 }
                        }))),
                        colon: Pos { col: 33, lineno: 1 },
                        body: vec![
                            Stmt::IncDec(Box::new(IncDecStmt {
                                expr: Expression::Ident(Box::new(Identifier {
                                    name: "x",
                                    pos: Pos { col: 34, lineno: 1 }
                                })),
                                op: OpLitPos {
                                    pos: Pos { col: 35, lineno: 1 },
                                    name: "++"
                                },
                            },)),
                            Stmt::Semi(Pos { col: 37, lineno: 1 })
                        ],
                    },
                    CaseCause {
                        tok_pos: TokenPos {
                            tok: Token::Default,
                            pos: Pos { col: 38, lineno: 1 }
                        },
                        expr: None,
                        colon: Pos { col: 45, lineno: 1 },
                        body: vec![Stmt::IncDec(Box::new(IncDecStmt {
                            expr: Expression::Ident(Box::new(Identifier {
                                name: "x",
                                pos: Pos { col: 46, lineno: 1 }
                            })),
                            op: OpLitPos {
                                pos: Pos { col: 47, lineno: 1 },
                                name: "--"
                            }
                        }))],
                    }
                ],
                rbrace: Pos { col: 49, lineno: 1 }
            })))
        );
    }

    #[test]
    fn select_stmt() {
        test_ast_ok!(
            "select{case x:=<-x:fallthrough;default:x++}",
            AST::Stmt(Stmt::Select(Box::new(SelectStmt {
                pos: Pos { col: 1, lineno: 1 },
                lbrace: Pos { col: 7, lineno: 1 },
                body: vec![
                    CommClause {
                        comm: (
                            Stmt::Assign(Box::new(AssignStmt {
                                expr: Expression::Ident(Box::new(Identifier {
                                    name: "x",
                                    pos: Pos { col: 13, lineno: 1 }
                                })),
                                op: Identifier {
                                    name: ":=",
                                    pos: Pos { col: 14, lineno: 1 }
                                },
                                value: Expression::Operation(Box::new(Operation {
                                    op: OperatorPos {
                                        op: Operator::Recv,
                                        pos: Pos { col: 16, lineno: 1 }
                                    },
                                    x: Expression::Ident(Box::new(Identifier {
                                        name: "x",
                                        pos: Pos { col: 18, lineno: 1 }
                                    })),
                                    y: None
                                }))
                            })),
                            TokenPos {
                                tok: Token::Case,
                                pos: Pos { col: 8, lineno: 1 }
                            }
                        ),
                        colon: Pos { col: 19, lineno: 1 },
                        body: vec![
                            Stmt::Branch(Box::new(BranchStmt {
                                tok: TokenPos {
                                    tok: Token::Fallthrough,
                                    pos: Pos { col: 20, lineno: 1 }
                                },
                                label: None,
                            })),
                            Stmt::Semi(Pos { col: 31, lineno: 1 })
                        ],
                    },
                    CommClause {
                        comm: (
                            Stmt::Empty,
                            TokenPos {
                                tok: Token::Default,
                                pos: Pos { col: 32, lineno: 1 }
                            }
                        ),
                        colon: Pos { col: 39, lineno: 1 },
                        body: vec![Stmt::IncDec(Box::new(IncDecStmt {
                            expr: Expression::Ident(Box::new(Identifier {
                                name: "x",
                                pos: Pos { col: 40, lineno: 1 }
                            })),
                            op: OpLitPos {
                                name: "++",
                                pos: Pos { col: 41, lineno: 1 }
                            }
                        }))],
                    }
                ],
                rbrace: Pos { col: 43, lineno: 1 }
            })))
        );
    }

    #[test]
    fn for_cond() {
        test_ast_ok!(
            "for true {}",
            AST::Stmt(Stmt::For(Box::new(ForStmt {
                pos: Pos { col: 1, lineno: 1 },
                opt: ForOption::Cond(Expression::Ident(Box::new(Identifier {
                    name: "true",
                    pos: Pos { col: 5, lineno: 1 }
                }))),
                body: BlockStmt {
                    lbrace: Pos { col: 10, lineno: 1 },
                    stmts: vec![],
                    rbrace: Pos { col: 11, lineno: 1 }
                }
            })))
        );
    }

    #[test]
    fn for_range_clause() {
        test_ast_ok!(
            "for x:=range x {}",
            AST::Stmt(Stmt::For(Box::new(ForStmt {
                pos: Pos { col: 1, lineno: 1 },
                opt: ForOption::Range(RangeClause {
                    value: Some((
                        Expression::Ident(Box::new(Identifier {
                            name: "x",
                            pos: Pos { col: 5, lineno: 1 }
                        })),
                        TokenPos {
                            tok: Token::Define,
                            pos: Pos { col: 6, lineno: 1 }
                        }
                    )),
                    range: Pos { col: 8, lineno: 1 },
                    expr: Expression::Ident(Box::new(Identifier {
                        name: "x",
                        pos: Pos { col: 14, lineno: 1 }
                    }))
                }),
                body: BlockStmt {
                    lbrace: Pos { col: 16, lineno: 1 },
                    stmts: vec![],
                    rbrace: Pos { col: 17, lineno: 1 }
                }
            })))
        );
    }
    #[test]
    fn for_range_only() {
        test_ast_ok!(
            "for range x {}",
            AST::Stmt(Stmt::For(Box::new(ForStmt {
                pos: Pos { col: 1, lineno: 1 },
                opt: ForOption::Range(RangeClause {
                    value: None,
                    range: Pos { col: 5, lineno: 1 },
                    expr: Expression::Ident(Box::new(Identifier {
                        name: "x",
                        pos: Pos { col: 11, lineno: 1 }
                    }))
                }),
                body: BlockStmt {
                    lbrace: Pos { col: 13, lineno: 1 },
                    stmts: vec![],
                    rbrace: Pos { col: 14, lineno: 1 }
                }
            })))
        );
    }

    #[test]
    fn for_for_cause() {
        test_ast_ok!(
            "for;;{}",
            AST::Stmt(Stmt::For(Box::new(ForStmt {
                pos: Pos { col: 1, lineno: 1 },
                opt: ForOption::ForClause(ForClause {
                    init: Stmt::Empty,
                    semi0: Pos { col: 4, lineno: 1 },
                    cond: None,
                    semi1: Pos { col: 5, lineno: 1 },
                    post: Stmt::Empty
                }),
                body: BlockStmt {
                    lbrace: Pos { col: 6, lineno: 1 },
                    stmts: vec![],
                    rbrace: Pos { col: 7, lineno: 1 }
                }
            })))
        );
        test_ast_ok!(
            "for{}",
            AST::Stmt(Stmt::For(Box::new(ForStmt {
                pos: Pos { col: 1, lineno: 1 },
                opt: ForOption::Empty,
                body: BlockStmt {
                    lbrace: Pos { col: 4, lineno: 1 },
                    stmts: vec![],
                    rbrace: Pos { col: 5, lineno: 1 }
                }
            })))
        );
        test_ast_ok!(
            "for x=1;x>1;x++{}",
            AST::Stmt(Stmt::For(Box::new(ForStmt {
                pos: Pos { col: 1, lineno: 1 },
                opt: ForOption::ForClause(ForClause {
                    init: Stmt::Assign(Box::new(AssignStmt {
                        expr: Expression::Ident(Box::new(Identifier {
                            name: "x",
                            pos: Pos { col: 5, lineno: 1 }
                        })),
                        op: OpLitPos {
                            name: "=",
                            pos: Pos { col: 6, lineno: 1 }
                        },
                        value: Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "1",
                            pos: Pos { col: 7, lineno: 1 }
                        }))
                    })),
                    semi0: Pos { col: 8, lineno: 1 },
                    cond: Some(Expression::Operation(Box::new(Operation {
                        op: OperatorPos {
                            op: Operator::Gtr,
                            pos: Pos { col: 10, lineno: 1 }
                        },
                        x: Expression::Ident(Box::new(Identifier {
                            name: "x",
                            pos: Pos { col: 9, lineno: 1 }
                        })),
                        y: Some(Expression::BasicLit(Box::new(BasicLit {
                            kind: LiteralKind::Int,
                            value: "1",
                            pos: Pos { col: 11, lineno: 1 }
                        })))
                    }))),
                    semi1: Pos { col: 12, lineno: 1 },
                    post: Stmt::IncDec(Box::new(IncDecStmt {
                        expr: Expression::Ident(Box::new(Identifier {
                            name: "x",
                            pos: Pos { col: 13, lineno: 1 }
                        })),
                        op: OpLitPos {
                            name: "++",
                            pos: Pos { col: 14, lineno: 1 }
                        }
                    }))
                }),
                body: BlockStmt {
                    lbrace: Pos { col: 16, lineno: 1 },
                    stmts: vec![],
                    rbrace: Pos { col: 17, lineno: 1 }
                }
            })))
        );
    }

    #[test]
    fn comments() {
        let mut parser = Parser::new(Block::new("//abc\n/*a\nb*/"), errh);
        match parser.parse() {
            Err(err) => panic!("{}", err),
            Ok(_) => assert_eq!(
                parser.comments,
                vec![CommentGroup {
                    comments: vec![
                        Comment {
                            pos: Pos { col: 1, lineno: 1 },
                            tok: Token::LineComment,
                            content: "//abc",
                        },
                        Comment {
                            pos: Pos { col: 1, lineno: 2 },
                            tok: Token::FullComment,
                            content: "/*a\nb*/",
                        }
                    ]
                }]
            ),
        }
    }

    #[test]
    fn type_group_doc() {
        test_ast_ok!(
            "/*x*/
type (
x=y
)",
            AST::TypeDecl(Box::new(TypeDecl {
                decl: TypeDeclOption::Group(TypeGroup {
                    lparen: Pos { col: 6, lineno: 2 },
                    rparen: Pos { col: 1, lineno: 4 },
                    specs: vec![(
                        TypeSpec::AliasDecl(AliasDecl {
                            ident: Identifier {
                                name: "x",
                                pos: Pos { col: 1, lineno: 3 }
                            },
                            params: None,
                            eq: Pos { col: 2, lineno: 3 },
                            typ: Expression::Ident(Box::new(Identifier {
                                name: "y",
                                pos: Pos { col: 3, lineno: 3 }
                            }))
                        }),
                        Pos::zero()
                    )],
                    doc: Some(0),
                }),
                pos: Pos { col: 1, lineno: 2 }
            }))
        );
    }

    #[test]
    fn type_spec_doc() {
        test_ast_ok!(
            "type (
/*x*/

/*y*/
x y
)",
            AST::TypeDecl(Box::new(TypeDecl {
                decl: TypeDeclOption::Group(TypeGroup {
                    lparen: Pos { col: 6, lineno: 1 },
                    rparen: Pos { col: 1, lineno: 6 },
                    specs: vec![(
                        TypeSpec::TypeDef(TypeDef {
                            ident: Identifier {
                                name: "x",
                                pos: Pos { col: 1, lineno: 5 }
                            },
                            params: None,
                            typ: Expression::Ident(Box::new(Identifier {
                                name: "y",
                                pos: Pos { col: 3, lineno: 5 }
                            })),
                            doc: Some(1),
                        }),
                        Pos::zero()
                    )],
                    doc: None,
                }),
                pos: Pos { col: 1, lineno: 1 }
            }))
        );
    }

    #[test]
    fn parse_smoke() {
        let mut parser = Parser::new(
            Block::new(
                r#"/*
Parenthesized type switch expressions originally
accepted by gofmt must continue to be rewritten
into the correct unparenthesized form.

Only type-switches that didn't declare a variable
in the type switch type assertion and which
contained only "expression-like" (named) types in their
cases were permitted to have their type assertion parenthesized
by go/parser (due to a weak predicate in the parser). All others
were rejected always, either with a syntax error in the
type switch header or in the case.

See also issue 4470.
*/
package p

func f() {
	var x interface{}
	switch x.(type) { // should remain the same
	}
	switch x.(type) { // should become: switch x.(type) {
	}

	switch x.(type) { // should remain the same
	case int:
	}
	switch x.(type) { // should become: switch x.(type) {
	case int:
	}

	switch x.(type) { // should remain the same
	case []int:
	}

	// Parenthesized (x.(type)) in type switches containing cases
	// with unnamed (literal) types were never permitted by gofmt;
	// thus there won't be any code in the wild using this style if
	// the code was gofmt-ed.
	/*
		switch (x.(type)) {
		case []int:
		}
	*/

	switch t := x.(type) { // should remain the same
	default:
		_ = t
	}

	// Parenthesized (x.(type)) in type switches declaring a variable
	// were never permitted by gofmt; thus there won't be any code in
	// the wild using this style if the code was gofmt-ed.
	/*
		switch t := (x.(type)) {
		default:
			_ = t
		}
	*/
}
"#,
            ),
            errh,
        );
        match parser.parse() {
            Err(err) => panic!("{}", err),
            Ok(ast) => println!("{:?}", ast),
        }
    }

    #[test]
    fn import_doc_comment() {
        test_ast_ok!(
            r#"import (
// x
 x "abc" // x
// y
 y "abc" // x
 )"#,
            AST::ImportDecl(Box::new(ImportDecl {
                pos: Pos { lineno: 1, col: 1 },
                import: ImportOption::Group(ImportGroup {
                    lparen: Pos { lineno: 1, col: 8 },
                    rparen: Pos { lineno: 6, col: 2 },
                    specs: vec![
                        ImportSpec {
                            doc: Some(0),
                            line_comment: Some(1),
                            pkg_name: Some(Identifier {
                                name: "x",
                                pos: Pos { lineno: 3, col: 2 },
                            }),
                            path: BasicLit {
                                kind: LiteralKind::String,
                                value: "\"abc\"",
                                pos: Pos { lineno: 3, col: 4 },
                            },
                        },
                        ImportSpec {
                            doc: Some(2),
                            line_comment: Some(3),
                            pkg_name: Some(Identifier {
                                name: "y",
                                pos: Pos { lineno: 5, col: 2 },
                            }),
                            path: BasicLit {
                                kind: LiteralKind::String,
                                value: "\"abc\"",
                                pos: Pos { lineno: 5, col: 4 },
                            },
                        }
                    ],
                }),
            })),
        );
    }

    #[test]
    fn var_spec_doc() {
        test_ast_ok!(
            "/*x*/
var x,y int=1,2 // x",
            AST::VarDecl(Box::new(VarDecl {
                pos: Pos { col: 1, lineno: 2 },
                decl: VarDeclOption::Spec(VarSpec {
                    doc: Some(0),
                    line_comment: Some(1),
                    ident_list: IdentifierList {
                        ident: Identifier {
                            name: "x",
                            pos: Pos { col: 5, lineno: 2 }
                        },
                        followers: vec![CommaAndIdentifier {
                            comma: Pos { col: 6, lineno: 2 },
                            ident: Identifier {
                                name: "y",
                                pos: Pos { col: 7, lineno: 2 }
                            }
                        }]
                    },
                    typ: Some(Expression::Ident(Box::new(Identifier {
                        name: "int",
                        pos: Pos { col: 9, lineno: 2 }
                    }))),
                    eq: Some(Pos { col: 12, lineno: 2 }),
                    expr_list: Some(Expression::ListExpr(vec![
                        (
                            Expression::BasicLit(Box::new(BasicLit {
                                kind: LiteralKind::Int,
                                value: "1",
                                pos: Pos { col: 13, lineno: 2 }
                            })),
                            Pos { col: 14, lineno: 2 }
                        ),
                        (
                            Expression::BasicLit(Box::new(BasicLit {
                                kind: LiteralKind::Int,
                                value: "2",
                                pos: Pos { col: 15, lineno: 2 }
                            })),
                            Pos::zero(),
                        )
                    ]))
                })
            }))
        );
    }

    #[test]
    fn func_doc() {
        test_ast_ok!(
            "/*x*/
func f(){+x}",
            AST::FuncDecl(Box::new(FuncDecl {
                doc: Some(0),
                func: Pos { col: 1, lineno: 2 },
                name: Identifier {
                    name: "f",
                    pos: Pos { col: 6, lineno: 2 }
                },
                type_params: None,
                sign: FuncSign {
                    params: Params {
                        lparen: Pos { col: 7, lineno: 2 },
                        list: None,
                        rparen: Pos { col: 8, lineno: 2 }
                    },
                    ret: None
                },
                body: Some(BlockStmt {
                    lbrace: Pos { col: 9, lineno: 2 },
                    stmts: vec![Stmt::Expr(Expression::Operation(Box::new(Operation {
                        op: OperatorPos {
                            op: Operator::Add,
                            pos: Pos { col: 10, lineno: 2 }
                        },
                        x: Expression::Ident(Box::new(Identifier {
                            name: "x",
                            pos: Pos { col: 11, lineno: 2 }
                        })),
                        y: None
                    })))],
                    rbrace: Pos { col: 12, lineno: 2 }
                })
            }))
        );
    }

    #[test]
    fn struct_doc_comment() {
        test_ast_ok!(
            r#"type s struct {
                //xxx
  y,  z string; // x
}"#,
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { lineno: 1, col: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    params: None,
                    ident: Identifier {
                        name: "s",
                        pos: Pos { lineno: 1, col: 6 }
                    },
                    typ: (Expression::Struct(Box::new(StructType {
                        lbrace: Pos { lineno: 1, col: 15 },
                        rbrace: Pos { lineno: 4, col: 1 },
                        pos: Pos { lineno: 1, col: 8 },
                        field_decls: vec![(
                            FieldDeclOption::Fields(Fields {
                                doc: Some(0),
                                line_comment: Some(1),
                                idents: IdentifierList {
                                    ident: Identifier {
                                        name: "y",
                                        pos: Pos { col: 3, lineno: 3 },
                                    },
                                    followers: vec![CommaAndIdentifier {
                                        comma: Pos { col: 4, lineno: 3 },
                                        ident: Identifier {
                                            name: "z",
                                            pos: Pos { col: 7, lineno: 3 },
                                        }
                                    }],
                                },
                                typ: Expression::Ident(Box::new(Identifier {
                                    name: "string",
                                    pos: Pos { lineno: 3, col: 9 }
                                })),
                                tag: None,
                            }),
                            Pos { col: 15, lineno: 3 }
                        )],
                    }))),
                })),
            })),
        );
    }
    #[test]
    fn embedded_field_doc_comment() {
        test_ast_ok!(
            r#"type s struct {
                //x
*T//y
y.z //z
}"#,
            AST::TypeDecl(Box::new(TypeDecl {
                pos: Pos { lineno: 1, col: 1 },
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    params: None,
                    ident: Identifier {
                        name: "s",
                        pos: Pos { lineno: 1, col: 6 }
                    },
                    typ: Expression::Struct(Box::new(StructType {
                        lbrace: Pos { lineno: 1, col: 15 },
                        rbrace: Pos { lineno: 5, col: 1 },
                        pos: Pos { lineno: 1, col: 8 },
                        field_decls: vec![
                            (
                                FieldDeclOption::Embedded(EmbeddedFieldTypeField {
                                    doc: Some(0),
                                    line_comment: Some(1),
                                    tag: None,
                                    typ: Expression::Operation(Box::new(Operation {
                                        op: OperatorPos {
                                            pos: Pos { col: 1, lineno: 3 },
                                            op: Operator::Mul,
                                        },
                                        x: Expression::Ident(Box::new(Identifier {
                                            name: "T",
                                            pos: Pos { col: 2, lineno: 3 }
                                        }),),
                                        y: None,
                                    })),
                                }),
                                Pos::zero()
                            ),
                            (
                                FieldDeclOption::Embedded(EmbeddedFieldTypeField {
                                    doc: None,
                                    line_comment: Some(2),
                                    tag: None,
                                    typ: Expression::QualifiedName(Box::new(QualifiedName {
                                        pkg: Identifier {
                                            name: "y",
                                            pos: Pos { col: 1, lineno: 4 }
                                        },
                                        dot: Pos { col: 2, lineno: 4 },
                                        ident: Identifier {
                                            name: "z",
                                            pos: Pos { col: 3, lineno: 4 }
                                        }
                                    })),
                                }),
                                Pos::zero()
                            ),
                        ],
                    })),
                })),
            })),
        );
    }

    #[test]
    fn method_decl_doc() {
        test_ast_ok!(
            "/*x*/
func (a int)f(){}",
            AST::MethodDecl(Box::new(MethodDecl {
                doc: Some(0),
                func: Pos { col: 1, lineno: 2 },
                receiver: Params {
                    lparen: Pos { col: 6, lineno: 2 },
                    list: Some(vec![(
                        ParamDecl {
                            idents: Some(IdentifierList {
                                ident: Identifier {
                                    name: "a",
                                    pos: Pos { col: 7, lineno: 2 }
                                },
                                followers: vec![],
                            }),
                            dotdotdot: None,
                            typ: Expression::Ident(Box::new(Identifier {
                                name: "int",
                                pos: Pos { col: 9, lineno: 2 }
                            }))
                        },
                        Pos::zero()
                    )]),
                    rparen: Pos { col: 12, lineno: 2 }
                },
                name: Identifier {
                    name: "f",
                    pos: Pos { col: 13, lineno: 2 }
                },
                sign: FuncSign {
                    params: Params {
                        lparen: Pos { col: 14, lineno: 2 },
                        list: None,
                        rparen: Pos { col: 15, lineno: 2 }
                    },
                    ret: None
                },
                body: Some(BlockStmt {
                    lbrace: Pos { col: 16, lineno: 2 },
                    stmts: vec![],
                    rbrace: Pos { col: 17, lineno: 2 }
                })
            }))
        );
    }

    #[test]
    fn interface_method_line_comment() {
        test_ast_ok!(
            "type i interface{
f(int)int //x
}",
            AST::TypeDecl(Box::new(TypeDecl {
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    ident: Identifier {
                        name: "i",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    params: None,
                    typ: (Expression::InterfaceType(Box::new(InterfaceType {
                        pos: Pos { col: 8, lineno: 1 },
                        lbrace: Pos { col: 17, lineno: 1 },
                        elems: vec![InterfaceElemAndSemi {
                            line_comment: Some(0),
                            semi: Pos { col: 0, lineno: 3 },
                            elem: InterfaceElem::MethodElem(MethodElem {
                                name: Identifier {
                                    name: "f",
                                    pos: Pos { col: 1, lineno: 2 }
                                },
                                sign: FuncSign {
                                    params: Params {
                                        lparen: Pos { col: 2, lineno: 2 },
                                        list: Some(vec![(
                                            ParamDecl {
                                                idents: None,
                                                dotdotdot: None,
                                                typ: Expression::Ident(Box::new(Identifier {
                                                    name: "int",
                                                    pos: Pos { col: 3, lineno: 2 }
                                                }))
                                            },
                                            Pos::zero()
                                        )]),
                                        rparen: Pos { col: 6, lineno: 2 }
                                    },
                                    ret: Some(FuncResult::Type(Expression::Ident(Box::new(
                                        Identifier {
                                            name: "int",
                                            pos: Pos { col: 7, lineno: 2 }
                                        }
                                    ))))
                                }
                            })
                        }],
                        rbrace: Pos { col: 1, lineno: 3 }
                    })))
                })),
                pos: Pos { col: 1, lineno: 1 }
            }))
        );
    }

    #[test]
    fn interface_type_elem_line_comment() {
        test_ast_ok!(
            "type i interface{
int|~i //x
}",
            AST::TypeDecl(Box::new(TypeDecl {
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    doc: None,
                    ident: Identifier {
                        name: "i",
                        pos: Pos { col: 6, lineno: 1 }
                    },
                    params: None,
                    typ: Expression::InterfaceType(Box::new(InterfaceType {
                        pos: Pos { col: 8, lineno: 1 },
                        lbrace: Pos { col: 17, lineno: 1 },
                        elems: vec![InterfaceElemAndSemi {
                            line_comment: Some(0),
                            semi: Pos { col: 0, lineno: 3 },
                            elem: InterfaceElem::TypeElem(TypeElem {
                                followers: vec![OrTypeTerm {
                                    or: Pos { col: 4, lineno: 2 },
                                    term: Expression::UnderlyingType(Box::new(UnderlyingType {
                                        pos: Pos { col: 5, lineno: 2 },
                                        typ: Expression::Ident(Box::new(Identifier {
                                            name: "i",
                                            pos: Pos { col: 6, lineno: 2 }
                                        }))
                                    }))
                                }],
                                term: Expression::Ident(Box::new(Identifier {
                                    name: "int",
                                    pos: Pos { col: 1, lineno: 2 }
                                }))
                            })
                        },],
                        rbrace: Pos { col: 1, lineno: 3 }
                    }))
                })),
                pos: Pos { col: 1, lineno: 1 }
            }))
        );
    }

    #[test]
    fn func_slice_param() {
        test_ast_ok!(
            "func f(l []T) {}",
            AST::FuncDecl(Box::new(FuncDecl {
                doc: None,
                func: Pos { col: 1, lineno: 1 },
                name: Identifier {
                    name: "f",
                    pos: Pos { col: 6, lineno: 1 }
                },
                type_params: None,
                sign: FuncSign {
                    params: Params {
                        lparen: Pos { col: 7, lineno: 1 },
                        list: Some(vec![(
                            ParamDecl {
                                idents: Some(IdentifierList {
                                    ident: Identifier {
                                        name: "l",
                                        pos: Pos { col: 8, lineno: 1 },
                                    },
                                    followers: vec![],
                                },),
                                dotdotdot: None,
                                typ: Expression::SliceType(Box::new(SliceType {
                                    lbrack: Pos { col: 10, lineno: 1 },
                                    rbrack: Pos { col: 11, lineno: 1 },
                                    typ: Expression::Ident(Box::new(Identifier {
                                        name: "T",
                                        pos: Pos { col: 12, lineno: 1 },
                                    }),),
                                }),),
                            },
                            Pos { col: 0, lineno: 0 },
                        ),],),
                        rparen: Pos { col: 13, lineno: 1 },
                    },
                    ret: None,
                },
                body: Some(BlockStmt {
                    lbrace: Pos { col: 15, lineno: 1 },
                    stmts: vec![],
                    rbrace: Pos { col: 16, lineno: 1 },
                }),
            }))
        );
    }

    #[test]
    fn func_pointer_return_param() {
        test_ast_ok!(
            "func f(l *PT)(
                *T, error,
                ) {}",
            AST::FuncDecl(Box::new(FuncDecl {
                doc: None,
                func: Pos { col: 1, lineno: 1 },
                name: Identifier {
                    name: "f",
                    pos: Pos { col: 6, lineno: 1 }
                },
                type_params: None,
                sign: FuncSign {
                    params: Params {
                        lparen: Pos { col: 7, lineno: 1 },
                        list: Some(vec![(
                            ParamDecl {
                                idents: Some(IdentifierList {
                                    ident: Identifier {
                                        name: "l",
                                        pos: Pos { col: 8, lineno: 1 },
                                    },
                                    followers: vec![],
                                },),
                                dotdotdot: None,
                                typ: Expression::Operation(Box::new(Operation {
                                    op: OperatorPos {
                                        op: Operator::Mul,
                                        pos: Pos { col: 10, lineno: 1 }
                                    },
                                    x: Expression::Ident(Box::new(Identifier {
                                        name: "PT",
                                        pos: Pos { col: 11, lineno: 1 }
                                    })),
                                    y: None,
                                }),),
                            },
                            Pos { col: 0, lineno: 0 },
                        ),],),
                        rparen: Pos { col: 13, lineno: 1 },
                    },
                    ret: Some(FuncResult::Params(Params {
                        lparen: Pos { col: 14, lineno: 1 },
                        list: Some(vec![
                            (
                                ParamDecl {
                                    idents: None,
                                    dotdotdot: None,
                                    typ: Expression::Operation(Box::new(Operation {
                                        op: OperatorPos {
                                            op: Operator::Mul,
                                            pos: Pos { col: 17, lineno: 2 },
                                        },
                                        x: Expression::Ident(Box::new(Identifier {
                                            name: "T",
                                            pos: Pos { col: 18, lineno: 2 },
                                        }),),
                                        y: None,
                                    }),),
                                },
                                Pos { col: 19, lineno: 2 },
                            ),
                            (
                                ParamDecl {
                                    idents: None,
                                    dotdotdot: None,
                                    typ: Expression::Ident(Box::new(Identifier {
                                        name: "error",
                                        pos: Pos { col: 21, lineno: 2 },
                                    },)),
                                },
                                Pos { col: 26, lineno: 2 },
                            ),
                        ],),
                        rparen: Pos { col: 17, lineno: 3 },
                    }),),
                },
                body: Some(BlockStmt {
                    lbrace: Pos { col: 19, lineno: 3 },
                    stmts: vec![],
                    rbrace: Pos { col: 20, lineno: 3 },
                }),
            }))
        );
    }

    #[test]
    fn func_return_slice() {
        test_ast_ok!(
            "func f(l *PT)(
                []T,error,
                ) {}",
            AST::FuncDecl(Box::new(FuncDecl {
                doc: None,
                func: Pos { col: 1, lineno: 1 },
                name: Identifier {
                    name: "f",
                    pos: Pos { col: 6, lineno: 1 }
                },
                type_params: None,
                sign: FuncSign {
                    params: Params {
                        lparen: Pos { col: 7, lineno: 1 },
                        list: Some(vec![(
                            ParamDecl {
                                idents: Some(IdentifierList {
                                    ident: Identifier {
                                        name: "l",
                                        pos: Pos { col: 8, lineno: 1 },
                                    },
                                    followers: vec![],
                                },),
                                dotdotdot: None,
                                typ: Expression::Operation(Box::new(Operation {
                                    op: OperatorPos {
                                        op: Operator::Mul,
                                        pos: Pos { col: 10, lineno: 1 }
                                    },
                                    x: Expression::Ident(Box::new(Identifier {
                                        name: "PT",
                                        pos: Pos { col: 11, lineno: 1 }
                                    })),
                                    y: None,
                                }),),
                            },
                            Pos { col: 0, lineno: 0 },
                        ),],),
                        rparen: Pos { col: 13, lineno: 1 },
                    },
                    ret: Some(FuncResult::Params(Params {
                        lparen: Pos { col: 14, lineno: 1 },
                        list: Some(vec![
                            (
                                ParamDecl {
                                    idents: None,
                                    dotdotdot: None,
                                    typ: Expression::SliceType(Box::new(SliceType {
                                        lbrack: Pos { col: 17, lineno: 2 },
                                        typ: Expression::Ident(Box::new(Identifier {
                                            name: "T",
                                            pos: Pos { col: 19, lineno: 2 }
                                        })),
                                        rbrack: Pos { col: 18, lineno: 2 },
                                    }),),
                                },
                                Pos { col: 20, lineno: 2 },
                            ),
                            (
                                ParamDecl {
                                    idents: None,
                                    dotdotdot: None,
                                    typ: Expression::Ident(Box::new(Identifier {
                                        name: "error",
                                        pos: Pos { col: 21, lineno: 2 },
                                    },)),
                                },
                                Pos { col: 26, lineno: 2 },
                            ),
                        ],),
                        rparen: Pos { col: 17, lineno: 3 },
                    }),),
                },
                body: Some(BlockStmt {
                    lbrace: Pos { col: 19, lineno: 3 },
                    stmts: vec![],
                    rbrace: Pos { col: 20, lineno: 3 },
                }),
            }))
        );
    }

    #[test]
    fn func_multi_params() {
        test_ast_ok!(
            "func f(s []T, t T){}",
            AST::FuncDecl(Box::new(FuncDecl {
                doc: None,
                func: Pos { col: 1, lineno: 1 },
                name: Identifier {
                    name: "f",
                    pos: Pos { col: 6, lineno: 1 }
                },
                type_params: None,
                sign: FuncSign {
                    params: Params {
                        lparen: Pos { col: 7, lineno: 1 },
                        list: Some(vec![
                            (
                                ParamDecl {
                                    idents: Some(IdentifierList {
                                        ident: Identifier {
                                            name: "s",
                                            pos: Pos { col: 8, lineno: 1 }
                                        },
                                        followers: vec![],
                                    }),
                                    dotdotdot: None,
                                    typ: Expression::SliceType(Box::new(SliceType {
                                        lbrack: Pos { col: 10, lineno: 1 },
                                        typ: Expression::Ident(Box::new(Identifier {
                                            name: "T",
                                            pos: Pos { col: 12, lineno: 1 }
                                        })),
                                        rbrack: Pos { col: 11, lineno: 1 },
                                    })),
                                },
                                Pos { col: 13, lineno: 1 }
                            ),
                            (
                                ParamDecl {
                                    idents: Some(IdentifierList {
                                        ident: Identifier {
                                            name: "t",
                                            pos: Pos { col: 15, lineno: 1 }
                                        },
                                        followers: vec![]
                                    }),
                                    dotdotdot: None,
                                    typ: Expression::Ident(Box::new(Identifier {
                                        name: "T",
                                        pos: Pos { col: 17, lineno: 1 }
                                    })),
                                },
                                Pos::zero(),
                            )
                        ]),
                        rparen: Pos { col: 18, lineno: 1 }
                    },
                    ret: None,
                },
                body: Some(BlockStmt {
                    lbrace: Pos { col: 19, lineno: 1 },
                    stmts: vec![],
                    rbrace: Pos { col: 20, lineno: 1 },
                }),
            }))
        );
    }

    #[test]
    fn embedded_field_followed_by_field() {
        test_ast_ok!(
            "type fumpter struct {
	Options

	file *File
}",
            AST::TypeDecl(Box::new(TypeDecl {
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    ident: Identifier {
                        name: "fumpter",
                        pos: Pos { col: 6, lineno: 1 },
                    },
                    params: None,
                    typ: Expression::Struct(Box::new(StructType {
                        lbrace: Pos { col: 21, lineno: 1 },
                        rbrace: Pos { col: 1, lineno: 5 },
                        field_decls: vec![
                            (
                                FieldDeclOption::Embedded(EmbeddedFieldTypeField {
                                    doc: None,
                                    line_comment: None,
                                    typ: Expression::Ident(Box::new(Identifier {
                                        name: "Options",
                                        pos: Pos { col: 2, lineno: 2 },
                                    }),),
                                    tag: None,
                                }),
                                Pos { col: 0, lineno: 0 },
                            ),
                            (
                                FieldDeclOption::Fields(Fields {
                                    doc: None,
                                    line_comment: None,
                                    idents: IdentifierList {
                                        ident: Identifier {
                                            name: "file",
                                            pos: Pos { col: 2, lineno: 4 },
                                        },
                                        followers: vec![],
                                    },
                                    typ: Expression::Operation(Box::new(Operation {
                                        op: OperatorPos {
                                            op: Operator::Mul,
                                            pos: Pos { col: 7, lineno: 4 },
                                        },
                                        x: Expression::Ident(Box::new(Identifier {
                                            name: "File",
                                            pos: Pos { col: 8, lineno: 4 },
                                        }),),
                                        y: None,
                                    }),),
                                    tag: None,
                                },),
                                Pos { col: 0, lineno: 0 },
                            ),
                        ],
                        pos: Pos { col: 14, lineno: 1 },
                    }),),
                    doc: None,
                },),),
                pos: Pos { col: 1, lineno: 1 },
            }))
        );
    }

    #[test]
    fn slice_field_in_struct() {
        test_ast_ok!(
            "type fumpter struct {
	parentFuncTypes[]*FuncType
}",
            AST::TypeDecl(Box::new(TypeDecl {
                decl: TypeDeclOption::Spec(TypeSpec::TypeDef(TypeDef {
                    ident: Identifier {
                        name: "fumpter",
                        pos: Pos { col: 6, lineno: 1 },
                    },
                    params: None,
                    typ: Expression::Struct(Box::new(StructType {
                        lbrace: Pos { col: 21, lineno: 1 },
                        rbrace: Pos { col: 1, lineno: 3 },
                        field_decls: vec![(
                            FieldDeclOption::Fields(Fields {
                                doc: None,
                                line_comment: None,
                                idents: IdentifierList {
                                    ident: Identifier {
                                        name: "parentFuncTypes",
                                        pos: Pos { col: 2, lineno: 2 },
                                    },
                                    followers: vec![],
                                },
                                typ: Expression::SliceType(Box::new(SliceType {
                                    lbrack: Pos { col: 17, lineno: 2 },
                                    rbrack: Pos { col: 18, lineno: 2 },
                                    typ: Expression::Operation(Box::new(Operation {
                                        op: OperatorPos {
                                            op: Operator::Mul,
                                            pos: Pos { col: 19, lineno: 2 },
                                        },
                                        x: Expression::Ident(Box::new(Identifier {
                                            name: "FuncType",
                                            pos: Pos { col: 20, lineno: 2 },
                                        }),),
                                        y: None,
                                    }),),
                                }),),
                                tag: None,
                            },),
                            Pos { col: 0, lineno: 0 },
                        ),],
                        pos: Pos { col: 14, lineno: 1 },
                    }),),
                    doc: None,
                },),),
                pos: Pos { col: 1, lineno: 1 },
            }),),
        );
    }
}
