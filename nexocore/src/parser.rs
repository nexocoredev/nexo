use crate::{
    ast::{BinOp, Expr, ExprKind, Span, Stmt, StmtKind, UnOp},
    token::{Token, TokenKind},
};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    source: Option<String>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            source: None,
        }
    }

    pub fn new_with_source(tokens: Vec<Token>, source: &str) -> Self {
        Self {
            tokens,
            pos: 0,
            source: Some(source.to_string()),
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();

        while !self.is_eof() {
            if self.match_newline() {
                continue;
            }
            stmts.push(self.statement());
        }

        stmts
    }

    fn statement(&mut self) -> Stmt {
        if self.match_keyword("if") {
            return self.if_stmt(self.pos.saturating_sub(1));
        }

        if self.match_keyword("while") {
            return self.while_stmt(self.pos.saturating_sub(1));
        }

        if self.match_keyword("def") {
            return self.func_def(self.pos.saturating_sub(1));
        }

        if self.match_keyword("return") {
            let start_pos = self.pos.saturating_sub(1);
            let expr = self.expression();
            self.consume_newlines();
            return self.wrap_stmt(StmtKind::Return(expr), start_pos);
        }

        if self.match_keyword("import") {
            let start_pos = self.pos.saturating_sub(1);
            match self.peek().kind.clone() {
                TokenKind::String(_) => {
                    let expr = self.expression();
                    self.consume_newlines();
                    return self.wrap_stmt(StmtKind::Import(expr), start_pos);
                }
                TokenKind::Ident(_) => {
                    let name = self.consume_ident();
                    let alias = if self.match_keyword("as") {
                        self.consume_ident()
                    } else {
                        name.clone()
                    };
                    self.consume_newlines();
                    return self.wrap_stmt(StmtKind::ImportModule { name, alias }, start_pos);
                }
                _ => self.error_expected("module name or string after import"),
            }
        }

        if self.match_keyword("try") {
            return self.try_catch_stmt(self.pos.saturating_sub(1));
        }

        if self.match_keyword("export") {
            let start_pos = self.pos.saturating_sub(1);
            let name = self.consume_ident();
            self.consume_newlines();
            return self.wrap_stmt(StmtKind::Export { name }, start_pos);
        }

        if self.match_keyword("break") {
            let start_pos = self.pos.saturating_sub(1);
            self.consume_newlines();
            return self.wrap_stmt(StmtKind::Break, start_pos);
        }

        if self.match_keyword("continue") {
            let start_pos = self.pos.saturating_sub(1);
            self.consume_newlines();
            return self.wrap_stmt(StmtKind::Continue, start_pos);
        }

        // assignment: ident = expr
        if let Some(name) = self.peek_ident() {
            if self.peek_symbol('=', 1) {
                let start_pos = self.pos;
                self.advance(); // consume ident
                self.advance(); // consume '='
                let value = self.expression();
                self.consume_newlines();
                return self.wrap_stmt(StmtKind::Assign { name, value }, start_pos);
            }
        }

        if self.peek_ident().is_some() && self.peek_symbol('[', 1) {
            let start_pos = self.pos;
            let expr = self.expression();
            if self.match_symbol('=') {
                let value = self.expression();
                self.consume_newlines();
                if let ExprKind::Index { target, index } = expr.kind {
                    return self.wrap_stmt(
                        StmtKind::IndexAssign {
                            target: *target,
                            index: *index,
                            value,
                        },
                        start_pos,
                    );
                }
                panic!("Invalid assignment target");
            }
            self.consume_newlines();
            return self.wrap_stmt(StmtKind::ExprStmt(expr), start_pos);
        }

        let start_pos = self.pos;
        let expr = self.expression();
        self.consume_newlines();
        self.wrap_stmt(StmtKind::ExprStmt(expr), start_pos)
    }

    fn while_stmt(&mut self, start_pos: usize) -> Stmt {
        let condition = self.expression();
        self.consume_symbol(':');
        self.consume_newlines();
        self.consume(TokenKind::Indent);

        let mut body = Vec::new();
        while !self.check(&TokenKind::Dedent) && !self.is_eof() {
            if self.match_newline() {
                continue;
            }
            body.push(self.statement());
        }
        self.consume(TokenKind::Dedent);

        self.wrap_stmt(StmtKind::While { condition, body }, start_pos)
    }

    fn if_stmt(&mut self, start_pos: usize) -> Stmt {
        let condition = self.expression();
        self.consume_symbol(':');
        self.consume_newlines();
        self.consume(TokenKind::Indent);

        let mut then_branch = Vec::new();
        while !self.check(&TokenKind::Dedent) && !self.is_eof() {
            if self.match_newline() {
                continue;
            }
            then_branch.push(self.statement());
        }
        self.consume(TokenKind::Dedent);

        let else_branch = if self.match_keyword("else") {
            self.consume_symbol(':');
            self.consume_newlines();
            self.consume(TokenKind::Indent);

            let mut else_stmts = Vec::new();
            while !self.check(&TokenKind::Dedent) && !self.is_eof() {
                if self.match_newline() {
                    continue;
                }
                else_stmts.push(self.statement());
            }
            self.consume(TokenKind::Dedent);
            Some(else_stmts)
        } else {
            None
        };

        self.wrap_stmt(
            StmtKind::If {
                condition,
                then_branch,
                else_branch,
            },
            start_pos,
        )
    }

    fn func_def(&mut self, start_pos: usize) -> Stmt {
        let name = self.consume_ident();
        self.consume_symbol('(');

        let mut params = Vec::new();
        if !self.check_symbol(')') {
            loop {
                params.push(self.consume_ident());
                if !self.match_symbol(',') {
                    break;
                }
            }
        }
        self.consume_symbol(')');

        self.consume_symbol(':');
        self.consume_newlines();
        self.consume(TokenKind::Indent);

        let mut body = Vec::new();
        while !self.check(&TokenKind::Dedent) && !self.is_eof() {
            if self.match_newline() {
                continue;
            }
            body.push(self.statement());
        }
        self.consume(TokenKind::Dedent);

        self.wrap_stmt(StmtKind::FuncDef { name, params, body }, start_pos)
    }

    fn try_catch_stmt(&mut self, start_pos: usize) -> Stmt {
        self.consume_symbol(':');
        self.consume_newlines();
        self.consume(TokenKind::Indent);

        let mut try_body = Vec::new();
        while !self.check(&TokenKind::Dedent) && !self.is_eof() {
            if self.match_newline() {
                continue;
            }
            try_body.push(self.statement());
        }
        self.consume(TokenKind::Dedent);

        if !self.match_keyword("catch") {
            self.error_expected("'catch' after try block");
        }

        let catch_name = self.consume_ident();
        self.consume_symbol(':');
        self.consume_newlines();
        self.consume(TokenKind::Indent);

        let mut catch_body = Vec::new();
        while !self.check(&TokenKind::Dedent) && !self.is_eof() {
            if self.match_newline() {
                continue;
            }
            catch_body.push(self.statement());
        }
        self.consume(TokenKind::Dedent);

        self.wrap_stmt(
            StmtKind::TryCatch {
                try_body,
                catch_name,
                catch_body,
            },
            start_pos,
        )
    }

    // ----------------- EXPRESSIONS -----------------
    fn expression(&mut self) -> Expr {
        self.pipeline()
    }

    fn pipeline(&mut self) -> Expr {
        let mut expr = self.logical_or();
        let mut extra_indent = 0usize;

        loop {
            if self.match_symbol2("|>") {
                let right = self.logical_or();
                let span = Span::merge(expr.span, right.span);
                expr = Expr {
                    kind: ExprKind::Pipe {
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                    span,
                };
                continue;
            }

            if self.match_pipeline_continuation(&mut extra_indent) {
                let right = self.logical_or();
                let span = Span::merge(expr.span, right.span);
                expr = Expr {
                    kind: ExprKind::Pipe {
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                    span,
                };
                continue;
            }

            break;
        }

        if extra_indent > 0 {
            self.consume_newlines();
            for _ in 0..extra_indent {
                if self.check(&TokenKind::Dedent) {
                    self.advance();
                }
            }
        }

        expr
    }

    fn logical_or(&mut self) -> Expr {
        let mut expr = self.logical_and();

        while self.match_keyword("or") {
            let right = self.logical_and();
            let span = Span::merge(expr.span, right.span);
            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op: BinOp::Or,
                    right: Box::new(right),
                },
                span,
            };
        }

        expr
    }

    fn logical_and(&mut self) -> Expr {
        let mut expr = self.equality();

        while self.match_keyword("and") {
            let right = self.equality();
            let span = Span::merge(expr.span, right.span);
            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op: BinOp::And,
                    right: Box::new(right),
                },
                span,
            };
        }

        expr
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();

        loop {
            if self.match_symbol2("==") {
                let right = self.comparison();
                let span = Span::merge(expr.span, right.span);
                expr = Expr {
                    kind: ExprKind::Binary {
                        left: Box::new(expr),
                        op: BinOp::Equal,
                        right: Box::new(right),
                    },
                    span,
                };
                continue;
            }

            if self.match_symbol2("!=") {
                let right = self.comparison();
                let span = Span::merge(expr.span, right.span);
                expr = Expr {
                    kind: ExprKind::Binary {
                        left: Box::new(expr),
                        op: BinOp::NotEqual,
                        right: Box::new(right),
                    },
                    span,
                };
                continue;
            }

            break;
        }

        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.addition();

        loop {
            if self.match_symbol('>') {
                let right = self.addition();
                let span = Span::merge(expr.span, right.span);
                expr = Expr {
                    kind: ExprKind::Binary {
                        left: Box::new(expr),
                        op: BinOp::Greater,
                        right: Box::new(right),
                    },
                    span,
                };
                continue;
            }

            if self.match_symbol2(">=") {
                let right = self.addition();
                let span = Span::merge(expr.span, right.span);
                expr = Expr {
                    kind: ExprKind::Binary {
                        left: Box::new(expr),
                        op: BinOp::GreaterEq,
                        right: Box::new(right),
                    },
                    span,
                };
                continue;
            }

            if self.match_symbol('<') {
                let right = self.addition();
                let span = Span::merge(expr.span, right.span);
                expr = Expr {
                    kind: ExprKind::Binary {
                        left: Box::new(expr),
                        op: BinOp::Less,
                        right: Box::new(right),
                    },
                    span,
                };
                continue;
            }

            if self.match_symbol2("<=") {
                let right = self.addition();
                let span = Span::merge(expr.span, right.span);
                expr = Expr {
                    kind: ExprKind::Binary {
                        left: Box::new(expr),
                        op: BinOp::LessEq,
                        right: Box::new(right),
                    },
                    span,
                };
                continue;
            }

            break;
        }

        expr
    }

    fn addition(&mut self) -> Expr {
        let mut expr = self.multiplication();

        while self.match_symbol('+') {
            let right = self.multiplication();
            let span = Span::merge(expr.span, right.span);
            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op: BinOp::Add,
                    right: Box::new(right),
                },
                span,
            };
        }

        expr
    }

    fn multiplication(&mut self) -> Expr {
        let mut expr = self.unary();

        while self.match_symbol('*') {
            let right = self.unary();
            let span = Span::merge(expr.span, right.span);
            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op: BinOp::Mul,
                    right: Box::new(right),
                },
                span,
            };
        }

        expr
    }

    fn unary(&mut self) -> Expr {
        if self.match_keyword("not") || self.match_symbol('!') {
            let start_pos = self.pos.saturating_sub(1);
            let expr = self.unary();
            let span = Span::merge(self.token_span(start_pos), expr.span);
            return Expr {
                kind: ExprKind::Unary {
                    op: UnOp::Not,
                    expr: Box::new(expr),
                },
                span,
            };
        }
        self.primary()
    }

    fn primary(&mut self) -> Expr {
        let start_pos = self.pos;
        let mut expr = match self.peek().kind.clone() {
            TokenKind::Number(n) => {
                self.advance();
                self.wrap_expr(ExprKind::Number(n), start_pos)
            }
            TokenKind::String(s) => {
                self.advance();
                self.wrap_expr(ExprKind::String(s), start_pos)
            }
            TokenKind::Keyword(ref k) if k == "true" => {
                self.advance();
                self.wrap_expr(ExprKind::Bool(true), start_pos)
            }
            TokenKind::Keyword(ref k) if k == "false" => {
                self.advance();
                self.wrap_expr(ExprKind::Bool(false), start_pos)
            }
            TokenKind::Keyword(ref k) if k == "null" => {
                self.advance();
                self.wrap_expr(ExprKind::Null, start_pos)
            }
            TokenKind::Ident(name) => {
                self.advance();
                self.wrap_expr(ExprKind::Var(name), start_pos)
            }
            TokenKind::Symbol('(') => {
                self.advance();
                let expr = self.expression();
                self.consume_symbol(')');
                Expr {
                    kind: expr.kind,
                    span: self.span_from_positions(start_pos, self.current_end_pos()),
                }
            }
            TokenKind::Symbol('[') => {
                self.advance();
                let mut items = Vec::new();
                if !self.check_symbol(']') {
                    loop {
                        items.push(self.expression());
                        if !self.match_symbol(',') {
                            break;
                        }
                    }
                }
                self.consume_symbol(']');
                self.wrap_expr(ExprKind::ArrayLiteral(items), start_pos)
            }
            TokenKind::Symbol('{') => {
                self.advance();
                let mut items = Vec::new();
                if !self.check_symbol('}') {
                    loop {
                        let key = self.expression();
                        self.consume_symbol(':');
                        let value = self.expression();
                        items.push((key, value));
                        if !self.match_symbol(',') {
                            break;
                        }
                    }
                }
                self.consume_symbol('}');
                self.wrap_expr(ExprKind::MapLiteral(items), start_pos)
            }
            _ => self.error_expected("expression"),
        };

        loop {
            if self.match_symbol('(') {
                let mut args = Vec::new();

                if !self.check_symbol(')') {
                    loop {
                        args.push(self.expression());
                        if !self.match_symbol(',') {
                            break;
                        }
                    }
                }
                self.consume_symbol(')');
                let end_span = self.token_span(self.current_end_pos());
                let span = Span::merge(expr.span, end_span);
                let kind = if let ExprKind::Var(name) = &expr.kind {
                    ExprKind::Call {
                        name: name.clone(),
                        args,
                    }
                } else {
                    ExprKind::CallExpr {
                        callee: Box::new(expr),
                        args,
                    }
                };
                expr = Expr { kind, span };
                continue;
            }

            if self.match_symbol('[') {
                let index = self.expression();
                self.consume_symbol(']');
                let end_span = self.token_span(self.current_end_pos());
                let span = Span::merge(expr.span, end_span);
                expr = Expr {
                    kind: ExprKind::Index {
                        target: Box::new(expr),
                        index: Box::new(index),
                    },
                    span,
                };
                continue;
            }

            if self.match_symbol('.') {
                let name = self.consume_ident();
                let end_span = self.token_span(self.current_end_pos());
                let span = Span::merge(expr.span, end_span);
                let key = Expr {
                    kind: ExprKind::String(name),
                    span: end_span,
                };
                expr = Expr {
                    kind: ExprKind::Index {
                        target: Box::new(expr),
                        index: Box::new(key),
                    },
                    span,
                };
                continue;
            }

            break;
        }

        expr
    }

    // ----------------- HELPERS -----------------
    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_eof() {
            self.pos += 1;
        }
        &self.tokens[self.pos - 1]
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }

    fn check(&self, kind: &TokenKind) -> bool {
        &self.peek().kind == kind
    }

    fn match_newline(&mut self) -> bool {
        if self.check(&TokenKind::Newline) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_newlines(&mut self) {
        while self.match_newline() {}
    }

    fn consume(&mut self, kind: TokenKind) {
        if self.peek().kind == kind {
            self.advance();
        } else {
            self.error_expected(&token_kind_display(&kind));
        }
    }

    fn match_keyword(&mut self, k: &str) -> bool {
        match &self.peek().kind {
            TokenKind::Keyword(kw) if kw == k => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn match_symbol(&mut self, c: char) -> bool {
        match self.peek().kind {
            TokenKind::Symbol(sc) if sc == c => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn match_symbol2(&mut self, s: &str) -> bool {
        match &self.peek().kind {
            TokenKind::Symbol2(op) if op == s => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn match_pipeline_continuation(&mut self, extra_indent: &mut usize) -> bool {
        let mut i = self.pos;
        let mut saw_newline = false;
        while i < self.tokens.len() {
            match self.tokens[i].kind {
                TokenKind::Newline => {
                    saw_newline = true;
                    i += 1;
                }
                _ => break,
            }
        }

        if !saw_newline {
            return false;
        }

        let mut has_indent = false;
        if i < self.tokens.len() {
            if matches!(self.tokens[i].kind, TokenKind::Indent) {
                has_indent = true;
                i += 1;
            }
        }

        if i >= self.tokens.len() {
            return false;
        }

        if !matches!(self.tokens[i].kind, TokenKind::Symbol2(ref op) if op == "|>") {
            return false;
        }

        self.consume_newlines();
        if has_indent {
            self.consume(TokenKind::Indent);
            *extra_indent += 1;
        }
        self.match_symbol2("|>")
    }

    fn check_symbol(&self, c: char) -> bool {
        matches!(self.peek().kind, TokenKind::Symbol(sc) if sc == c)
    }

    fn peek_symbol(&self, c: char, offset: usize) -> bool {
        if self.pos + offset >= self.tokens.len() {
            return false;
        }
        matches!(self.tokens[self.pos + offset].kind, TokenKind::Symbol(sc) if sc == c)
    }

    fn consume_symbol(&mut self, c: char) {
        if self.match_symbol(c) {
            return;
        }
        self.error_expected(&format!("symbol '{}'", c));
    }

    fn consume_ident(&mut self) -> String {
        match self.peek().kind.clone() {
            TokenKind::Ident(s) => {
                self.advance();
                s
            }
            _ => self.error_expected("identifier"),
        }
    }

    fn peek_ident(&self) -> Option<String> {
        match self.peek().kind.clone() {
            TokenKind::Ident(s) => Some(s),
            _ => None,
        }
    }

    fn current_end_pos(&self) -> usize {
        if self.pos == 0 {
            0
        } else {
            self.pos - 1
        }
    }

    fn span_from_positions(&self, start_pos: usize, end_pos: usize) -> Span {
        let start = &self.tokens[start_pos];
        let end = &self.tokens[end_pos];
        Span {
            start_line: start.line,
            start_col: start.col,
            end_line: end.line,
            end_col: end.col,
        }
    }

    fn token_span(&self, pos: usize) -> Span {
        self.span_from_positions(pos, pos)
    }

    fn wrap_expr(&self, kind: ExprKind, start_pos: usize) -> Expr {
        let end_pos = self.current_end_pos();
        Expr {
            kind,
            span: self.span_from_positions(start_pos, end_pos),
        }
    }

    fn wrap_stmt(&self, kind: StmtKind, start_pos: usize) -> Stmt {
        let end_pos = self.current_end_pos();
        Stmt {
            kind,
            span: self.span_from_positions(start_pos, end_pos),
        }
    }

    fn error_expected(&self, expected: &str) -> ! {
        let msg = self.format_error(expected, self.peek());
        panic!("{}", msg);
    }

    fn format_error(&self, expected: &str, got: &Token) -> String {
        let mut out = String::new();
        out.push_str("Syntax error: expected ");
        out.push_str(expected);
        out.push_str(", got ");
        out.push_str(&token_kind_display(&got.kind));
        out.push_str(&format!(" at line {} col {}", got.line, got.col));

        if let Some(source) = &self.source {
            let mut lines = source.lines();
            if let Some(line) = lines.nth(got.line.saturating_sub(1)) {
                out.push_str("\n");
                out.push_str(line);
                out.push_str("\n");
                if got.col > 0 {
                    let caret_pos = got.col - 1;
                    for _ in 0..caret_pos {
                        out.push(' ');
                    }
                    out.push('^');
                }
            }
        }

        out
    }
}

fn token_kind_display(kind: &TokenKind) -> String {
    match kind {
        TokenKind::Number(n) => format!("number {}", n),
        TokenKind::String(s) => format!("string \"{}\"", s),
        TokenKind::Ident(s) => format!("identifier {}", s),
        TokenKind::Keyword(k) => format!("keyword {}", k),
        TokenKind::Symbol(c) => format!("symbol '{}'", c),
        TokenKind::Symbol2(s) => format!("symbol '{}'", s),
        TokenKind::Newline => "newline".to_string(),
        TokenKind::Indent => "indent".to_string(),
        TokenKind::Dedent => "dedent".to_string(),
        TokenKind::Eof => "end of file".to_string(),
    }
}
