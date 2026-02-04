#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Number(f64),
    String(String),
    Ident(String),
    Keyword(String),
    Symbol(char),
    /// Two-character operators/symbols like ==, !=, <=, >=
    Symbol2(String),
    Newline,
    Indent,
    Dedent,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub col: usize,
}
