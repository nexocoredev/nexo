use crate::token::{Token, TokenKind};

pub struct Lexer {
    input: String,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.to_string(),
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        let mut chars = self.input.chars().peekable();

        let mut indent_stack: Vec<usize> = vec![0];
        let mut line_no = 1usize;
        let mut col_no = 1usize;
        let mut at_line_start = true;

        while let Some(&c) = chars.peek() {
            // --------- whitespace / newlines ---------
            if c == '\r' {
                chars.next();
                col_no += 1;
                continue;
            }

            if c == '\n' {
                tokens.push(Token {
                    kind: TokenKind::Newline,
                    line: line_no,
                    col: col_no,
                });
                chars.next();
                line_no += 1;
                col_no = 1;
                at_line_start = true;
                continue;
            }

            if at_line_start {
                // measure indentation
                let mut spaces = 0usize;
                while let Some(&' ') = chars.peek() {
                    chars.next();
                    spaces += 1;
                    col_no += 1;
                }

                if let Some(&next) = chars.peek() {
                    if next == '\n' || next == '\r' || next == '#' {
                        at_line_start = false;
                        continue;
                    }
                }

                let current = *indent_stack.last().unwrap();
                if spaces > current {
                    indent_stack.push(spaces);
                    tokens.push(Token {
                        kind: TokenKind::Indent,
                        line: line_no,
                        col: 1,
                    });
                } else if spaces < current {
                    while let Some(&top) = indent_stack.last() {
                        if spaces < top {
                            indent_stack.pop();
                            tokens.push(Token {
                                kind: TokenKind::Dedent,
                                line: line_no,
                                col: 1,
                            });
                        } else {
                            break;
                        }
                    }
                    if *indent_stack.last().unwrap() != spaces {
                        return Err(format!(
                            "Indentation error at line {} col 1: got {}, expected {}",
                            line_no,
                            spaces,
                            indent_stack.last().unwrap()
                        ));
                    }
                }

                at_line_start = false;
                continue;
            }

            if c.is_whitespace() {
                chars.next();
                col_no += 1;
                continue;
            }

            // --------- comments ---------
            if c == '#' {
                while let Some(&ch) = chars.peek() {
                    if ch == '\n' || ch == '\r' {
                        break;
                    }
                    chars.next();
                    col_no += 1;
                }
                continue;
            }

            // --------- numbers ---------
            if c.is_ascii_digit() {
                let start_col = col_no;
                let mut num = String::new();
                while let Some(&d) = chars.peek() {
                    if d.is_ascii_digit() || d == '.' {
                        num.push(d);
                        chars.next();
                        col_no += 1;
                    } else {
                        break;
                    }
                }
                let value: f64 = num.parse().map_err(|_| {
                    format!(
                        "Invalid number '{}' at line {} col {}",
                        num, line_no, start_col
                    )
                })?;
                tokens.push(Token {
                    kind: TokenKind::Number(value),
                    line: line_no,
                    col: start_col,
                });
                continue;
            }

            // --------- strings ---------
            if c == '"' {
                let start_col = col_no;
                chars.next(); // consume "
                col_no += 1;
                let mut s = String::new();
                while let Some(ch) = chars.next() {
                    if ch == '"' {
                        col_no += 1;
                        break;
                    }
                    if ch == '\n' {
                        return Err(format!(
                            "Unterminated string at line {} col {}",
                            line_no, start_col
                        ));
                    }
                    if ch == '\\' {
                        col_no += 1;
                        let esc = chars.next().ok_or_else(|| {
                            format!("Unterminated string at line {} col {}", line_no, start_col)
                        })?;
                        col_no += 1;
                        match esc {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            'r' => s.push('\r'),
                            '"' => s.push('"'),
                            '\\' => s.push('\\'),
                            other => s.push(other),
                        }
                        continue;
                    }
                    s.push(ch);
                    col_no += 1;
                }
                tokens.push(Token {
                    kind: TokenKind::String(s),
                    line: line_no,
                    col: start_col,
                });
                continue;
            }

            // --------- identifiers / keywords ---------
            if c.is_ascii_alphabetic() || c == '_' || c == '¿' || c == '¡' {
                let start_col = col_no;
                let mut id = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_ascii_alphanumeric()
                        || ch == '_'
                        || ch == '¿'
                        || ch == '¡'
                        || ch == '?'
                        || ch == 'á'
                        || ch == 'é'
                        || ch == 'í'
                        || ch == 'ó'
                        || ch == 'ú'
                        || ch == 'Á'
                        || ch == 'É'
                        || ch == 'Í'
                        || ch == 'Ó'
                        || ch == 'Ú'
                    {
                        id.push(ch);
                        chars.next();
                        col_no += 1;
                    } else {
                        break;
                    }
                }

                let kind = match id.as_str() {
                    "if" | "else" | "while" | "and" | "or" | "not" | "true" | "false" | "null"
                    | "def" | "return" | "break" | "continue" | "import" | "try" | "catch"
                    | "as" | "export" => TokenKind::Keyword(id),
                    _ => TokenKind::Ident(id),
                };

                tokens.push(Token {
                    kind,
                    line: line_no,
                    col: start_col,
                });
                continue;
            }

            // --------- two-char operators ---------
            if matches!(c, '=' | '!' | '<' | '>' | '|') {
                let start_col = col_no;
                let first = c;
                chars.next();
                col_no += 1;

                if first == '|' {
                    if let Some('>') = chars.peek().copied() {
                        chars.next();
                        col_no += 1;
                        tokens.push(Token {
                            kind: TokenKind::Symbol2("|>".to_string()),
                            line: line_no,
                            col: start_col,
                        });
                        continue;
                    }
                    return Err(format!(
                        "Unexpected character '{}' at line {} col {}",
                        first, line_no, start_col
                    ));
                }

                if let Some('=') = chars.peek().copied() {
                    chars.next();
                    col_no += 1;
                    tokens.push(Token {
                        kind: TokenKind::Symbol2(format!("{}=", first)),
                        line: line_no,
                        col: start_col,
                    });
                } else {
                    // single-char versions
                    tokens.push(Token {
                        kind: TokenKind::Symbol(first),
                        line: line_no,
                        col: start_col,
                    });
                }
                continue;
            }

            // --------- other symbols ---------
            match c {
                '(' | ')' | ',' | ':' | '+' | '*' | '[' | ']' | '{' | '}' | '.' => {
                    let start_col = col_no;
                    tokens.push(Token {
                        kind: TokenKind::Symbol(c),
                        line: line_no,
                        col: start_col,
                    });
                    chars.next();
                    col_no += 1;
                }
                _ => {
                    return Err(format!(
                        "Unexpected character '{}' at line {} col {}",
                        c, line_no, col_no
                    ))
                }
            }
        }

        // finalize
        while indent_stack.len() > 1 {
            indent_stack.pop();
            tokens.push(Token {
                kind: TokenKind::Dedent,
                line: line_no,
                col: 1,
            });
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            line: line_no,
            col: col_no,
        });

        Ok(tokens)
    }
}
