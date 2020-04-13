use regex::Regex;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TokenType {
    If,
    Then,
    Else,
    For,
    In,
    OpeningParenthesis,
    ClosingParenthesis,
    Comma,
    Delimiter,
    Def,
    Extern,
    Binary,
    Unary,
    Var,
    Ident(String),
    Number(f64),
    Operator(String),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Token {
    pub kind: TokenType,
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

lazy_static! {
    static ref COMMENT_REG: Regex = Regex::new(r"(?m)#.*\n").unwrap();
    static ref TOKEN_REG: Regex = Regex::new(concat!(
        r"(?P<ident>\p{Alphabetic}\w*)|",
        r"(?P<number>\d+\.?\d*)|",
        r"(?P<delimiter>;)|",
        r"(?P<oppar>\()|",
        r"(?P<clpar>\))|",
        r"(?P<comma>,)|",
        r"(?P<operator>\S)"
    ))
    .unwrap();
}

// line number now have no usage
pub fn tokenize(input: &str, start_line: usize) -> Vec<Token> {
    let mut result: Vec<Token> = vec![];
    let preprocessed = COMMENT_REG.replace_all(input, "\n");
    for (idx, line) in preprocessed.lines().enumerate() {
        let idx = start_line + idx;
        for cap in TOKEN_REG.captures_iter(line) {
            if let Some(ref ident) = cap.name("ident") {
                result.push(Token {
                    kind: match ident.as_str() {
                        "def" => TokenType::Def,
                        "extern" => TokenType::Extern,
                        "if" => TokenType::If,
                        "then" => TokenType::Then,
                        "else" => TokenType::Else,
                        "for" => TokenType::For,
                        "in" => TokenType::In,
                        "binary" => TokenType::Binary,
                        "unary" => TokenType::Unary,
                        "var" => TokenType::Var,
                        text => TokenType::Ident(text.to_string()),
                    },
                    line: idx,
                    start: ident.start(),
                    end: ident.end(),
                });
            }
            if let Some(ref number_match) = cap.name("number") {
                match number_match.as_str().parse() {
                    Ok(number) => result.push(Token {
                        kind: TokenType::Number(number),
                        line: idx,
                        start: number_match.start(),
                        end: number_match.end(),
                    }),
                    Err(_) => panic!("Lexer failed trying to parse number"),
                }
            }
            if let Some(ref delimiter) = cap.name("delimiter") {
                result.push(Token {
                    kind: TokenType::Delimiter,
                    line: idx,
                    start: delimiter.start(),
                    end: delimiter.end(),
                });
            }
            if let Some(ref oppar) = cap.name("oppar") {
                result.push(Token {
                    kind: TokenType::OpeningParenthesis,
                    line: idx,
                    start: oppar.start(),
                    end: oppar.end(),
                });
            }
            if let Some(ref clpar) = cap.name("clpar") {
                result.push(Token {
                    kind: TokenType::ClosingParenthesis,
                    line: idx,
                    start: clpar.start(),
                    end: clpar.end(),
                });
            }
            if let Some(ref comma) = cap.name("comma") {
                result.push(Token {
                    kind: TokenType::Comma,
                    line: idx,
                    start: comma.start(),
                    end: comma.end(),
                });
            }
            if let Some(ref operator) = cap.name("operator") {
                result.push(Token {
                    kind: TokenType::Operator(operator.as_str().to_string()),
                    line: idx,
                    start: operator.start(),
                    end: operator.end(),
                });
            }
        }
    }
    result
}
