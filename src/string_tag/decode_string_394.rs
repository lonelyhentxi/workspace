/*
 * @lc app=leetcode.cn id=394 lang=rust
 *
 * [394] 字符串解码
 */

// @lc code=start
/*
    E => DE | ME | ε 
    D => \d+P
    P => \[M\]
    M => [a-zA-Z]E
 */
#[derive(Debug, Clone, PartialEq)]
enum Token {
    Term(String),
    Number(u32),
    OpeningParenthesis, 
    ClosingParenthesis,
}

enum ASTNode {
    Leaf(String),
    Branch(ASTBranch),
}

struct ASTBranch {
    times: u32,
    children: Vec<ASTNode>,
}

struct Parser;

impl Parser {
    fn tokenize(input: String) -> Vec<Token> {
        let chars = input.chars().collect::<Vec<char>>();
        let mut i = 0;
        let len = chars.len();
        let mut tokens = vec![];
        while i<len {
            let c = chars[i];
            if c.is_alphabetic() {
                let mut buffer = String::new();
                while i<len && chars[i].is_alphabetic() {
                    buffer.push(chars[i]);
                    i+=1;
                }
                tokens.push(Token::Term(buffer));
            }
            else if c.is_digit(10) {
                let mut buffer = String::new();
                while i<len && chars[i].is_digit(10) {
                    buffer.push(chars[i]);
                    i+=1;
                }
                tokens.push(Token::Number(buffer.parse::<u32>().unwrap()));
            }
            else if c == '[' {
                tokens.push(Token::OpeningParenthesis);
                i+=1;
            } else if c == ']' {
                tokens.push(Token::ClosingParenthesis);
                i+=1;
            } else {
                unreachable!();
            }
        }
        tokens
    }

    fn parse(tokens: &[Token]) -> ASTNode {
        ASTNode::Branch (
            ASTBranch {
                times: 1,
                children: Parser::parse_expression(tokens, 0, tokens.len())
            }
        )
    }

    fn parse_expression(tokens: &[Token], lo: usize, hi: usize) -> Vec<ASTNode> {
        let mut res = vec![];
        let mut i=lo;
        while i<hi {
            let token = &tokens[i];
            match token {
                Token::Term(term) => {
                    res.push(Parser::parse_term(term.clone()));
                    i+=1;
                },
                Token::Number(num) => {
                    let (node, repeat_end) = Parser::parse_repeat(tokens, *num, i, hi);
                    res.push(node);
                    i = repeat_end;
                },
                _ => unreachable!(),
            }
        }
        res
    }

    fn parse_term(term: String) -> ASTNode {
        ASTNode::Leaf(term)
    }

    fn parse_repeat(tokens: &[Token], repeat: u32, lo: usize, hi: usize) -> (ASTNode,usize) {
        let (children, parenthesis_end) = Parser::parse_parenthesis_expr(tokens, lo+1, hi);
        (ASTNode::Branch(ASTBranch { times: repeat, children}), parenthesis_end)
    }

    fn parse_parenthesis_expr(tokens: &[Token], lo: usize, hi: usize) -> (Vec<ASTNode>, usize) {
        let mut i = lo;
        let mut openings = 0;
        while i<hi {
            let token = &tokens[i];
            i+=1;
            match token {
                Token::OpeningParenthesis => {
                    openings += 1;
                },
                Token::ClosingParenthesis => {
                    if openings==0 {
                        panic!("invalid parenthesis");
                    } else {
                        openings -= 1;
                        if openings==0 {
                            break;
                        }
                    }
                },
                _ => continue
            }
        }
        if openings==0 {
            (Parser::parse_expression(tokens,lo+1,i-1),i)
        } else {
            panic!("invalid parenthesis");
        }
    }

    fn print_ast(node: &ASTNode) -> String {
        match node {
            ASTNode::Leaf(leaf) => leaf.to_string(),
            ASTNode::Branch(branch) => {
                let once = branch.children.iter()
                    .map(|node| Parser::print_ast(node)).collect::<String>();
                once.repeat(branch.times as usize)
            }
        }
    }
}

impl Solution {
    pub fn decode_string(s: String) -> String {
        let tokens = Parser::tokenize(s);
        let ast = Parser::parse(&tokens);
        Parser::print_ast(&ast)
    }
}
// @lc code=end

struct Solution;

#[cfg(test)]
mod test {
    use super::*;
    
    #[test]
    fn test_decode_string() {
        assert_eq!(Solution::decode_string("3[a]2[bc]".to_string()), "aaabcbc".to_string());
        assert_eq!(Solution::decode_string("3[a2[c]]".to_string()), "accaccacc".to_string());
        assert_eq!(Solution::decode_string("2[abc]3[cd]ef".to_string()), "abcabccdcdcdef".to_string());
        assert_eq!(Solution::decode_string("".to_string()),"".to_string());
    }
}