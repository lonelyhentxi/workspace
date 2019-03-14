pub mod core;

#[cfg(test)]
mod tests {
    use super::core::lex;
    [test]
    fn test_all() {
        let lex_reg_action_pairs = lex();

    }
}