pub trait IToken {
    fn kind(&self) -> &String;
    fn lexeme(&self) -> &String;
}

impl IToken for (String, String) {
    fn kind(&self) -> &String {
        &self.0
    }

    fn lexeme(&self) -> &String {
        &self.1
    }
}
