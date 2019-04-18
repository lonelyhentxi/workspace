use std::collections::HashMap;

pub trait LexPattern {
    fn get_boundary(&self) -> (usize, usize);
    fn hook(&self, table: &LexTable, target: &str) -> usize;
    fn register(&self, table: &mut LexTable);
    fn is_match(&self, target: &str) -> bool;
}

pub struct LexTable(pub HashMap<String, (usize, String, String)>);

impl LexTable {
    pub fn new() -> LexTable {
        LexTable(HashMap::new())
    }

    pub fn match_index<P: LexPattern>(&mut self, pattern: P, target: &str) -> Option<usize> {
        if pattern.is_match(target) {
            Some(pattern.hook(self, target))
        } else {
            None
        }
    }

    pub fn try_insert(&mut self, type_name: &str, super_name: &str) -> bool {
        if !self.0.contains_key(type_name) {
            self.0.insert(
                type_name.to_string(),
                (self.0.len(), type_name.to_string(), super_name.to_string()),
            );
            true
        } else {
            false
        }
    }

    pub fn get(&self, target: &str) -> &(usize, String, String) {
        &self.0[target]
    }
}
