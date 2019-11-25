/*
 * @lc app=leetcode.cn id=208 lang=rust
 *
 * [208] 实现 Trie (前缀树)
 */

// @lc code=start
use std::rc::Rc;
use std::cell::RefCell;
use std::mem::swap;

const ZERO_ASCII: usize = 'a' as usize;

#[derive(Clone,Debug,PartialEq,Eq)]
struct TrieNode {
    children: Vec<Option<Rc<RefCell<TrieNode>>>>,
    terminal: bool,
    value: Vec<usize>,
}

#[derive(Clone,Debug,PartialEq,Eq)]
struct Trie {
    root: Rc<RefCell<TrieNode>>,
}


/** 
 * `&self` means the method takes an immutable reference.
 * If you need a mutable reference, change it to `&mut self` instead.
 */
impl Trie {

    /** Initialize your data structure here. */
    fn new() -> Self {
        Trie {
            root: Rc::new(RefCell::new(TrieNode {
                children: vec![None; 26],
                terminal: false,
                value: vec![],
            }))
        }
    }

    #[allow(unused_assignments)]
    fn diff(&self, elements: &[usize]) -> (Rc<RefCell<TrieNode>>, usize, usize) {
        let mut current = self.root.clone();
        let mut i = 0;
        let mut j = 0;
        'outer: loop {
            j = 0;
            while i<elements.len() && j<current.borrow().value.len() {
                if elements[i] != current.borrow().value[j] {
                    break 'outer;
                }
                i+=1;
                j+=1;
            }
            if i<elements.len() {
                let option = current.borrow().children[elements[i]].clone();
                match option {
                    Some(next) => current = next.clone(),
                    None => break 'outer, 
                }
            }
            else {
               break 'outer; 
            }
        }
        (current, i, j)
    }

    fn ascii_to_usize(s: String) -> Vec<usize> {
        s.as_bytes().iter().map(|c| *c as usize - ZERO_ASCII).collect()
    }
    
    /** Inserts a word into the trie. */
    fn insert(&mut self, word: String) {
        let chars = Trie::ascii_to_usize(word);
        let (last_same_node, first_diff_i, first_diff_j) = self.diff(&chars);
        let mut node_mut_ref = last_same_node.borrow_mut();
        if first_diff_j<node_mut_ref.value.len() {
            let front = node_mut_ref.value[0..first_diff_j].to_vec();
            let raw_end = node_mut_ref.value[first_diff_j..node_mut_ref.value.len()].to_vec();
            let first_diff_j_value = node_mut_ref.value[first_diff_j];
            node_mut_ref.value = front;
            let mut new_children = vec![None;26];
            let mut new_terminal = false;
            swap(&mut new_children, &mut node_mut_ref.children);
            swap(&mut new_terminal, &mut node_mut_ref.terminal);
            node_mut_ref.children[first_diff_j_value] = Some(
                Rc::new(RefCell::new(
                    TrieNode {
                        value: raw_end,
                        children: new_children,
                        terminal: new_terminal,
                    }
                ))
            );
        }
        if first_diff_i<chars.len() {
            let new_end = chars[first_diff_i..chars.len()].to_vec();
            let first_diff_i_value = chars[first_diff_i];
            node_mut_ref.children[first_diff_i_value] = Some(
                Rc::new(RefCell::new(
                    TrieNode {
                        value: new_end,
                        children: vec![None;26],
                        terminal: true,
                    }
                ))
            );
        } 
        else if !node_mut_ref.terminal {
            node_mut_ref.terminal = true;   
        }
    }
    
    /** Returns if the word is in the trie. */
    fn search(&self, word: String) -> bool {
        let chars = Trie::ascii_to_usize(word);
        let (last_same_node, first_diff_i, first_diff_j) = self.diff(&chars);
        first_diff_i==chars.len() 
            && first_diff_j==last_same_node.borrow().value.len() 
            && last_same_node.borrow().terminal
    }
    
    /** Returns if there is any word in the trie that starts with the given prefix. */
    fn starts_with(&self, prefix: String) -> bool {
        let chars = Trie::ascii_to_usize(prefix);
        let (_, first_diff_i, _) = self.diff(&chars);
        first_diff_i==chars.len()
        
    }
}

//
// Your Trie object will be instantiated and called as such:
// let obj = Trie::new();
// obj.insert(word);
// let ret_2: bool = obj.search(word);
// let ret_3: bool = obj.starts_with(prefix);
//
// @lc code=end

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_trie1() {
        let mut trie = Trie::new();
        trie.insert("apple".to_string());
        assert!(trie.search("apple".to_string()));
        assert!(!trie.search("app".to_string()));
        assert!(trie.starts_with("app".to_string()));
        trie.insert("app".to_string());   
        assert!(trie.search("app".to_string()));
    }

    #[test]
    fn test_trie2() {
        let mut trie = Trie::new();
        trie.insert("emplacement".to_string());
        trie.insert("emporia".to_string());
        trie.insert("embowed".to_string());
        assert!(!trie.search("em".to_string()));
    }
}