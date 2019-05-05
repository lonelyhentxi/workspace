use crate::core::{LexPattern, LexTable};
use std::collections::btree_map::BTreeMap;
use std::collections::HashSet;
use std::sync::Arc;

pub fn merge_pattern(lex_patterns: &Vec<Arc<dyn LexPattern>>) -> BTreeMap<usize, Vec<usize>> {
    let mut points: BTreeMap<usize, Vec<(usize, bool)>> = BTreeMap::new();
    let mut index = 0usize;
    for pat in lex_patterns {
        let exit = pat.get_boundary().0;
        let entry = pat.get_boundary().1;
        assert!(exit > 0);
        let exit_next = exit - 1;
        if !points.contains_key(&exit_next) {
            points.insert(exit_next, vec![(index, false)]);
        } else {
            points.get_mut(&exit_next).unwrap().push((index, false));
        }
        if !points.contains_key(&entry) {
            points.insert(entry, vec![(index, true)]);
        } else {
            points.get_mut(&entry).unwrap().push((index, true));
        }
        index += 1;
    }
    let mut patterns: BTreeMap<usize, Vec<usize>> = BTreeMap::new();
    let mut current_patterns: HashSet<usize> = HashSet::new();
    for (key, value) in points.iter().rev() {
        for (pattern_index, is_entry) in value {
            if *is_entry {
                current_patterns.insert(*pattern_index);
            } else {
                current_patterns.remove(pattern_index);
            }
        }
        let mut copy_vec = current_patterns
            .iter()
            .map(|x| x.clone())
            .collect::<Vec<usize>>();
        copy_vec.sort();
        patterns.insert(*key, copy_vec);
    }
    return patterns;
}

pub fn lex(
    lex_patterns: Vec<Arc<dyn LexPattern>>,
    lex_table: &LexTable,
    sstream: &str,
) -> Result<Vec<(usize, String)>, String> {
    let patterns_seq = merge_pattern(&lex_patterns);
    let mut tokens: Vec<(usize, String)> = vec![];
    let patterns_seq_iter: Vec<(&usize, &Vec<usize>)> = patterns_seq.iter().rev().collect();
    let mut start: usize = 0usize;
    while start < sstream.len() {
        let mut matched = false;
        let mut len = 0;
        for i in 0..patterns_seq_iter.len() - 1 {
            len = usize::min(sstream.len() - start, *patterns_seq_iter[i].0);
            while len > *patterns_seq_iter[i + 1].0 {
                for pattern_index in patterns_seq_iter[i].1 {
                    let pattern = lex_patterns.get(*pattern_index).unwrap();
                    let slice = &sstream[start..start + len];
                    if pattern.is_match(slice) {
                        tokens.push((pattern.hook(lex_table, slice), slice.to_string()));
                        matched = true;
                        break;
                    }
                }
                if matched {
                    break;
                } else {
                    len -= 1;
                }
            }
            if matched {
                break;
            }
        }
        if matched {
            start += len;
            continue;
        } else {
            return Err(sstream[start..usize::min(sstream.len(), start + 64)].to_string());
        }
    }
    Ok(tokens)
}
