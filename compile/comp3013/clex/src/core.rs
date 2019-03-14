use super::pattern::gen_patterns;

pub fn lex(sstream:&str) -> Option<Vec<(usize,Option<String>, &str)>> {
    let lex_reg_action_pairs = gen_patterns();
    let mut start: usize = 0u32 as usize;
    let mut results = Vec::new();
    while start < sstream.len() {
        let mut len: usize = usize::min(64, sstream.len() - start);
        while len > 0  {
            let mut matched = false;
            let slice = &sstream[start..start+len];
            for pair in &lex_reg_action_pairs {
                if (pair.1).reg_match(slice) {
                    results.push((pair.0,pair.2(slice), slice));
                    matched = true;
                    break;
                }
            }
            if matched==true {
                start+=len;
                break;
            }
            len-=1;
        }
        if len==0 {
            return None;
        }
    }
    Some(results)
}