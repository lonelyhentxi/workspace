pub fn revrot(s: &str, c: usize) -> String {
    if s.is_empty() || c == 0 {
        return "".to_string();
    }
    const MOD: i32 = 2;
    const ZERO_ASCII: i32 = '0' as i32;
    let mut res = String::with_capacity(s.len());
    let parts: usize = s.len() / c;
    for i in 0..parts {
        let part_start_index = i * c;
        let ss = &s[part_start_index..part_start_index + c];
        // because here is to mod two ,you can simplify it, else must pow 3:
        let acc: i32 = ss.chars()
            .map(|c| (c as i32 - ZERO_ASCII))
            .sum();
        if acc % MOD == 1 {
            res = res + &ss[1..] + &ss[0..1];
        } else {
            res += &(ss.chars().rev().collect::<String>());
        }
    }
    res
}


#[cfg(test)]
mod tests {
    fn testing(s: &str, c: usize, exp: &str) {
        assert_eq!(&super::revrot(s, c), exp);
    }

    #[test]
    fn basics_revrot() {
        testing("1234", 0, "");
        testing("", 0, "");
        testing("1234", 5, "");
        let s = "733049910872815764";
        testing(s, 5, "330479108928157");
        let s = "73304991087281576455176044327690580265896";
        testing(s, 8, "1994033775182780067155464327690480265895");
    }
}