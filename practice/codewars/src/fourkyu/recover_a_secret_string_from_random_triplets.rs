use std::collections::HashSet;

pub fn recover_secret(triplets: Vec<[char;3]>) -> String {
    let mut chars: Vec<char> = vec![];
    for line in &triplets {
        for c in line.iter() {
            chars.push(c.clone());
        }
    }
    let mut counter:HashSet<char> = HashSet::new();
    chars = chars.into_iter().filter(|x| if !counter.contains(x) {  counter.insert(x.clone()); true } else { false } ).collect();
    loop {
        let mut count = 0;
        for line in &triplets {
            for j in 0..line.len()-1 {
                let a = &line[j];
                let b = &line[j+1];
                let index_of_a = chars.iter().position(|p|a==p).unwrap();
                let index_of_b = chars.iter().position(|p|b==p).unwrap();
                if index_of_a > index_of_b {
                    chars.swap(index_of_b,index_of_a);
                    count+=1;
                }
            }
        }
        if count==0 {
            return chars.into_iter().collect();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn example_test() {
        assert_eq!(recover_secret(vec![
            ['t','u','p'],
            ['w','h','i'],
            ['t','s','u'],
            ['a','t','s'],
            ['h','a','p'],
            ['t','i','s'],
            ['w','h','s']])
                   , "whatisup");
    }
}