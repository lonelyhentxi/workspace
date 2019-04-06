extern crate num;
use super::super::sixkyu::decode_the_morse_code::MorseDecoder;
use num::integer::gcd;
use std::collections::HashSet;

impl MorseDecoder {
    pub fn decode_bits(&self, encoded: &str) -> String {
        let mut zero_lengths = HashSet::new();
        let mut one_lengths = HashSet::new();
        let mut length = 0;
        let mut sum_1 = 0;
        let mut sum_0 = 0;
        let trimed = encoded.trim_matches('0').chars();
        for c in trimed.clone() {
            if c == '1' {
                sum_1 += 1;
                zero_lengths.insert(sum_0);
                sum_0 = 0;
            } else {
                sum_0 += 1;
                one_lengths.insert(sum_1);
                sum_1 = 0;
            }
        }
        zero_lengths.insert(sum_0);
        one_lengths.insert(sum_1);
        zero_lengths.remove(&0);
        one_lengths.remove(&0);
        println!("{:?} {:?}", zero_lengths, one_lengths);
        if one_lengths.len() == 2 {
            length = *one_lengths.iter().min().unwrap();
        } else if zero_lengths.len() == 3 {
            length = *zero_lengths.iter().min().unwrap();
        } else if zero_lengths.len() == 2 {
            let zero_length_vec = zero_lengths.into_iter().collect::<Vec<i32>>();
            length = gcd(zero_length_vec[0], zero_length_vec[1]);
        } else if zero_lengths.len() == 1 {
            // if one_lengths.len() == 1 {
            let zero_length = *zero_lengths.iter().next().unwrap();
            let one_length = *one_lengths.iter().next().unwrap();
            if zero_length == one_length {
                // 111000111 will be think as 101
                length = zero_length;
            } else {
                length = gcd(zero_length, one_length);
            }
        // }  else if one_lengths.len() == 0 {
        // trimed, "0*" will not existed
        // return unreachable!();
        // }
        } else if one_lengths.len() == 1 {
            length = *one_lengths.iter().next().unwrap();
        // 111 will be think as 1
        } else if one_lengths.is_empty() {
            return "".to_string();
        }
        let trimed_vec = trimed.collect::<Vec<char>>();
        let mut res = String::new();
        let length = length as usize;
        for i in 0..(trimed_vec.len() / length) {
            res.push(trimed_vec[i * length]);
        }
        println!("{:?} {:?}", res, length);
        res = res
            .split("0000000")
            .map(|word| {
                word.split("000")
                    .map(|ch| {
                        ch.split('0')
                            .map(|token| {
                                if token == "1" {
                                    ".".to_string()
                                } else if token == "111" {
                                    "-".to_string()
                                } else {
                                    "".to_string()
                                }
                            })
                            .collect::<Vec<String>>()
                            .join("")
                    })
                    .collect::<Vec<String>>()
                    .join(" ")
            })
            .collect::<Vec<String>>()
            .join("   ");
        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn examples() {
        let decoder = MorseDecoder::new();
        assert_eq!(decoder.decode_morse(&decoder.decode_bits("1100110011001100000011000000111111001100111111001111110000000000000011001111110011111100111111000000110011001111110000001111110011001100000011")), "HEY JUDE".to_string());
        assert_eq!(decoder.decode_morse(&decoder.decode_bits("10001")), "EE");
    }
}
