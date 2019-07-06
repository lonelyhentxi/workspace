use std::cmp::Ordering::*;

pub const GOOD_WORTH: [u32; 6] = [1u32, 2, 3, 3, 4, 10];
pub const EVIL_WORTH: [u32; 7] = [1u32, 2, 2, 2, 3, 5, 10];
pub const GOOD_WIN_PROMPT: &str = "Battle Result: Good triumphs over Evil";
pub const EVIL_WIN_PROMPT: &str = "Battle Result: Evil eradicates all trace of Good";
pub const NO_WIN_PROMPT: &str = "Battle Result: No victor on this battle field";


fn staff_count(staff: &str, worth: &[u32]) -> u32 {
        staff.split_whitespace()
            .enumerate()
            .map(|(index,content)|
                worth[index]* content.parse::<u32>().unwrap())
            .sum()
}

pub fn good_vs_evil<'a>(good: &str, evil: &str) -> &'a str {
    match staff_count(good, &GOOD_WORTH)
        .cmp(&staff_count(evil, &EVIL_WORTH)) {
        Greater => GOOD_WIN_PROMPT,
        Equal => NO_WIN_PROMPT,
        Less => EVIL_WIN_PROMPT,
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn returns_expected() {
        use super::good_vs_evil;
        assert_eq!(good_vs_evil("0 0 0 0 0 10", "0 0 0 0 0 0 0"), "Battle Result: Good triumphs over Evil");
        assert_eq!(good_vs_evil("0 0 0 0 0 0", "0 0 0 0 0 0 10"), "Battle Result: Evil eradicates all trace of Good");
        assert_eq!(good_vs_evil("0 0 0 0 0 10", "0 0 0 0 0 0 10"), "Battle Result: No victor on this battle field");
    }
}