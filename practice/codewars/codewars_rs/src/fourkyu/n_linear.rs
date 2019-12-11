use std::collections::BinaryHeap;
use std::cmp::Ordering;

#[derive(Eq, Debug)]
struct MBox {
    index: usize,
    value: u32,
}

impl PartialOrd for MBox {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for MBox {
    fn cmp(&self, other: &Self) -> Ordering {
        self.value.cmp(&other.value).reverse()
    }
}

impl PartialEq for MBox {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

pub fn n_linear(m: &[u32], n: usize) -> u32 {
    let mut m = m.to_vec();
    m.sort();
    let mut arr = vec![1];
    arr.reserve(n + 1 as usize);
    let mut m_indices = vec![0usize; m.len()];
    let mut m_current = BinaryHeap::new();
    for (i,item) in m.iter().enumerate() {
        let value = arr[0] * item + 1;
        m_current.push(
            MBox { index: i, value });
    }
    while arr.len() <= n {
        let mut m_min = m_current.pop().unwrap();
        let next_value =
            arr[m_indices[m_min.index]] * m[m_min.index] + 1;
        m_min.value = next_value;
        m_current.push(m_min);
        if m_current.peek().unwrap().value != arr[arr.len() - 1] {
            arr.push(m_current.peek().unwrap().value);
        } else {
            m_indices[m_current.peek().unwrap().index] += 1;
        }
    }
    arr[n]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pair_test() {
        assert_eq!(n_linear(&[2, 3], 10), 22);
        assert_eq!(n_linear(&[3, 2], 10), 22);
    }

    #[test]
    fn triplet_test() {
        assert_eq!(n_linear(&[5, 7, 8], 10), 64);
        assert_eq!(n_linear(&[5, 7, 8], 11), 65);
    }

    #[test]
    fn random_test() {
        assert_eq!(n_linear(&[6, 16, 11, 2, 8, 3], 75), 144)
    }
}