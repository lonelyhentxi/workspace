use std::iter::FromIterator;

#[derive(Debug, PartialEq, Eq)]
pub enum Cons<T> {
    Cons(T, Box<Cons<T>>),
    Null,
}

impl<T:Clone> FromIterator<T> for Cons<T> {
    fn from_iter<I>(it: I) -> Self
        where I: IntoIterator<Item=T>
    {
        Self::from_iter_recursively(it.into_iter())
    }
}

impl<T: Clone> Cons<T> {
    pub fn new(head: T, tail: Self) -> Self {
        Cons::Cons(head, Box::new(tail))
    }

    fn from_iter_recursively<I>(mut iter: I) -> Self
        where I: Iterator<Item=T> {
        match iter.next() {
            Some(item) => Cons::new(item, Self::from_iter_recursively(iter)),
            None => Cons::Null,
        }
    }

    pub fn filter<F>(&self, fun: F) -> Self
        where F: Fn(&T) -> bool
    {
        match self {
            Cons::Cons(ref item, ref tail) => {
                if fun(item) {
                    Cons::new(item.clone(), Self::filter(tail, fun))
                } else {
                    Self::filter(tail, fun)
                }
            }
            Cons::Null => {
                Cons::Null
            }
        }
    }

    pub fn map<F, S>(&self, fun: F) -> Cons<S>
        where F: Fn(T) -> S, S: Clone
    {
        match self {
            Cons::Cons(ref item, ref tail) => {
                Cons::new(fun(item.clone()), Self::map(tail, fun))
            }
            Cons::Null => {
                Cons::Null
            }
        }
    }

    pub fn to_vec(&self) -> Vec<T> {
        match self {
            Cons::Null => vec![],
            Cons::Cons(ref head, ref tail) => {
                let mut head = vec![head.clone()];
                head.extend(tail.to_vec());
                head
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_create_from_vec() {
        assert_eq!(Cons::from_iter(Vec::<i32>::new()), Cons::Null);

        assert_eq!(Cons::from_iter(vec![1, 2, 3, 4, 5]).to_vec(),
                   vec![1, 2, 3, 4, 5]);
    }


    #[test]
    fn should_filter() {
        assert_eq!(Cons::from_iter(vec![1, 2, 3, 4, 5])
                       .filter(|&n| n > 3)
                       .to_vec(),
                   vec![4, 5]);

        assert_eq!(Cons::from_iter(vec![1, 2, 3, 4, 5])
                       .filter(|&n| n > 5),
                   Cons::Null);
    }


    #[test]
    fn should_map() {
        assert_eq!(Cons::from_iter(vec!["1", "2", "3", "4", "5"])
                       .map(str::parse::<i32>)
                       .map(Result::unwrap)
                       .to_vec(),
                   vec![1, 2, 3, 4, 5]);
    }


    #[test]
    fn should_filter_map() {
        assert_eq!(Cons::from_iter(vec![1, 2, 3, 4, 5])
                       .filter(|n| n % 2 == 0)
                       .map(|x| x.to_string())
                       .to_vec(),
                   ["2", "4"]);
    }
}