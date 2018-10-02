#![allow(dead_code)]
#![allow(unused_variables)]

fn main() {
    // 容器和迭代器
    {
        // 容器
        // 和 C++ STL 类似，rust 标准库也提供了一些比较常用的容器以及相关的迭代器
        {
            // 目前实现的有：
            /*
                1. Vec -> vector
                2. VecDeque -> deque
                3. LinkedList -> list
                4. HashMap -> unordered_map
                5. BTreeMap -> map
                6. HashSet -> unordered_set
                7. BTreeSet -> set
                8. BinaryHeap -> heap
            */
        }
        {
            // Vec 自动扩容，重载 Index，重载 Deref/DerefMut
            {
                let v1 = Vec::<i32>::new();
                let v2 = Vec::<String>::new();
                let v3 = vec![1_i32, 2, 3];
                let mut v4 = Vec::new();
                v4.push(1_i32);
                v4.extend_from_slice(&[10, 20, 30, 40, 50]);
                v4.insert(2, 100);
                println!("capacity: {},length: {}", v4.capacity(), v4.len());
                v4[5] = 5;
                let i = v4[5];
                println!("{}", i);
                if let Some(i) = v4.get(6) {
                    println!("{}", i);
                }
                let slice = &v4[4..];
                println!("{:?}", slice);
            }
        }
        {
            // VecDeque, 在头尾添加删除元素很快
            {
                use std::collections::VecDeque;
                let mut queue = VecDeque::with_capacity(64);
                for i in 1..10 {
                    queue.push_back(i);
                }
                while let Some(i) = queue.pop_front() {
                    println!("{}", i);
                }
            }
        }
        {
            // HashMap<K,V,S> S是哈希算法类型，key 要满足 Eq + Hash 的约束
            {
                use std::hash::{Hash, Hasher};

                struct Person {
                    first_name: String,
                    last_name: String,
                }

                impl Hash for Person {
                    fn hash<H: Hasher>(&self, state: &mut H) {
                        self.first_name.hash(state);
                        self.last_name.hash(state);
                    }
                }
            }
            {
                // or
                #[derive(Hash)]
                struct Person {
                    first_name: String,
                    last_name: String,
                }
            }
            {
                use std::collections::HashMap;
                #[derive(Hash,Eq,PartialEq,Debug)]
                struct Person {
                    first_name: String,
                    last_name: String,
                }
                impl Person {
                    fn new(first: &str, last: &str) -> Self {
                        Person {
                            first_name: first.to_string(),
                            last_name: last.to_string(),
                        }
                    }
                }

                let mut book = HashMap::new();
                book.insert(Person::new("John", "Smith"), "521-8976");
                book.insert(Person::new("Sandra", "Dee"), "521-9655");
                let p = Person::new("John", "Smith");
                if let Some(phone) = book.get(&p) {
                    println!("Phone number found: {}", phone);
                }
                book.remove(&p);
                println!("Find key: {}", book.contains_key(&p));
                book.entry(Person::new("Ted","Baker")).or_insert("418-4165");
            }
        }
        {
            // BTreeMap 是基于 B 树数据结构的存储一组键值对的容器，要求满足 Ord，即全序特征
            {
                use std::collections::BTreeMap;
                #[derive(Eq,PartialEq,Debug,Ord, PartialOrd)]
                struct Person {
                    first_name: String,
                    last_name: String,
                }

                impl Person {
                    fn new(first: &str,last: &str) -> Person {
                        Person {
                            first_name:first.to_string(),
                            last_name: last.to_string()
                        }
                    }
                }

                let mut book = BTreeMap::new();
                book.insert(Person::new("John", "Smith"), "521-8976");
                book.insert(Person::new("Sandra", "Dee"), "521-9655");
                let p = Person::new("John", "Smith");
                if let Some(phone) = book.get(&p) {
                    println!("Phone number found: {}", phone);
                }
                book.remove(&p);
                println!("Find key: {}", book.contains_key(&p));
            }
            {
                // HashMap 和 BTreeMap 都可以遍历，但是后者有序
                // BTreeMap 可以查询区间的结果
                use std::collections::BTreeMap;
                let mut map = BTreeMap::new();
                map.insert(3,'a');
                map.insert(5,'b');
                map.insert(8,'c');
                for (k,v) in map.range(2..6) {
                    println!("{} {}",k,v);
                }
            }
        }
    }
    {
        // 迭代器
        {
            // rust 的迭代器定义如下
            /*
            trait Iterator {
                type Item;
                fn next(&mut self) -> Option<Self::Item>;
                ...
            }*/
        }
        {
            // 实现迭代器
            use std::iter::Iterator;

            struct Seq {
                current: i32,
            }

            impl Seq {
                fn new() -> Self {
                    Self {current:0}
                }
            }

            impl Iterator for Seq {
                type Item = i32;
                fn next(&mut self) -> Option<i32> {
                    if self.current < 10 {
                        self.current += 1;
                        return Some(self.current);
                    } else {
                        return None;
                    }
                }
            }

            let mut seq = Seq::new();
            while let Some(i) = seq.next() {
                println!("{}",i);
            }
        }
        {
            // 迭代器的组合
            // iter() 创造 & 迭代器
            // iter_mut() 创造 &mut
            // into_iter() 创造 T 类型迭代器
            {
                let v = vec![1,2,3,4,5];
                let mut iter = v.iter();
                while let Some(i) = iter.next() {
                    println!("{}", i);
                }
            }
            {
                let v = vec![1,2,3,4,5];
                let mut iter = v.iter()
                    .take(5).filter(|&x| x%2 == 0)
                    .map(|&x| x*x)
                    .enumerate();
                while let Some((i,v)) = iter.next() {
                    println!("{} {}",i,v);
                }
                // 惰性求值，见 haskell、reactive X等
            }
            {
                // for 循环是专门为迭代器设计的一个语法糖
                use std::collections::HashMap;

                let v = vec![1_i32,2,3,4,5,6,7,8,9];
                for i in v {
                    println!("{}", i);
                }
                let map: HashMap<i32,char> =
                    [(1_i32,'a'),(2,'b'),(3,'c')].iter().cloned().collect();
                for (k,v) in &map {
                    println!("{} : {}",k,v);
                }
            }
            {
                // Iter => (&K,&V) => & container
                // IterMut => (&K,&mut V) => & mut container
                // IntoIter => (K,V) => container
                // 使用 rustc --unpretty=hir -Z unstable-options 编译后
                // for in 循环的原理就是调用 <container>.into_iter() 方法，将返回值解包
                // 所以在使用 for 循环的时候，可以自由的选择三种方式
            }
        }
    }
}