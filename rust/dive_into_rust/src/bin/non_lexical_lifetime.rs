#![allow(dead_code)]
fn main() {
    {
        // 在一般情况下借用指针的生命周期规则，和普通对象的生命周期一致，按作用域决定
        // 这是严格按照文法设计和作用域绑定的，限制了程序员的发挥
    }
    {
        // NLL 希望解决的问题
        fn capitalize(data: &mut [char]) {
            for c in data {
                c.make_ascii_uppercase();
            }
        }
        {

            fn foo() -> Vec<char> {
                let mut data = vec!['a','b','c'];
                capitalize(&mut data[..]);
                data.push('d');
                data.push('e');
                data.push('f');
                data
            }

            let v = foo();
            println!("{:?}", v);
            // 由于较窄的作用域
        }
        {
            fn foo() -> Vec<char> {
                let mut data = vec!['a','b','c'];
                let slice = &mut data[..];
                capitalize(slice);
                data.push('d');
                data.push('e');
                data.push('f');
                data
            }
            let v = foo();
            println!("{:?}", v);
            // 在 1.30.0 nightly 中这段代码已经默认 nll，不会失效了
        }
        {
            use std::collections::HashMap;
            fn process_or_default<K:std::cmp::Eq+std::hash::Hash,V:Default+std::fmt::Display>(map: &mut HashMap<K,V>,key: K) {
                match map.get_mut(&key) {
                    Some(value) => println!("{}",value),
                    None => { map.insert(key, V::default()); }
                }
            }
            let mut map = HashMap::<String,String>::new();
            let key = String::from("abc");
            process_or_default(&mut map,key);
            println!("{:?}",map);
            // 在 1.30.0 nightly 中这段代码已经默认 nll，不会失效了
        }
    }
    {
        // NLL 的原理
        // 由于简单的使用 AST 分析最后使用的位置，会导致问题
        // 新版本的借用检查器将 AST 转化为中间表达形式 MIR，这个数据结构会表述一个控制流图
        {
            // 这个功能只影响静态分析结果，不影响程序的执行情况
            // 不会影响以前能通过编译的代码
            // 依然保证了安全性，只是将以前过于保守的检查规则适当放宽
            // 它依赖的是静态检查规则
            // 它只影响引用类型的生命周期，不影响对象的生命周期
            // 它不会影响 RAII 语义
        }
    }
}