#![crate_name="doc"]
#![allow(dead_code)]

/// 这里给出一个人类
pub struct Person {
    /// 一个人的名字，不管 Juliet 多讨厌它
    name: String,
}

impl Person {
    /// 返回给定的人名字
    ///
    /// # 参数
    ///
    /// * `name` - 字符串 slice，代表人物的名称
    ///
    /// # 示例
    ///
    /// ```
    /// // 可以在注定的特定标记内编写rust
    /// // 如果可以通过 --- 测试传递给 Rustdoc 将会帮助测试
    /// let person = Person::new("name);
    /// ```
    pub fn new(name: &str) -> Person {
        Person {
            name: name.to_string(),
        }
    }
    /// 给一个友好的问候！
    /// 对被叫到的 `Person` 说 "Hello, [name]" 。
    pub fn hello(& self) {
        println!("Hello, {}!", self.name);
    }
}

fn main() {
    let john = Person::new("John");
    john.hello();
}