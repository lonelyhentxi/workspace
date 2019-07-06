extern crate hello_macro;
#[macro_use]
extern crate hello_macro_derive;

fn main() {
    {
        // introduction
        // rust 的宏是卫生宏，调用语法和函数明显不同
        // 宏的内部和外部有严格的访问空间界限
        // c/c++ 的宏在预处理阶段起作用，rust 的宏在语法解析阶段起作用，因此可以获得更多的上下文信息
        {
            // 实现编译期检查
            // println!("number1 {} number2 {}"); // 需要提供足够的参数，这是在编译期进行的检查
        }
        {
            // 实现编译期计算
            println!("file {} line {}", file!(), line!());
        }
        {
            // 实现自动代码生成
        }
        {
            // 实现语法拓展
            #[allow(unused_variables)]
            let v = vec![1,2,3,4,5];
        }
    }
    {
        // 泛型示例
        // 自定义宏只能通过 macro_rules! 宏或者编译期拓展实现，后者现在还不稳定
        macro_rules! hashmap {
            ($($key: expr => $val: expr),*) => {
                {
                let mut map = ::std::collections::HashMap::new();
                $(map.insert($key,$val);)*
                map
                }
            }
        }
        let counts = hashmap!['A'=>0,'C'=>0,'G'=>0,'T'=>0];
        println!("{:?}",counts);
    }
    {
        // 宏 1.1
        // 对于简单的宏，使用示例型的方式就足够了，但是更复杂的就需要过程宏来实现，后者相当于编译期插件
        // 委员会希望拓展一套比较稳定的API，但是现在还不够，当前先来体验 1.1 版本

        use hello_macro::HelloMacro;

        #[derive(HelloMacro)]
        struct Pancakes;
        Pancakes::hello_macro();
    }
}