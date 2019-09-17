#![feature(box_syntax)]

fn main() {
    {
        // 所有权
        {
            let s = String::from("hello");
            let s1 = s.clone(); // clone
            let s2 = s; // move
            println!("{}",s1);
            println!("{}",s2);
        }
    }
    {
        // 移动语义
        // 类似 c++，略，不过在 rust 中作为默认
    }
    {
        // 复制语义
        // rust 中，在普通变量绑定，函数传参，模式匹配等场景下，凡是实现了 std::marker::Copy trait的，
        // 都会执行 copy 语义
        // copy 继承了 clone，凡是要实现 copy 必须实现 clone
        {
            #[derive(Copy)]
            struct Foo {
                data: i32,
            }

            impl Clone for Foo {
                fn clone(&self) -> Foo {
                    Foo { data: self.data}
                }
            }

            let v1 = Foo {data:0};
            let _v2 = v1;
            println!("{:?}",v1.data);
        }
        // 由于过于常用且实现简单，编译器为之实现了便捷实现
        // #[derive(xxx)] 即可
    }
    {
        // box 类型
        // linker to start, box 关键字可以将变量装箱
        struct T {
            value: i32
        }

        let p: Box<T> = box T{value:1};
        println!("{}", p.value);
    }
    {
        // clone vs copy
        {
            // copy 的含义
            // std::marker 模块中所有的 trait 它们都是和编译器密切绑定的，实现这些 trait 对编译器行为
            // 有重要影响，这几个 trait 内部都没有方法，唯一的任务是给类型打标记，表明符合某种约定
        }
        {
            // copy 的实现条件
            // 对于自定义类型，只有每个成员都实现了 copy trait，这个类型才有资格实现
            // rust 中，只有 POD 才有资格实现 copy trait
        }
        {
            // clone 的含义
            // clone 的方法一般用于“基于语义的复制”
            // 对于实现了 copy 的类型，它的 clone 方法应该和 copy 语义相容，等同按字节复制
            {
                pub trait Clone: Sized {
                    fn clone(&self) -> Self;
                    fn clone_from(&mut self,source: &Self) {
                        *self = source.clone();
                    }
                }
            }
        }
        {
            // 自动 derive
            // 通过 derive 方式自动实现 copy 和手动实现的 copy 有微小的区别， 当类型具有泛型参数的时候
            // 通过 derive 生成的代码将会自动添加一个 T
        }
    }
    {
        // 析构函数
        // rust 同样使用 RAII 作为资源管理的方法，和 C++ 类似，略
        {
            // 资源管理
            use std::fs::File;
            use std::io::Read;

            let f = File::open("./src/bin/mm_basic.rs");
            if f.is_err() {
                println!("file is not exist.");
                return;
            }
            let mut f = f.unwrap();
            let mut content = String::new();
            let result = f.read_to_string(&mut content);
            if result.is_err() {
                println!("read file error.");
                return;
            }
            println!("{}",result.unwrap());
        }
        {
            // 主动析构
            // 此处的主动析构是指调节变量所有权，使得不能使用，实际上取决于编译器实现
            // （当前是到作用域结束）
            #[inline]
            pub fn _drop<T>(_x:T){}
            // 变量遮蔽不会导致提前析构
            // 变量赋值到下划线会导致立即析构
            // std::mem::drop 会导致生命周期提前结束
            // std::ops::Drop 是一个 trait，用户不能直接调用，在作用域结束的时候，编译器会自动调用
        }
        {
            // Drop or Copy
            // copy 需要执行安全的 memcpy 操作，但是我们不能保证带有析构函数的类型，使用 memcpy
            // 复制一个副本一定不会产生问题，因此编译器会禁止
        }
        {
            // 析构标记
            // 变量生命周期不是简单和代码块绑定
            use std::mem::drop;
            use std::ops::Drop;

            struct D(& 'static str);
            impl Drop for D {
                fn drop(&mut self){
                    println!("destructor {}",self.0);
                }
            }

            fn condition() -> Option<u32> {
                std::env::var("DROP")
                    .map(|s| s.parse::<u32>().unwrap_or(0))
                    .ok()
            }

            let var = (D("first"),D("second"),D("third"));
            match condition() {
                Some(1) => drop(var.0),
                Some(2) => drop(var.1),
                Some(3) => drop(var.2),
                _ => {},
            }
            println!("main end");
            // 判定变量是否可能在多个不同的路径上析构
            // 如果可能，那么它们会在当前函数调用栈中自动插入一个bool类型的标记
            // 用于标记该对象的析构函数是否已经被调用
            // 在各个可能调用析构函数的地方都判断一下状态在调用，以将编译阶段确定声明周期和执行阶段
            // 根据情况调用统一
        }
    }
}