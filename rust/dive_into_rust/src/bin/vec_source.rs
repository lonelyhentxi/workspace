#![feature(generic_param_attrs, dropck_eyepatch)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(stable_features)]
fn main() {
    {
        // 使用展示
        // 和 c++ vec 原理及使用类似
        let mut v1 = Vec::<i32>::new();
        println!("Start: length {} capacity {}", v1.len(), v1.capacity());
        for i in 1..10 {
            v1.push(i);
            println!("[Pushed {}] length {} capacity {}", i, v1.len(), v1.capacity());
        }
        let mut v2 = Vec::<i32>::with_capacity(1);
        println!("Start: length {} capacity {}", v2.len(), v2.capacity());
        v2.reserve(10);
        for i in 1..10 {
            v2.push(i);
            println!("[Pushed {}] length {} capacity {}", i, v2.len(), v2.capacity());
        }
    }
    {
        // 内存申请，扩容和释放
        // source code 见标准库源码，与标准库版本有关
        // 实现逻辑和 C++ Vector 类似，一些细节实现与 rust 及其版本相关
        {
            // drop check
            {
                let a = 1;
                let b  = 2;
                // 'a > 'b
            }
            {
                let (a,b) = (1,2);
                // 'a = 'b
            }
            {
                // 证明上一条
                {
                    let (a,mut _b) :(i32, Option<&i32>) = (1,None);
                    _b = Some(&a);
                }
                {
                    let (mut _a,b):(Option<&i32>,i32) = (None,1);
                    _a = Some(&b);
                }
            }
            {
                // 生命周期可以相等，但是析构函数却不可能同时进行
                // rust 在涉及析构函数的时候有个特殊规定，即如果两个变量具有互相引用的关系，那么它们
                // 的声明周期必须满足“严格大于”关系，为了防止出现，编译器内部做了专门检查
                // 但是有时检查过于严格，如果没什么危险，大于等于就够了，让用户告诉编译器够不够危险，
                // 就是#[may_dangle]

                struct T {dropped: bool};

                impl T {
                    fn new() -> Self {
                        Self {dropped:false}
                    }
                }

                impl Drop for T {
                    fn drop(&mut self) {
                        self.dropped = true;
                    }
                }

                struct R<'a> {
                    inner: Option<&'a T>
                }

                impl <'a> R<'a> {
                    fn new() -> Self {
                        Self { inner: None }
                    }
                    fn set_ref<'b:'a>(&mut self,ptr: &'b T) {
                        self.inner = Some(ptr);
                    }
                }

                unsafe impl<#[may_dangle] 'a> Drop for R<'a> {
                    fn drop(&mut self) {

                    }
                }

                {
                    let (a,mut b):(T,R) = (T::new(),R::new());
                    b.set_ref(&a);
                }
                {
                    let (mut a,b):(R,T) = (R::new(),T::new());
                    a.set_ref(&b);
                }
                // 并打开
                // #![feature(generic_param_attrs, dropck_eyepatch)]
            }
        }
    }
    {
        // 不安全的边界
        {
            /*
            pub unsafe fn set_len(&mut self,len:usize) {
                self.len = len;
            }*/
            // 虽然是简单的赋值，但依赖于 self.len 这个值的合法性，因此是 unsafe 的
        }
        // 我们在使用 unsafe 代码块的时候，很可能需要一些对 safe 代码的隐含的假设和依赖，
        // 这些依赖关系既不能通过类型系统向编译器清楚表达，也未必能够在代码中明显地表现出来，
        // safe 代码有义务维护 unsafe 代码相对应的假设，unsafe 代码也要注意保持一致性
        // 在 rust 中，如果用户有机会利用库在不使用 unsafe 的情况制造出 “内存不安全”
        // 那么这个库就是有 bug 的，低质量、不可接受的
    }
    {
        // 自定义解引用
        // source code 见标准库源码，与标准库版本有关
    }
    {
        // 迭代器
        // 合理实现了 IntoIterator trait 的任何类型，就可被用在 for 循环中，见第三部分
        {
            // 特殊实现的 drain 方法
            // 从动态数组中将内容“移除”出去，会返回一个迭代器，在自己析构的时候，会调用源容器中待删除部分
            // 为了避免自己的析构函数无法调用，在自己和源容器中都产生副本，会判断长度
            {
                let mut origin = vec![0,1,2,3,4,5];
                print!("Removed:");
                for i in origin.drain(1..3) {
                    print!("{}",i);
                }
                println!(" ");
                print!("Left:");
                for i in origin.iter() {
                    print!("{}",i);
                }
                println!(" ");
            }
            {
                // 如果没有安全防护，可能出现的问题代码
                // 由于检查的存在，如果没有正确的析构函数，只会剩下前面的元素
                // 内存可能泄露，但是不会内存不安全
                let mut origin  = vec!["0".to_string(),"1".to_string(),"2".to_string()
                                       ,"3".to_string(),"4".to_string(),"5".to_string()];
                {
                    let mut d = origin.drain(1..2);
                    let s: Option<String> = d.next();
                    println!("{:?}",s);
                    let s: Option<String> = d.next();
                    println!("{:?}",s);
                    std::mem::forget(d);
                }
                println!("Left:");
                for i in origin.iter() {
                    println!("{:?}",i);
                }
            }
        }
    }
    {
        // panic safety
        // 库需要调用传入的用户代码的时候，由于必须应对可能不安全的用户代码（有panic风险）
        // 要假定这些代码有 panic 风险，保证损失最小，尽量的保护合法数据，即使牺牲性能也在所不惜
    }
}