fn main() {
    {
        // unsafe 关键字
        {
            // rust 的 unsafe 关键字有以下几种用法：
            /*
                1. 用于修饰函数的 fn
                2. 用于修饰代码块
                3. 用于修饰 trait
                4. 用于装饰 impl
            */
        }
        {
            // unsafe 的不安全性具有传递性
            // unsafe函数在unsafe语句或者unsafe函数中使用
            // unsafe 代码相比一般情况多了几种使用情况
            /*
                1. 对裸指针进行解引用操作
                2. 读写可变静态变量
                3. 读 union 或者写 union 的非 copy 成员
                4. 调用 unsafe 函数
            */
            // 调用 extern 必须通过 unsafe 实现
        }
    }
    {
        // 裸指针
        // 裸指针可以为空，而且编译器不保证裸指针一定指向一个合法的内存地址。
        // 不会执行任何自动化清理工作，比如释放内存；
        // 裸指针复制操作执行的是简单的内存浅复制，并且不存在borrow_checker的限制；
        {
            let x = 1_i32;
            let mut y: u32 = 1;
            let raw_mut = &mut y as *mut u32 as *mut i32 as *mut i64;
            unsafe {
                *raw_mut = -1;
            }
            println!("{:X} {:X}", x, y);
            // 并没有按照书上显示的影响到 X
        }
        {
            // fail code
            #[allow(dead_code)]
            fn raw_to_ref<'a>(p: * const i32) -> &'a i32 {
                unsafe {
                    &*p
                }
            }

            fn _raw_to_ref_core_dump() {
                let p: &i32 = raw_to_ref(std::ptr::null::<i32>());
                println!("{}",p);
            }
        }
        {
            // successful code
            fn raw_to_ref<'a>(p: *const i32) -> Option<&'a i32> {
                if p.is_null() {
                    None
                } else {
                    unsafe {
                        Some(&*p)
                    }
                }
            }
            let p = raw_to_ref(std::ptr::null::<i32>());
            println!("{:?}",p);
        }
    }
    {
        // 内置函数
        {
            // transmute
            {
                // transmute 转换要求是 二进制长度 相等
                // 使用 as 转型无法做到检测 二进制长度 相等
                let x = vec![1,2,3,4,5];
                unsafe {
                  let t: (usize, usize, usize) = std::mem::transmute_copy(&x);
                    println!("{} {} {}", t.0, t.1, t.2);
                };
            }
        }
    }
}