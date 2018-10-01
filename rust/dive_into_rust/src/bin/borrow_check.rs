#![allow(unused_variables)]
#![allow(dead_code)]
fn main() {
    {
        {
            // danger arises from aliasing + mutation
            // alias 意思是别名，如果一个变量可以通过多种 path 访问，其意味着共享，可以通过多个入口访问
            // 同一块内存
            // mutation 意思是改变，意味着拥有修改的权限。
            // rust 认为，只要保证 alias 不和 mutation 同时出现，那么代码就一定是安全的
        }
        {
            // 编译错误实例
            {
                let i = 0;
                let p1 = &i;
                let p2 = &i;
                println!("{} {} {}",i,p1,p2);
                // 共享不可变，安全
            }
            {
                {
                    let mut _i  = 0;
                    let _p1 = &_i;
                    _i = 1;
                    // 由于 _p1 实际上并不会被使用，可以编译通过
                }
                {
                    // let mut _i  = 0;
                    // let _p1 = &_i;
                    // _i = 1;
                    // println!("{}",_p1);
                    // 借用检查失败
                }
                // 共享不可变，失败
            }
            {
                let mut i = 0;
                let p1 =  &mut i;
                *p1 = 1;
                // i 已经被冻结，所以并非共享，所以只有 p1 这个入口可以读写变量
                // let x = i;
            }
            {
                let mut i = 0;
                let p1 = &mut i;
                // let p2 = &mut i;
                // 不能同时有两个可变借用
            }
        }
    }
    {
        // 内存不安全示例：修改枚举
        {
            #[derive(Debug)]
            enum StringOrInt {
                Str(String),
                Int(i64),
            }
            {
                // let mut x = StringOrInt::Str("Hello world".to_string());
                /* if let StringOrInt::Str(ref insides) = x {
                    x = StringOrInt::Int(1);
                    println!("inside is {} x says: {:?}", insides, x);
                }*/
            }
        }
    }
    {
        // 内存不安全示例：迭代器失效
        {
            // let mut arr = vec!["ABC","DEF","GHI"];
            /* for item in &arr {
                arr.clear();
            } */
        }
    }
    {
        // 内存不安全示例：悬空指针
        {
            /*
            let mut arr = vec![1_i32,2,3,4,5];
            let p: &i32 = &arr[0];
            for i in 1..100 {
                arr.push(i);
            }
            println!("{}",p);
            */
            // mutation and alias 冲突
        }
    }
}