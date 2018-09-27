#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_assignments)]
#![allow(dead_code)]
fn main() {
    {
        // 生命周期
        let v = vec![1, 2, 3, 4, 5];
        {
            let center = &v[2] as &i32;
            println!("{}", &center);
        }
        println!("{:?}", v);
    }
    {
        // 借用
        {
            fn foo(v: &mut Vec<i32>) {
                v.push(5);
            }
            let mut v = vec![];
            foo(&mut v);
            println!("{:?}", v);
        }
        {
            let mut var = 0_i32;
            {
                // 指针不能被重新绑定，我们可以通过指针改变变量的值
                let p1 = &mut var;
                *p1 = 1;
            }
            {
                let temp = 2_i32;
                let mut p2 = &var;
                p2 = &temp;
            }
            {
                let mut temp = 3_i32;
                let mut p3 = &mut var;
                *p3 = 3;
            }
        }
    }
    {
        // 借用规则
        // 借用指针不能比指向的变量存活时间长
        // &mut 型借用只能指向 mut 变量
        // &mut 型借用指针存在的时候，被借用变量本身会处于“冻结”状态
        // 如果存在 & 型借用指针，那么可以同时存在多个；如果存在 &mut 指针，那么只能存在一个&；
        {
            fn borrow_semantics(v: &Vec<i32>) {
                println!("size of param: {}", std::mem::size_of::<&Vec<i32>>());
                for item in v {
                    print!("{} ", item);
                }
                println!(" ");
            };
            fn move_semantics(v: Vec<i32>) {
                println!("size of param: {}", std::mem::size_of::<Vec<i32>>());
                for item in v {
                    print!("{} ", item);
                }
                println!(" ");
            };
            let array = vec![1_i32, 2, 3];
            // 如果需要引用传递，在函数声明和调用的地方使用 & 标记
            borrow_semantics(&array);
            move_semantics(array);
        }
        {
            // 对于小数点方式使用的方法，对于self参数，会自动转换
            let mut x: String = "hello".into();
            println!("length of String {}", x.len());
            x.push('!');
            println!("length of String {}", x.len());
            let v = x.clone().into_bytes();
            let w = x.into_bytes();
        }
        {
            let mut x = 1_i32;
            let p = &mut x;
            // x = 2; // cannot assign to `x` because it is borrowed
            println!("value of pointed : {}", p);
        }
    }
    {
        // 生命周期标记
        {
            struct T {
                member: i32
            };
            // 函数的生命周期标记
            {
                fn test<'a>(arg: &'a T) -> &'a i32 {
                    &(arg.member)
                }

                let t = T { member: 0 };
                let x = test(&t);
                println!("{:?}", x);
            }
            {
                fn test<'a, 'b>(arg: &'a T) -> &'b i32 where 'a: 'b {
                    &arg.member
                }
            }
            {
                fn select<'a>(arg1: &'a i32, arg2: &'a i32) -> &'a i32 {
                    if *arg1 < *arg2 {
                        arg1
                    } else {
                        arg2
                    }
                }

                let x = 1;
                let y = 2;
                let selected = select(&x, &y);
                println!("{}", selected);
            }
            {
                // 类型的生命周期标记
                struct Test<'a> {
                    member: &'a str,
                }

                impl<'t> Test<'t> {
                    fn test<'a>(&self, s: &'a str) {}
                }
            }
        }
    }
    {
        // 省略生命周期标记
        {
            // 编译器对于省略掉的生命周期
            // 不使用自动推理策略，而是用几个非常简单的固定规则
            fn get_str(s: &String)->& 'static str {
                println!("call fn {}",s);
                "hello world"
            }

            let c = String::from("haha");
            let x: & 'static str = get_str(&c);
            println!("{}",x);
            // 每个带声明周期参数的输入参数，对应不同的生命周期参数
            // 如果只有一个输入参数带生命周期参数，那么返回值的生命周期被指定为这个参数
            // 如果有多个输入参数带生命周期参数，但其中有 &self，&mut self, 那么生命周期被指定为这个参数
            // 以上都不满足，就不能自动补全返回值的声明周期参数
            // elision != inference
        }
    }
}