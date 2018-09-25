#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_assignments)]
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
                v.push( 5);
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
            fn borrow_semantics(v:&Vec<i32>) {
                println!("size of param: {}",std::mem::size_of::<&Vec<i32>>());
                for item in v {
                    print!("{} ", item);
                }
                println!(" ");
            };
            fn move_semantics(v: Vec<i32>) {
                println!("size of param: {}", std::mem::size_of::<Vec<i32>>());
                for item in v {
                    print!("{} ",item);
                }
                println!(" ");
            };
            let array = vec![1_i32,2,3];
            // 如果需要引用传递，在函数声明和调用的地方使用 & 标记
            borrow_semantics(&array);
            move_semantics(array);
        }
        {
            // 对于小数点方式使用的方法，对于self参数，会自动转换
            let mut x : String = "hello".into();
            println!("length of String {}",x.len());
            x.push('!');
            println!("length of String {}",x.len());
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
    }
}