#![allow(dead_code)]
fn main() {
    // 闭包
    // 闭包是一种匿名函数，具有捕获外部变量的能力
    // 可以像函数一样被调用
    // 可以捕获当前环境中的变量
    {
        /*
        let _add1 = |a,b| {return a+b;};
        let _add2 = |a,b| {a+b};
        let _add3 = |a,b| a+b;
        */
        // 以上都没被使用，类型无法推导，出错
    }
    // fn 的定义和调用位置不重要，只要在作用域内可观察，rust 中不需要前向声明
    // closure 的生命周期类似变量
    {
        // 变量捕获
        {
            // 当创建一个闭包的时候，编译器帮助创建了一个匿名 struct 类型
            {
                let x = 1_i32;
                let add_x = |a| x+a;
                let result = add_x(5);
                println!("result is {}", result);
            }
            {
                // 实际上创建了泛型，该处表现的是在实例化时期创建的内容
                struct MyClosure {
                    inner1: i32,
                }

                impl MyClosure {
                    fn call(&self,a:i32)->i32 {
                        self.inner1+a
                    }
                }

                let x = 1_i32;
                let add_x = MyClosure {inner1:x};
                let result = add_x.call(5);
                println!("result is {}", result);
            }
        }
        {
            // rust 闭包的类型推导方式
            // 在保证能通过编译的情况下，编译器会自动选择一种对外部影响最小的方式；对于被捕获类型为
            // T 的外部变量，在匿名结构体中的存储方式选择为： 尽可能先选择 &T, 其次选择 & mut T,最后 T
        }
    }
    {
        // move 关键字
        // 前述的变量捕获的方式都是针对局部作用变量的闭包而准备的
        // 如果闭包的声明周期超过函数的范围，保证在原变量被释放后仍旧能使用
        // move 关键字会使得所有变量捕获全部使用 by_value 方式
        {
            fn make_adder(x:i32) -> Box<Fn(i32)->i32> {
                Box::new(move |y| x+y)
            }

            let f = make_adder(3);
            println!("{}", f(1));
            println!("{}", f(10));
        }
    }
    {
        // Fn/FnMut/FnOnce
        {
            // Fn 只具有读取外部变量的能力
            // FnMut 具有读写的能力
            // FnOnce 只能使用一次
            // 对于一个闭包，编译器会按从上到下的顺序尝试匹配
        }
        {
            // move 关键字只是修改变量被捕获的方式
            // 并不影响闭包类型的推断
        }
    }
    {
        // 闭包与泛型
        // 实际上函数是闭包的特化，无捕获 Fn 闭包
        // 即使两个函数的参数一致，也是不同的闭包，只是实现了相同的 trait 而已
        // 于是
        {
            // 不能给一个变量赋予两个闭包
            // 向函数参数中传递闭包，有两种方式
            {
                // 泛型以实现静态分派
                // trait object 实现动态分派
            }
            // 从函数返回值接受闭包
            {
                // fn test() -> impl Fn(i32) -> i32 静态分派 （haskell？233）
                // 使用 Box<dyn Fn(i32)->i32> 动态分派
                // 详见下一章
            }
        }
    }
    {
        // 闭包与生命周期
        {
            fn calc_by<'a,F>(var: &'a i32,f:F) -> i32 where F: Fn(& 'a i32) -> i32 {
                f(var) // 正确推导
            }
        }
        {
            fn calc_by<F>(var: &i32, f:F) -> i32 where F: Fn(&i32)->i32  {
                let local = *var;
                f(&local) // 去掉标注错误的生命周期标记，正确
            }
        }
        {
            fn calc_by<'a,F>(var: &'a i32,f:F) -> i32
                where F: for<'f> Fn(&'f i32) -> i32 {
                let local = *var;
                f(&local) // 使用高阶生命周期标记手动标记，正确
            }
            // for<'a> Fn(&'a Arg) -> &'a Ret 这样的语法只能用于生命周期参数，不能用于任何泛型
        }
    }
}