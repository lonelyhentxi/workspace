#![allow(dead_code)]
#[macro_use]
extern crate failure;

fn main() {
    // 错误
    {
        // rust 把错误分成了两大类
        // 不可恢复错误，使用 panic 处理
        // 可恢复错误，使用返回值处理
    }
    {
        // 基本错误处理
        // 最基本类型为 Option<T>
        // 对于带错误状态的返回值使用 Result<T,Error>
        // 对于一定成功的 Result 返回的 Error 为空 enum，内存为0，与 T 重构，0 开销
    }
    {
        // 组合错误类型
        // map T -> T1
        // map_err E -> E1
        // and R<T1,E>
        // and_then T -> R<T1,E>
        // or R<T,E1>
        // or_then E -> R<T,E1>
        {
            use std::env;

            fn double_arg(mut argv: env::Args) -> Result<i32, String> {
                argv.nth(1)
                    .ok_or("please give at least one argument".to_owned())
                    .and_then(|arg|
                        arg.parse::<i32>().map_err(
                            |err| err.to_string()
                        )
                    ).map(|n| 2 * n)
            }

            match double_arg(env::args()) {
                Ok(n) => println!("{}", n),
                Err(err) => println!("Error: {}", err)
            }
        }
    }
    {
        // 问号运算符
        // Result 类型有很多优点，但是可能会出现嵌套层数过高，不利于阅读
        {
            use std::fs::File;
            use std::io::Read;
            use std::path::Path;
            fn file_double_bad<P:AsRef<Path>>(file_path: P) -> Result<i32, String> {
                File::open(file_path)
                    .map_err(|err| err.to_string())
                    .and_then(|mut file| {
                        let mut contents = String::new();
                        file.read_to_string(&mut contents)
                            .map_err(|err| err.to_string())
                            .map(|_| contents)
                    })
                    .and_then(|contents| {
                        contents.trim().parse::<i32>()
                            .map_err(|err| err.to_string())
                    })
                    .map(|n| 2*n)
            }

            fn file_double<P:AsRef<Path>>(file_path: P) -> Result<i32, String> {
                let mut file = File::open(file_path).map_err(|e| e.to_string())?;
                let mut contents = String::new();
                file.read_to_string(&mut contents)
                    .map_err(|err| err.to_string())?;
                let n = contents.trim().parse::<i32>()
                    .map_err(|err| err.to_string())?;
                Ok(2*n)
            }

            // ? 在 result 嵌套中的作用和 await 在 promise 嵌套中的作用有异曲同工之妙
            // 可以继续精简
            #[derive(Debug)]
            enum MyError {
                Io(std::io::Error),
                Parse(std::num::ParseIntError)
            }

            impl From<std::io::Error> for MyError {
                fn from(error: std::io::Error) -> Self {
                    MyError::Io(error)
                }
            }

            impl From<std::num::ParseIntError> for MyError {
                fn from(error: std::num::ParseIntError) -> Self {
                    MyError::Parse(error)
                }
            }

            fn file_double_better<P:AsRef<Path>>(file_path: P) -> Result<i32, MyError> {
                let mut file = File::open(file_path)?;
                let mut contents = String::new();
                file.read_to_string(&mut contents)?;
                let n = contents.trim().parse::<i32>()?;
                Ok(2*n)
            }
        }
        {
            // 问号操作符的实现
            // 问号操作符对应 Try trait
            /*
            trait Try {
                type Ok;
                type Error;
                fn into_result(self) -> Result<Self::Ok,Self::Error>;
                fn from_error(v:Self::Error) -> Self;
                fn from_ok(v:Self::Ok) -> Self;
            }*/
        }
        {
            // 和 Try trait 一起设计的还有一个临时性的 do catch 块
            // 这个特性需要打开 #[feature(catch_expr)]
            // 为避免现存代码编译错误，catch 关键字将在 2018 的下一个 edition 代替 do catch {} 语法
        }
        {
            // 如果使用 ？，那么对应的 Error 定义代码又有增长
            // 在性能要求不高的情况下，可以使用 trait object
            {
                use std::fmt::{Debug,Display};
                pub trait MyError: Debug + Display {
                    fn description(&self) -> &str;
                    fn cause(&self) -> Option<&dyn MyError>;
                }
            }
            // 所有标准库里的 Error 都已经实现了这个 trait
            {
                use std::fs::File;
                use std::io::Read;
                use std::path::Path;

                fn file_double<P: AsRef<Path>>(file_path: P) ->
                    Result<i32, Box<dyn std::error::Error>> {
                    let mut file = File::open(file_path)?;
                    let mut contents = String::new();
                    file.read_to_string(&mut contents)?;
                    let n = contents.trim().parse::<i32>()?;
                    Ok(2*n)
                }
            }
            // 这种写法不方便向下转型，适合不需要针对错误类型有任何区分的处理
            // 给 enum 加上 #[non_exhaustive] 强制增加默认情况以避免上游 enum 更改导致的编译错误
        }
    }
    {
        // main 函数中使用问号运算符
        // 为了兼容原始代码又增加错误处理，main 函数签名改为了 fn<T:Termination>() -> T
        // 用户代码是由 runtime 函数调用的
    }
    {
        // 新的 failure 库
        {
            // 现存 Error trait 的问题
            // description 方法基本没有什么用
            // 无法回溯，没有记录错误一层层传播的过程，不方便 debug
            // Box<Error> 不是线程安全的
        }
        // Failure 可以就是为了进一步优化错误处理设计的，包括
        {
            // 新的 failure::Fail trait，取代 Error，拥有更多方法，并且具有线程安全性
            // 自动 derive 机制
            // failure::Error 结构体，所有实现了 Fail 的类型，都可以转型为之，并且提供了向下转型的方法
            {
                use std::fs::File;
                use std::io::Read;
                use std::path::Path;

                #[derive(Debug,Fail)]
                enum MyError {
                    #[fail(display = "IO error {}.", _0)]
                    Io(#[cause] std::io::Error),
                    #[fail(display = "Parse error {}.",_0)]
                    Parse(#[cause] std::num::ParseIntError)
                }

                impl From<std::io::Error> for MyError {
                    fn from(error: std::io::Error) -> Self {
                        MyError::Io(error)
                    }
                }

                impl From<std::num::ParseIntError> for MyError {
                    fn from(error: std::num::ParseIntError) -> Self {
                        MyError::Parse(error)
                    }
                }

                fn file_double<P: AsRef<Path>>(file_path: P) ->
                Result<i32, Box<dyn std::error::Error>> {
                    let mut file = File::open(file_path)?;
                    let mut contents = String::new();
                    file.read_to_string(&mut contents)?;
                    let n = contents.trim().parse::<i32>()?;
                    Ok(2*n)
                }
            }
        }
    }
}