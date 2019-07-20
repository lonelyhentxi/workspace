#![feature(get_type_id)]
#![allow(dead_code)]
extern crate dirs;
fn main() {
  // 标准库简介
  {
    // 类型转换
    // rust 提供了一个关键字 as 用于基本类型的转换。
    {
      // AsRef/AsMut
      /*
      pub trait AsRef<T: ?Sized> {
        fn as_ref(&self)-> &T;
      }
      
      pub trait AsRefMut<T: ?Sized> {
        fn as_mut(&mut self) -> &mut T;
      }
      */
      // 这些 trait 适合用在泛型代码中，接受各种类型，只要能转换为某个类型即可
      {
        fn _iter_bytes<T: AsRef<[u8]>>(arg: T) {
          for i in arg.as_ref() {
            println!("{}", i);
          }
        }

        let s: String = String::from("this is a string");
        let v: Vec<u8> = vec![1, 2, 3];
        let c: &str = "hello";
        _iter_bytes(s);
        _iter_bytes(v);
        _iter_bytes(c);
      }
    }
    {
      // Borrow/BorrowMut
      // 这个 trait 一般只实现少数重要的类型
      // 它要求 borrow 返回的值，必须和原值具有相同的 hash 值
    }
    {
      // From/Into
      // From/Into 做的是 T 到 U 的转换
      // 标准库还有 TryFrom/TryInto用于可能出错的转换，返回result
    }
    {
      // ToOwned
      // clone => &T => T
      // toOwned => &T => U
      {
        // 默认实现
        /*
          impl<T> ToOwned for T where T:Clone {
          type Owned = T;
          fn to_owned(&self) -> T {
            self.clone()
          }
        
          fn clone_into(target: &mut Self::Owned) {
            target.clone_from(self)
          }
        }
        */
      }
      // 很有用的 Cow 类型也是基于之实现的
      {
        pub enum MyCow<'a, B> where B: 'a+ToOwned+?Sized {
          Borrowed(&'a B),
          Owned(<B as ToOwned>::Owned)
        }
      }
    }
    {
      // ToString/FromStr
      // 只要实现 Display，ToString 就自动实现
      // FromStr 是从字符串切片转换，可能出错，返回 result
    }
  }
  {
    // 运算符重载
    // rust 允许部分运算符重载，重载的方式是实现预定义好的trait
    {
      // add 的定义
      trait MyAdd<RHS=Self> {
        type Output;
        fn add(self,rhs:RHS) -> Self::Output;
      }

      /*
      impl MyAdd<i32> for i32 {
        type Output = i32;
        fn add(self,rhs:RHS) -> i32 {
           ...
        }
      }*/
    }
    {
      // 自定义复数类型加法运算的实现
      use std::ops::Add;

      #[derive(Copy,Clone,Debug,PartialEq)]
      struct Complex {
        real: i32,
        imaginary: i32,
      }

      impl Add for Complex {
        type Output = Complex;
        fn add(self,other:Complex) -> Complex {
          Complex {
            real: self.real + other.real,
            imaginary:self.imaginary + other.imaginary,
          }
        }
      }

      let c1 = Complex {real:1,imaginary:1};
      let c2 = Complex {real:1,imaginary:1};
      println!("{:?}",c1+c2);
    }
  }
  {
    // IO
    {
      // 平台相关字符串
      {
        // rust 在标准库提供了 OsString/OsStr
        use std::path::PathBuf;
        let mut buf = PathBuf::from("/");
        buf.set_file_name("bar");
        if let Some(s) = buf.to_str() {
          println!("{}",s);
        } else {
          println!("invalid path");
        }
      }
    }
    {
      // 文件和路径
      {
        // rust 使用 PathBuf 和 Path 来处理路径
        // 对应 OsString 和 OsStr
        // 位于 std::path
      }
      {
        // 对于文件的操作依靠 std::fs::File 以及 std::fs 下相关函数
        // 对文件的读写依靠 std::io 模块
        {
          // 用例
          use std::io::prelude::*;
          use std::io::BufReader;
          use std::fs::File;
          use dirs::home_dir;
          
          fn test_read_file() -> Result<(), std::io::Error> {
            let mut path = home_dir().unwrap();
            path.push(".rustup");
            path.push("settings");
            path.set_extension("toml");
            let file = File::open(path)?;
            let reader = BufReader::new(file);
            for line in reader.lines() {
              println!("Read a line: {}",line?);
            }
            Ok(())
          }

          match test_read_file() {
            Ok(_) => {},
            Err(e) => {
              println!("Error occurred: {}",e);
            }
          }
        }
      }
    }
    {
      // 标准输入输出
      // 在 C++ 中，cin、cout 是全局变量，在 rust 中，基于线程安全考虑，获取标准输入输出实例，需要调用函数
      // io::stdin() 和 io::stdout() 
      // 返回的类型时对应的结构体，本身实现了 Read 结构体，可以直接调用读写，但是每次都会上锁，效率较低
      // 好办法是手动的调用 lock()
      {
        use std::io::prelude::*;
        use std::io::BufReader;

        fn test_stdin() -> Result<(), std::io::Error> {
          let stdin = std::io::stdin();
          let handle = stdin.lock();
          let reader  = BufReader::new(handle);
          for line in reader.lines() {
            let line = line?;
            if line.is_empty() {
              return Ok(());
            }
            println!("Read a line: {}", line);
          }
          Ok(())
        }
        match test_stdin() {
          Ok(()) => {},
          Err(e) => {
            println!("Error occured: {}",e);
          }
        }
      }
    }
    {
      // 进程启动参数
      {
        if std::env::args().any(|arg| arg == "-kill") {
          std::process::exit(1); // 进程返回值
        }
        for arg in std::env::args() { // 进程输入参数
          println!("{}",arg);
        }
      }
    }
  }
  {
    // rust 标准库中实现了一个乞丐版的反射 std::any
    // 所有的类型都自动实现了这个 dyn trait
    // 可以把任何对象的引用转为 &Any
    // 还可以判断对象的类型，以及强制转换为某个类型，get_type_id 暂时要求 'static 约束，以后会放宽
    {
      use std::fmt::Display;
      use std::any::Any;

      fn log<T: Any+Display>(value: &T) {
        let value_any = value as &Any;
        if let Some(s) = value_any.downcast_ref::<String>() {
          println!("String: {}",s);
        } else if let Some(i) = value_any.downcast_ref::<i32>() {
          println!("i32: {}", i);
        } else {
          let type_id = value_any.get_type_id();
          println!("unknown type {:?}: {}", type_id, value);
        }
      }

      fn do_work<T: Any+Display>(value:&T) {
        log(value);
      }

      let my_string = "Hello World".to_string();
      do_work(&my_string);
      let my_i32: i32 = 100;
      do_work(&my_i32);
      let my_char: char = '-';
      do_work(&my_char);
    }
  }
}
