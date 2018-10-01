fn main() {
    // 解引用
    {
        let v1 = 1;
        let p = &v1;
        let v2 = *p;
        println!("{} {}", v1, v2);
    }
    {
        // 自定义解引用
        {
            pub trait MyDeref {
                type Target: ?Sized;
                fn deref(&self) -> &Self::Target;
            }

            pub trait MyDerefMut: MyDeref {
                fn deref_mut(&mut self) -> &mut Self::Target;
            }
        }
        {
            // for String s
            // s -> String, &s -> &String
            // Deref::Target -> str
            // Deref::deref() -> &str
            // s.deref() -> &str
            // *s -> str
            // &*s -> &str
        }
        {
            // Box<T> 是指向在堆分配的对象
            // Vec<T> 指向一组同类型在堆上分配的对象
            // String 指向堆上分配的字节数组
            // Rc<T> 和 Arc<T> 是某种形式、携带了额外元数据的指针，提供的是引用计数的共享所有权
            // shared_ptr ?
        }
    }
    {
        // 自动解引用
        {
            let s = "hello";
            println!("length: {}", s.len()); // self 可以自动使用 &self
            println!("length: {}", (&s).len());
            println!("length: {}", (&&&&&&&&&&&&&&&&&s).len());
            // 自动 deref 的规则是，如果类型 T 可以解引用为 U，即 T:Deref<U>,则 &T 可以自动转换为 &U
        }
    }
    {
        // 自动解引用的用处
        {
            #[allow(dead_code)]
            #[allow(unused_imports)]
            use std::ops::Deref;
            #[allow(unused_imports)]
            use std::rc::Rc;

            /*impl<T:?Sized> Deref for MyRc<T> {
                type Target = T;
                #[inline(always)]
                fn deref(&self) -> &T {
                    &self.inner().value;
                }
            }*/
        }
        {
            use std::rc::Rc;
            let s = Rc::new(String::from("hello"));
            println!("{:?}", s.bytes());
        }
        {
            fn joint() {
                let s = Box::new(String::new());
                let p = &*s;
                println!("{} {}", p, s);
            }
            // 成功，直接 deref
            /*
            fn separate() {
                let  s = Box::new(String::new());
                let tmp = *s;
                let p = &tmp;
                println!("{} {}", p, s);
            }*/
            // fail, 移动语义+借用右值
            joint();
        }
    }
    {
        // 有时候需要手动处理
        use std::rc::Rc;
        use std::ops::Deref;
        // rc.clone 复制指针 string.clone 复制内容
        {
            let s = Rc::new(Rc::new(String::from("hello")));
            let _s1 = s.clone();
            let _ps1 = (*s).clone();
            let _pps1 = (**s).clone();
        }
        {
            let s = String::new();
            {
                match s.deref() {
                    "" => println!("\"\""),
                    _ => println!("other")
                }
            }
            {
                match &*s {
                    "" => println!("\"\""),
                    _ => println!("other")
                }
            }
            {
                match s.as_ref() {
                    "" => println!("\"\""),
                    _ => println!("other")
                }
            }
            {
                use std::borrow::Borrow;
                match s.borrow() {
                    "" => println!("\"\""),
                    _ => println!("other")
                }
            }
            {
                match &s[..] {
                    "" => println!("\"\""),
                    _ => println!("other")
                }
            }
        }
    }
    {
        // 智能指针
        {
            use std::rc::Rc;

            struct SharedValue {
                value: i32
            }
            let shared_value: Rc<SharedValue> = Rc::new(SharedValue { value: 42 });
            let owner1 = shared_value.clone();
            let owner2 = shared_value.clone();
            println!("value : {} {}", owner1.value, owner2.value);
            println!("address : {:p} {:p}", &owner1.value, &owner2.value)
        }
    }
    {
        // 智能指针
        {
            // 引用计数
            use std::rc::Rc; // 类似 shared_ptr<const T>
        // 当逻辑上不可变的方法的实现细节又要求某部分成员变量具有可变性的时候
        // 我们可以使用内部可变性
        use std::cell::RefCell;

            let shared_vec = Rc::new(RefCell::new(vec![1, 2, 3]));
            let shared1 = shared_vec.clone();
            let shared2 = shared1.clone();
            shared1.borrow_mut().push(4);
            println!("{:?}", shared_vec.borrow());
            shared2.borrow_mut().push(5);
            println!("{:?}", shared_vec.borrow());
        }
    }
    {
        // Cow
        {
            #[allow(dead_code)]
            pub enum MyCow<'a, B: ?Sized + 'a> where B: ToOwned {
                Borrowed(&'a B),
                Owned(<B as ToOwned>::Owned),
            }
        }
        {
            use std::borrow::Cow;

            fn remove_spaces(input: & str) -> Cow<str> {
                if input.contains(' ') {
                    let mut buf = String::with_capacity(input.len());
                    for c in input.chars() {
                        if c != ' ' {
                            buf.push(c);
                        }
                    }
                    return Cow::Owned(buf);
                }
                return Cow::Borrowed(input);
            }

            let s1 = "no_spaces_in_string";
            let result1 = remove_spaces(s1);
            let s2 = "spaces in string";
            let result2 = remove_spaces(s2);
            println!("{}\n{}", result1,result2);
            // Cow 类型既能够满足编译器的生命周期要求，也避免了不需要的复制
        }
    }
}