#![feature(core_intrinsics)]
#![allow(dead_code)]
#![allow(unused_imports)]

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
            fn raw_to_ref<'a>(p: *const i32) -> &'a i32 {
                unsafe {
                    &*p
                }
            }

            fn _raw_to_ref_core_dump() {
                let p: &i32 = raw_to_ref(std::ptr::null::<i32>());
                println!("{}", p);
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
            println!("{:?}", p);
        }
    }
    {
        // 内置函数
        {
            // transmute
            {
                // transmute 转换要求是 二进制长度 相等
                // 使用 as 转型无法做到检测 二进制长度 相等
                let x = vec![1, 2, 3, 4, 5];
                unsafe {
                    let t: (usize, usize, usize) = std::mem::transmute_copy(&x);
                    println!("{} {} {}", t.0, t.1, t.2);
                };
            }
        }
        {
            // 内存读写
            use std::intrinsics;
            {
                // ptr::copy
                // 和 memmove 很像，假定了 src 和 dst 所指向的内存可能重叠
                // unsafe fn _copy<T> (src:*const T,des:* mut T,count:usize);

                // 和 memcpy 很像，都假定了用户已经保证了 src 和 dst 所指向的内存不重叠
                // ptr::copy_nonoverlapping
                // 签名类似，更快
            }
            {
                // ptr::write
                // unsafe fn write<T>(dst: *mut T, src: T);
                // 将变量写入内存中，基于 move_val_init 实现
                // 在写的过程中，覆盖 dst，并且 src 不会执行析构函数
                // 写内存的还有 ptr::write_bytes ptr::write_unaligned ptr::write_volatile
            }
            {
                // ptr::read
                // unsafe fn read<T>(src: *const T)->T;
                // 基于 copy_nonoverlapping 实现
                // 还有 ptr::read_unalign ptr::read_volatile
            }
            {
                // ptr::swap
                // unsafe fn swap<T>(x:*mut T,y:*mut T);
                // 交换两个指针的内容
                // mem 中还有两个引用的交换
                // 其它的还有具有内部可变性的对象之间的非 mut 交换方法
            }
            {
                // ptr::drop_in_place
                // unsafe fn drop_in_place<T: ?Sized>(to_dropped: *mut T);
                // 执行当前对象的析构函数，没有就不执行
            }
            {
                // uninitialized
                // unsafe fn uninitialized<T>() -> T;
                // 基于 intrinsics::uninit 实现的
                // 调用这个函数会导致缺少初始化
            }
        }
        {
            use std::mem;
            use std::intrinsics;
            use std::ptr;
            // 综合示例
            fn my_swap<T>(x: &mut T, y: &mut T) {
                unsafe {
                    let mut t: T = mem::uninitialized();
                    ptr::copy_nonoverlapping(&*x, &mut t, 1);
                    ptr::copy_nonoverlapping(&*y, x, 1);
                    ptr::copy_nonoverlapping(&t, y, 1);
                    mem::forget(t);
                }
            }
        }
    }
    {
        // 分割借用
        {
            struct Foo {
                a: i32,
                b: i32,
                c: i32,
            }
            let mut x = Foo { a: 0, b: 0, c: 0 };
            let pa = &mut x.a;
            let pb = &mut x.b;
            let pc = &x.c;
            *pb += 1;
            let pc2 = &x.c;
            *pa += 10;
            println!("{} {} {} {}", pa, pb, pc, pc2);
        }
        {
            // 整体引用会导致编译器无法知晓是否交叉
            // tuple struct 可能也有类似问题，因为field的index不是编译确定，并且如果是字典还有可能引用交叉
            let mut x = [1_i32, 2, 3];
            {
                let (first, rest): (&mut [i32], &mut [i32]) = x.split_at_mut(1);
                let (second, third): (&mut [i32], &mut [i32]) = rest.split_at_mut(1);
                first[0] += 2;
                second[0] += 4;
                third[0] += 8;
                println!("{:?} {:?} {:?}", first, second, third);
            }
            println!("{:?}", &x);
        }
        {
            // split_at_mut 实现, 本来是方法，以下为了编译成功改成泛型
            use std::marker::Sized;
            use std::slice::from_raw_parts_mut;
            #[inline]
            fn _split_at_mut<T>(t: &mut [T], mid: usize) -> (&mut [T], &mut [T]) where [T]: Sized {
                let len = t.len();
                let ptr = t.as_mut_ptr();
                unsafe {
                    assert!(mid <= len);
                    (from_raw_parts_mut(ptr, mid), from_raw_parts_mut(ptr.offset(mid as isize), len - mid))
                }
            }
        }
        {
            /*
                unsafe 一定不能滥用，尽量把 unsafe 代码封装在一个较小的范围内， 对外公开的是
                safe 的 api；千万不能自欺欺人，把应该 unsafe 的函数代码，标记成普通函数
            */
        }
    }
    {
        // 协变
        {
            // 什么是协变
            // 协变：若 T1 <: T2 时, 满足 C<T1> <: C<T2>， 则称之为协变
            // 逆变：若 T1 <: T2 时, 满足 C<T2> <: C<T1>， 则称之为逆变
            // 不变：以上都不满足
            // rust 和 C# 不同，不支持泛型类型的协变标记，只支持泛型中生命周期标记的协变
            // 既然这边被指向的生命周期内都是合法的，那么一个较短的生命周期内也一定是合法的
            type StrRef<'a> = &'a str;
            fn print_str<'b>(s: StrRef<'b>) {
                println!("{}", s);
            }
            let s: StrRef<'static> = "hello";
            print_str(s);
            // 借用指针支持协变
        }
        {
            fn test<'a>(s: &'a &'static str) {
                let _local: &'a &'a str = s;
            }
            // pass, 借用指针支持逆变
        }
        {
            /* fn test<'a>(s: &'a mut &'static str) {
                let _local: & 'a mut & 'a str = s;
            }*/
            // fail, 可变借用指针协变性为不变
        }
        {
            fn test<'a>(s: Box<&'static str>) {
                let _local: Box<&'a str> = s;
            }
            // box 指针支持逆变
        }
        {
            fn test_arg<'a>(f: fn(&'a str)) {
                let _local: fn(&'static str) = f;
            }
            fn test_ret<'a>(f: fn() -> &'static str) {
                let _local: fn() -> &'a str = f;
            }
            // 函数 fn(T)->U 对 T 支持协变，对 U 支持逆变
            // 常数可以视为 fn() -> U 作为特例，支持逆变
        }
        {
            // 内部不变性类型如果支持协变性，则可能会造成悬空指针
            // 裸指针逆变，可变裸指针不变
            // 在写 unsafe 代码，特别是涉及 泛型的时候，常常要使用 PhantomData 类型来表达泛型参数的协变关系
        }
        {
            // 在 rust 中， 如果一个泛型参数从未被使用，那么就是一个编译错误
            // PhantomData 没有运行期开销，只在类型系统层面有意义。
            use std::marker::PhantomData;
            pub struct MyUnique<T:?Sized> {
                // pointer: NoZero<*const T>,
                _marker: PhantomData<T>
            }
        }
        {
            // 可以用来表示不可 Send，Sync
            use std::marker::PhantomData;
            struct MyStruct {
                data: String,
                _marker: PhantomData<* mut ()>,
            }
        }
        {
            use std::fmt::Debug;
            use std::ptr::null;

            #[derive(Clone,Debug)]
            struct S;

            #[derive(Debug)]
            struct R<T:Debug> {
                x: *const T
            }

            let mut r = R { x:null() };
            {
                let local = S{};
                r.x = &local;
            }
            // r.x 是空悬指针
        }
        {
            use std::fmt::Debug;
            use std::ptr::null;
            use std::marker::PhantomData;

            #[derive(Clone,Debug)]
            struct S;

            #[derive(Debug)]
            struct R<'a,T:Debug+'a> {
                x: *const T,
                marker: PhantomData<&'a T>
            }

            impl<'a,T:Debug> Drop for R<'a,T> {
                fn drop(&mut self) {
                    unsafe {
                        println!("Dropping R while S {:?}", *self.x);
                    }
                }
            }

            impl<'a, T:Debug + 'a> R<'a,T> {
                pub fn ref_to<'b: 'a>(&mut self,obj:&'b T) {
                    self.x = obj;
                }
            }

            /* let mut r = R {x:null(),marker: PhantomData};
            {
                let local = S{};
                r.ref_to(&local);
            }*/
            // 编译期成功的发现了生命周期错误
        }
    }
    {
        // 未定义行为
        // rust 中，只有 unsafe 才能出现未定义错误
        {
            /*
                1 数据竞争
                2 解引用空指针或者空悬指针
                3 使用未对齐指针读写内存
                4 读取未初始化内存
                5 破坏了指针别名规则
                6 通过共享的引用修改对象
                7 调用编译器内置函数制造 UB
                8 给内置类型赋予非法值
            */
        }
    }
    {
        // unsafe 主要提供面向更底层的接口和 FFI
        // 既不能过于依赖 unsafe，也不能心怀恐惧，只是表明代码的安全性依赖于某些情况
        // 无法使用编译器能够理解的方式让编译器替我们自动检查
    }
}