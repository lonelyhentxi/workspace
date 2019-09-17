#![feature(const_fn)]
#[allow(unused_variables)]
#[allow(unused_parens)]
#[allow(dead_code)]
fn main() {
    fn test(condition: bool) {
        let x: i32;
        if condition {
            x = 1; // initialize, not assignment
            println!("{}", x);
        } else {
            // if not condition, no initialization, but if not use, pass else {
            println!("pass");
        }
    }

    test(true);
    test(false);

    let x = "hello";
    println!("x is {}", x);
    let x = 5;
    println!("x is {}", x);
    // 允许变量遮蔽，允许不同类型
    let mut v = Vec::new();
    v.push(1);
    v.push(2);
    v.push(3);
    // 类型推断
    let v = v; // 变量遮蔽，成为不可变类型
    for i in &v {
        println!("{}", i);
    }
    let player_scores = [("Jack", 20), ("Jane", 23), ("Jill", 18), ("John", 19)];
    let players: Vec<_> = player_scores.iter().map(|&(player, _score)| { player }).collect();
    println!("{:?}", players);
    type Age = u32; // 类型别名
    // 全局变量必须当场初始化，函数签名不能使用自动类型推导
    fn grow(age: Age, year: u32) -> Age {
        age + year
    }
    let x: Age = 20;
    println!("20 years later: {}", grow(x, 20));
    type Double<T> = (T, Vec<T>);
    let a: Double<i32> = (1, Vec::new());
    println!("{:#?}", a);
    // 必须是编译期可以确定的常量
    static G1: i32 = 3;
    println!("{}", G1);
    static mut G2: i32 = 4;
    unsafe { // 全局变量无论读写都必须使用 unsafe 标识
        G2 = 5;
        println!("{}", G2);
    }
    // rust 禁止在声明 static 变量的时候使用普通函数，或者是利用语句块调用其它非 const 代码，类似
    // c++ 的 constexpr（其实真正的constexpr 对应 const）
    use std::sync::atomic::AtomicBool;
    static FLAG: AtomicBool = AtomicBool::new(true);
    println!("{:?}", FLAG);
    // 如果需要使用较为复杂的全局初始化，可以使用 lazy_static 库
    const GLOBAL: i32 = 0; // 编译期确定可能没有内存空间
    // bool 略
    // char，unicode
    println!("{}", GLOBAL);
    let love = '❤';
    let c1 = '\n';
    let c2 = '\x7f';
    let c3 = '\u{7FFF}';
    let x: u8 = 1;
    let y: u8 = b'A';
    let s: &[u8; 5] = b"hello";
    let r: &[u8; 14] = br#"hello \n world"#;
    println!("{},{},{},{},{},{},{:?},{:?}", love, c1, c2, c3, x, y, s, r);
    // 整数类型
    // 可以在任意地方添加下划线，以方便阅读
    // 可以直接对内置类型调用方法
    // 对于整数溢出，debug 模式下，编译器会自动插入溢出检查，一旦发生则会 panic；
    // release下，溢出会抛弃高位
    /*
    fn arithmetic(m:i8,n:i8) {
        println!("{}", m+n);
    }

    let m: i8 = 120;
    let n: i8 = 120;
    arithmetic(m,n);
    */
    let i = 100_i8;
    println!("checked {:?}", i.checked_add(i));
    println!("saturating {:?}", i.saturating_add(i));
    println!("wrapping {:?}", i.wrapping_add(i));
    // 浮点类型
    let f1 = 123.0f64;
    let f2 = 0.1f32;
    let f3 = 12E+99_f64;
    let f4: f64 = 2.;
    println!("{},{},{},{}", f1, f2, f3, f4);
    let x = 1.0f32 / 0.0;
    let y = 0.0f32 / 0.0;
    println!("{} {}", x, y);
    // 因为 NaN 的存在，浮点数无法形成偏序关系
    let nan = std::f32::NAN;
    println!("{} {} {}", nan < nan, nan > nan, nan == nan);
    // 指针类型
    // Box<T> 指向T类型的，具有所有权的指针，有权释放内存
    // &T 指向T的借用指针，也称为引用，无权释放内存，无权写数据
    // &mutT 指向T的mut借用指针，无权释放内存，有权写数据
    // *const T指向类型T的只读裸指针，没有生命周期信息，无权写数据
    // *mut T指向T的可读写裸指针，没有生命周期信息，有权写数据
    // Rc<T> 指向类型T的引用计数指针，共享所有权，线程不安全，类似 shared_ptr
    // Arc<T> 指向类型T的原子型引用指针，共享所有权，线程安全
    // Cow<'a,T> Clone-on-Write 写时复制指针。可能是借用指针，也可能是具有所有权的指针。
    // 类型转换
    // rust 的类型转换控制的非常严格
    let var1: i8 = 41;
    let var2: i16 = var1 as i16;
    println!("{}", var2);
    // as 类似于 static_cast 只能用在合理的转换上
    let i = 42;
    let p = &i as *const i32 as *mut i32;
    println!("{:p}", p);
    // 复合数据类型
    // tuple
    let a = (1i32, false);
    let a = (0, ); // tuple, like python
    let b = (0); // parentheses
    let c = (); // empty tuple，内存大小为 0
    println!("size of '()' {}", std::mem::size_of::<()>());
    // struct 结构体的每个变量都有名字
    {
        struct Point {
            x: i32,
            y: i32,
        }
        let p = Point { x: 0, y: 0 };
        let Point { x, y } = p;
        let Point { x: px, y: py } = p;
        struct Point3d {
            x: i32,
            y: i32,
            z: i32,
        }
        fn default() -> Point3d {
            Point3d { x: 0, y: 0, z: 0 }
        }
        let origin = Point3d { x: 5, ..default() };
        let point = Point3d { z: 1, x: 2, ..origin };
        struct Foo1;
        struct Foo2();
        struct Foo3 {};
        // 空结构体，内存大小为0
    }
    // tuple struct
    {
        struct Color(i32, i32, i32);
        /* equal to
        struct Color1 {
            0: i32,
            1: i32,
            2: i32,
        }
        */
        struct T1 {
            v: i32
        }

        struct T2(i32);
        let v1 = T1 {v:1};
        let v2 = T2(1);
        // struct Inches(i32);
        // 帮助创建新的类型
        // type I = i32; // 并非新的类型
    }
    // enum
    {
        enum Number {
            Int(i32),
            Float(i32),
        }
        fn read_num(num: &Number){
            match num {
                &Number::Int(value) => println!("Integer {}",value),
                &Number::Float(value) => println!("Float {}",value),
            }
        }
        let n:Number = Number::Int(10);
        read_num(&n);
        // enum 内部的 variant 是一个函数
    }
    // 类型递归定义
    {
        struct Recursive {
            data: i32,
            rec: Box<Recursive>
        }
        // 只要使得 struct 可以计算内存大小，就可以递归的定义
    }
}