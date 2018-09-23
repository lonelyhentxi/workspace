#[allow(unused_variables)]
fn main() {
    {
        {
            // 数组
            {
                let xs: [i32; 5] = [1, 2, 3, 4, 5];
                let ys: [i32; 500] = [0; 500];
            }
            {
                // 内置方法
                let v = [0_i32; 10];
                for i in &v {
                    println!("{:?}", i);
                }
            }
            {
                // 多维数组
                let v: [[i32; 2]; 3] = [[0, 0], [0, 0], [0, 0]];
                for i in &v {
                    println!("{:?}", i);
                }
            }
            {
                // 数组切片
                fn mut_array(a: &mut [i32]) {
                    a[2] = 5;
                }
                println!("size of &[i32;3] : {:?}", std::mem::size_of::<&[i32; 3]>());
                println!("size of &[i32] : {:?}", std::mem::size_of::<&[i32]>());
                let mut v: [i32; 3] = [1, 2, 3];
                {
                    let s: &mut [i32; 3] = &mut v;
                    mut_array(s);
                }
                println!("{:?}", v);
            }
            {
                // dst 和胖指针
                fn raw_slice(arr: &[i32]) {
                    unsafe {
                        let (var1, var2): (usize, usize) = std::mem::transmute(arr);
                        println!("Value in raw pointer");
                        println!("Value1: {:x}", var1);
                        println!("Value2: {:x}", var2);
                    }
                }
                let arr: [i32; 5] = [1, 2, 3, 4, 5];
                let address: &[i32; 5] = &arr;
                println!("Address of arr: {:p}", address);
                raw_slice(address as &[i32]);
                // 只能通过指针来创建和操作 dst
                // 局部变量和函数参数的类型不能是 dst 类型，因为它们必须在编译期了解内存大小
                // 等到 unsized rvalue 实现，该功能可能会解锁
                // enum 中不能含有 dst 类型，struct 只有最后一个元素可以使 dst 类型，其他地方不能确定内存
            }
            {
                // range
                {
                    let r = 1..10;
                    for i in r {
                        print!("{:?}\t", i);
                    }
                }
                // equal to
                {
                    use std::ops::Range;
                    let r = Range {start:1,end:10};
                    println!(" ");
                    for i in r {
                        print!("{:?}\t",i);
                    }
                }
                // rev 把这个方法反过来
                {
                    use std::iter::Iterator;
                    let r = (1i32..11).rev().map(|i| i*10);
                    println!(" ");
                    for i in r {
                        print!("{:?}\t",i);
                    }
                }
                {
                    let arr: [i32;5] = [1,2,3,4,5];
                    fn print_slice(arr:&[i32]) {
                        for it in arr {
                            print!("{}\t",it);
                        }
                    }
                    println!(" ");
                    print_slice(&arr[..]);
                    // ..end rangeto )
                    // start.. rangefrom [
                    // .. fullrange
                    // start..=end (]
                    // ..=end ]
                }
            }
            {
                // 边界检查
                {
                    println!(" ");
                    let v = [10i32,20,30,40,50];
                    let index: usize = std::env::args().nth(1).map(|x| x.parse().unwrap_or(0)).unwrap_or(0);
                    println!("{:?}",v[index]);
                }
                {
                    let v = [10i32,20,30,40,50];
                    let first = v.get(0);
                    let second = v.get(10);
                    println!("{:?} {:?}", first, second);
                    // 在 release 模式将不执行边界检查
                }
                {
                    use std::iter::Iterator;
                    let v = &[10i32,20,30,40,50];
                    for (index,value) in v.iter().enumerate() {
                        println!("{} {}",index,value);
                    }
                    let item = v.iter().filter(|&x| *x%2==0).nth(2);
                    println!("{:?}",item);
                }
            }

        }
        {
            // 字符串
            {
                // &str
                let greeting: &str = "Hello";
                let substr: &str = &greeting[2..];
                println!("{}", substr);
                println!("Size of pointer: {}", std::mem::size_of::<*const ()>());
                println!("Size of &str: {}", std::mem::size_of::<&str>());
            }
            {
                // String, 拥有管理空间的权力
                let mut s = String::from("Hello");
                s.push(' ');
                s.push_str("World.");
                println!("{}",s);
                // 它实现了 Deref<Target=str> 的 trait，所以在大多数情况下，&String可以被编译器转换
                // 为 &str 类型
                fn capitalize(substr: &mut str) {
                    substr.make_ascii_uppercase();
                }
                let mut s=String::from("Hello World");
                capitalize(&mut s);
                println!("{}", s);
            }
        }
    }
}