#[allow(unused_parens)]
#[allow(unused_assignments)]
#[allow(unused_variables)]
#[allow(dead_code)]
#[allow(while_true)]
fn main() {
    // 表达式必然会产生一个值
    {
        // rust 禁止连续比较
        let a = false;
        println!("{}",a==(true==false));
    }
    // 赋值表达式
    {
        let mut x = 1;
        x = 2;
        let mut y = 2;
        let z = (y=x); // 赋值表达式返回的值是 unit, 为禁止连续赋值而设计
        println!("{:?}", z);
    }
    // 语句块表达式
    {
        let y: i32 = {
            println!("Hello.");5
        };
        print!("{}",y);
    }
    // if else
    {
        // 为了防止空悬 if，语句块中必须使用 {}
        // 可以当做表达式使用
        // 该表达式的各个分支的返回值类型必须一致
    }
    // loop
    {
        let mut m = 1;
        let n = 1;
        'a: loop {
            if m < 100 {
                m +=1;
            } else {
                'b: loop {
                    if m + n > 50 {
                        println!("break");
                        break 'a;
                    } else {
                        continue 'a;
                    }
                }
            }
        }
        // 可以在 loop，while，for循环前面加上 “ 生命周期标识”
        // let v = loop {}; // 无限循环将会返回发散类型
        // println!("{:#?}", v); // 永远不会达到
    }
    // while
    {
        let x;
        loop { x = 1; break; }
        println!("{}", x); // 编译器可以静态推断出 x 一定会初始化
        let x;
        while true { x=1;break; }
        // println!("{}", x); // 编译器不能推断出 x 在这里是否已经初始化
    }
    // for 循环
    let array = &[1,2,3,4,5];
    for i in array {
        println!("The number is {}", i);
    }
}