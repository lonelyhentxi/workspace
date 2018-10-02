fn main() {
    // 动态分配和静态分配
    {
        /*
            静态分派指调用哪个函数，在编译期就已经确定下来
            动态分派指调用哪个函数，在运行期确定
        */
    }
    {
        //  trait object
        {
            // 指向 trait 的指针就是 trait object
            // 假定 Bird 是一个 trait 名称，dyn Bird 就是一个 DST 动态大小类型
            // dyn 是一个新引入的上下文关键字，目前还没有稳定，需要打开 #![feature(dyn_trait)]
            // 在 1.3.0 中这个特性已经成为稳定特性了
            // 指向编译期不确定大小的类型的指针都是胖指针，因此 to 也是胖指针
            /* 可以理解为
             pub struct TraitObject {
                pub data: *mut (),
                pub vtable: *mut (),
             }
             vtable 为虚函数表
            */
            // 在 C++ 里，对象本身携带指向虚函数表的地址；
            // 在 rust 里，指向虚函数表的地址由 trait object 携带；
        }
    }
    {
        // object safe
        // trait object 收到许多的约束
        {
            // 当 trait 有 Self:Sized 约束时
            // trait 只是描述了公共的行为，不描述的具体内部实现
        }
        {
            // 当 trait 中有 Self 或相关类型作为参数或者返回类型时（第一个参数self除外，指向自身）
            // 原类型被抹除了信息，无法识别
            // 当部分类型满足，而其他类型不满足时，可以使用 :Sized 将其从虚函数表中去除
            {
                trait Double {
                    fn new() -> Self where Self:Sized;
                    fn double(&mut self);
                }

                impl Double for i32 {
                    fn new() -> i32 { 0 }
                    fn double(&mut self) {  *self *= 2; }
                }

                let mut i = 1;
                let p: &mut dyn Double = &mut i as &mut dyn Double;
                p.double();
            }
            // 当函数的第一个参数不是self时，使用 :Sized 将之从虚函数表中去除
            // 当函数有泛型参数时，泛型函数是从虚函数表中去除的
            // C++ 同样规定了类的虚成员函数不可以是 template 方法
        }
    }
    {
        // impl trait
        // 在 1.30.0 中，已经进入 stable feature
        {
            use std::iter::Iterator;
            fn _foo(n: u32) -> impl Iterator<Item=u32> {
                (0..n).map(|x| x*100)
            }
        }
        {
            // 确实返回类型的静态分派可能遇到的问题
            // 返回复杂的类型的时候，不仅书写麻烦，而且泄露了具体的实现
            // 函数无法直接返回一个闭包
        }
        {
            // 当下 impl trait 使用场景非常保守
            // 但是设想中的该提案已经拓展到非常多的地方去了
            {
                // 用在函数参数中
                // fn test(f: impl Fn(i32) -> i32) {}
                // 用在类型别名中
                // type MyIter = impl Iterator<Item=i32>
                // 让 impl trait 用在 trait 中的方法或返回值中
                // trait Test {
                //  fn test() -> impl MyTrait;
                // }
                // 用在关联类型中：
                // trait test {
                //      type AT = impl MyTrait;
                // }
            }
        }
        {
            // 不要过于激进的使用 impl Trait 功能，因为相比推导的类型，损失了表达能力
        }
    }
}