#![feature(lang_items)]
#![allow(dead_code)]
fn main() {
    {
        // rust 中 mut 关键字不能再声明类型的时候使用，只能和类型使用
        // rust 中变量具有承袭的可变性
        // cell 和 refcell 只在单线程的情况下具备内部可变性
    }
    {
        // cell
        // 内部可变性，虽然具有共享的引用，但是内部值会改变
        {
            use std::rc::Rc;
            let r1 = Rc::new(1);
            println!("reference count {}", Rc::strong_count(&r1));
            let r2 = r1.clone();
            println!("reference count {}", Rc::strong_count(&r2));
        }
        {
            use std::cell::Cell;
            let data: Cell<i32> = Cell::new(100);
            let p = &data;
            data.set(10);
            println!("{}",p.get());
            p.set(20);
            println!("{:?}",data);
            // 是对 alias + mutation 规则的有益补充
        }
    }
    {
        // RefCell
        {
            use std::cell::RefCell;
            let shared_vec: RefCell<Vec<isize>> = RefCell::new(vec![1,2,3]);
            let shared_vec1 = &shared_vec;
            let shared_vec2 = &shared_vec1;
            shared_vec1.borrow_mut().push(4);
            println!("{:?}",shared_vec.borrow());
            shared_vec2.borrow_mut().push(5);
            println!("{:?}",shared_vec.borrow());
            // RefCell 抛弃了编译阶段的 alias+mutation 原则，采用运行期检查
        }
        // RefCell 内部有一个借用计数器
    }
    {
        // UnsafeCell
        {
            // cell v1
            struct CellV1<T> {
                value:T
            }

            impl<T> CellV1<T> {
                fn new(v:T) -> Self where T: Copy {
                    CellV1 {value:v}
                }

                fn set(&self, v:T) where T: Copy {
                    unsafe {
                        let p  = &(self.value) as *const T as *mut T;
                        *p = v;
                    }
                }

                fn get(&self) -> T where T:Copy {
                    self.value
                }
            }
            // 存在野指针的风险，unsafe 导致生命周期推断失效
        }
        {
            /* #[lang="unsafe_cell"]
            #[stable(feature="rust1",since="1.0.0")]
            pub struct MyUnsafeCell<T:?Sized> {
                value: T,
            }*/
            // 所有具有内部可变性特点的类型都必须基于 UnsafeCell 来实现
            // 这个类型是唯一合法的将 &T 类型转换为 &mut T 类型的办法
        }
    }
}