extern crate scoped_threadpool;

fn main() {
    {
        // 内存泄漏
        {
            use std::rc::Rc;
            use std::cell::RefCell;

            struct Node {
                next: Option<Rc<RefCell<Node>>>
            }

            impl Node {
                fn new() -> Node {
                    Node {next:None}
                }
            }

            impl Drop for Node {
                fn drop(&mut self) {
                    println!("drop");
                }
            }

            fn alloc_objects() {
                let node1 = Rc::new(RefCell::new(Node::new()));
                let node2 = Rc::new(RefCell::new(Node::new()));
                let node3 = Rc::new(RefCell::new(Node::new()));
                node1.borrow_mut().next = Some(node2.clone());
                node2.borrow_mut().next = Some(node3.clone());
                node3.borrow_mut().next = Some(node1.clone());
            }

            alloc_objects();
            println!("program finished.");
            // 对于以上的例子，可以使用 std::rc::Weak 来打破循环，避免泄漏
        }
    }
    {
        // 内存泄漏属于内存安全
        // 语言层面几乎无法彻底解决这样的情况 —— 彻底解决：无论使用了何种技巧，永远无法构造出内存泄漏
    }
    {
        // 析构函数泄漏
        // rust 的保证 “内存安全” 意味着只要不使用 unsafe， 用户永远不能构造出 unsafe
        // 对于内存泄漏，做不到这么强的保证
        {
            /* 于是
             1. 标准库中的 std::mem::forget 去掉了 unsafe 标记
             2. 允许带有析构函数的类型，作为 static 变量和 const 变量。全局变量的析构函数最后是泄漏
                掉了的，不会被调用（全局变量的析构函数永远不会调用）。
             3. 标准库中不安全代码需要依赖析构函数调用的逻辑得到修改，其中涉及 Vec::drain_range 和
                Thread::scoped 等方法。
             */
        }
        {
            // rust 的 std::mem::forget<T>(t:T)
            // 它会阻止编译器调用析构函数，也不会释放它在堆上申请的内存
            // 1 我们有一个未初始化的值，我们不希望它执行析构函数。
            // 2 在用 FFI 跟外部函数打交道的时候。
        }
        {
            // Thread::scope 函数被删除，这个函数保证子线程一定会在当前函数退出之前退出
            // 这个函数的实现原理是，它返回一个 JoinGuard 类型，在这个类型的析构函数当中阻塞当前线程
            // 等待线程结束。所以在函数退出之前，子线程必定已经被销毁，所以直接使用 &，&mut也不会造成
            // 野指针。
            // 改变为 RAII 式的实现后，scoped 函数就可以变成安全的了
            use scoped_threadpool::Pool;
            let mut pool = Pool::new(4);
            let mut vec = vec![0,1,2,3,4,5,6,7];
            pool.scoped(|scoped| {
                for e in &mut vec {
                    scoped.execute(move || {
                        *e+=1;
                    });
                }
            });
            println!("{:?}",vec);
        }
    }
}