fn main() {
    // 详解 Send 和 Sync
    {
        // 什么是 Send
        // 如果一个类型可以安全地从一个线程 move 到另一个线程，那么就可以是 Send
        // 诸如 Vec 的容器需要泛型是 Send，自己才能是 Send
        // 诸如 Mutex 的容器，无论泛型参数是什么，一定是 Send
        // Rc 没有实现线程同步，如果 move 到其他线程，Rc 会导致不同线程中引用计数指向同一块内存
    }
    {
        // 什么是 Sync
        // Vec、Option、Box 等依赖其中的翻新参数是 Sync 的
        // Mutex 容器是 Sync
        // 所有具备内部可变性又没有考虑多线程同步性的类型都是不 Sync 的
        // Mutex 类型具有内部可变性
        // 不可变 Mutex 通过调用 lock 返回一个 MutexGuard，这个智能指针有权修改内部数据
        // 还有 RwLock<T>、AtomicBool、AtomicIsize、AtomicUSize、AtomicPtr 等
    }
    {
        // 自动推理
        // 如果是基本类型组成出来的，绝大多数类型都能够推理出来其 Send、Sync 性
        // 从而自动 impl trait 
        // Send，Sync 的判定无法由编译器自动检查出来
        // 因为是程序员判定，需要加注 unsafe 关键字
        // 如果有 Unsafe 代码，就需要用户手动实现
    }
}