#include <future>
#include <iostream>
#include <vector>

int main() {
    /**
     * 优先使用基于任务而非基于线程的程序设计
     */
    // 基于任务的程序设计可以方便的得到任务的返回值
    // 基于任务，可以通过get得到异常，而基于线程的设计，将会调用 std::terminal
    // 1. 硬件线程是实际执行计算的线程；
    // 2. 软件线程是操作系统用以实施跨进程管理，以及进行硬件线程调度的线程；非阻塞的线程能够提升吞吐率；
    // 3. std::thread 是 c++ 提供的线程，用作底层软件线程的句柄；null状态表示：默认构造，被移动，被连结，被分离
    // 软件线程是有限的资源，一旦超过，就会抛出std::system_error
    // 线程切换带来的上下文切换会带来性能问题：不能命中缓存；或者污染为别的线程准备好的数据；
    // 对于需要访问底层线程实现的api（使用native_handle）；或者有能力为应用优化线程用法的情况下；需要超越 c++ 并发技术的线程技术的时候；
    // 这些情况下，使用线程更为合理
    /**
     * 如果异步是有必要的，则指定std::launch::async
     */
    {
        // std::launch::async 启动策略意味着函数以异步的方式运行
        // std::launch::deferred 意味着只有在期待返回的值的get和wait得到调用的时候才会开始运行
        auto task_counter = 0;
        auto f = [&task_counter]() mutable -> int {
            // std::cout << "task running..." << std::endl;
            ++task_counter;
            return task_counter;
        };
        auto fut1 = static_cast<std::shared_future<int>>(std::async(f));
        auto fut2 = static_cast<std::shared_future<int>>(std::async(std::launch::async | std::launch::deferred, f));
        auto fut3 = static_cast<std::shared_future<int>>(std::async(std::launch::async, f));
        auto fut4 = static_cast<std::shared_future<int>>(std::async(std::launch::deferred, f));
        std::vector<std::shared_future<int>> tasks{fut1, fut2, fut3, fut4};
        auto counter = 0;
        for (auto &task:tasks) {
            counter++;
            std::cout << "fut " << counter << " end at " << task.get() << std::endl;
        }
        // 默认的策略无法预知是否并发
        // 默认的策略无法预知是否在不同的线程上
        // 无法预知是否会运行
    }
}