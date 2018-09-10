#include <future>
#include <iostream>
#include <vector>
#include <mutex>
#include <condition_variable>

namespace launch_async {
    template <typename F,typename ...Ts>
    inline auto
    reallyAsync(F&&f,Ts&&...params){
        return std::async(std::launch::async,std::forward<F>(f),std::forward<Ts>(params)...);
    }
}

namespace always_joinable {
    constexpr auto tenMillion = 10'000'000;
    // ugly function
    bool doWork(std::function<bool(int)> filter,int maxVal = tenMillion) {
        std::vector<int> goodVals;
        std::thread t([&filter,maxVal,&goodVals]{
            for(auto i=0;i<=maxVal;i++)
            {
                if(filter(i)) {
                    goodVals.emplace_back(i);
                }
            }
        });
        auto nh = t.native_handle();
        // ...
        return false;
    }

    class ThreadRAII {
    public:
        enum class DtorAction {join,detach};
        ThreadRAII(std::thread&&t,DtorAction a):action{a},t{std::move(t)} {}
        ThreadRAII(ThreadRAII&&) noexcept = default;
        ThreadRAII&operator=(ThreadRAII&&) noexcept = default;

        ~ThreadRAII(){
            if(t.joinable()) {
                if(action==DtorAction::join){
                    t.join();
                } else {
                    t.detach();
                }
            }
        }

        std::thread& get() {return t;}
    private:
        DtorAction action;
        std::thread t;
    };
}

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
        // 默认的策略无法预知是否在不同的线程上，因而不能很好的运用局部存储
        // 无法预知是否会运行，如果调用 wait_for 或者 wait_until 可能会一直运行下去
        {
            using namespace std::literals;
            auto sentences = std::vector<std::string>{};
            auto func = [&sentences](){
                sentences.emplace_back("sleeping ...");
                std::this_thread::sleep_for(1s);
            };
            {
                auto fut = std::async(std::launch::deferred,f);
                while(fut.wait_for(100ms)==std::future_status::ready) {
                    std::cout << "never happened!"<<std::endl;
                }
            }
            // 如果任务被推迟了，调用异步 async
            {
                auto fut = std::async(func);
                if(fut.wait_for(0s)==std::future_status::deferred) {
                    fut.get();
                } else {
                    while(fut.wait_for(100ms)!=std::future_status::ready) {
                        sentences.emplace_back("waiting...");
                    }
                }
                for(const auto &s:sentences){
                    std::cout<<s<<std::endl;
                }
                // 默认的async适用于任务不需要与get或者wait的线程并发执行
                // 读写线程的thread_local变量无影响
                // 或者可以给出保证在future上调用get或wait，或者可以接受任务永不执行
                // 将使用 wait_for 或者 wait_until 的代码必须将任务可能被推迟纳入考量
            }
        }
    }
    /**
     * 使得std::thread对象在所有路径都不可联结
     */
    {
        // 可联结的线程一旦被析构,程序的执行就会终止
        // 阻塞和等待调度，是可联结的

        // 不可连结的包括：默认构造；已移动的；已联结的；已分离的
    }
    /**
     * 对变化多端的线程句柄析构函数行为保持关注
     */
    {
        // 指涉到经由 std::async 启动的未推迟任务的共享状态的最后一个期望值会阻塞，直到任务结束。
        // 其它所有有期望值的对象，仅仅将期望值对象析构就结束了。
    }
    /**
     * 考虑针对一次性事件通信以使用 void 为模板类型实参的期望值
     */
    {
        {
            // 设计1，能够运作，但是不够干净利落
            std::condition_variable cv;
            std::mutex m;
            bool flag(false);
            auto send_func = [&](){
                std::cout<<"initializing ...."<<std::endl;
                std::lock_guard<std::mutex> g(m);
                flag = true;
                cv.notify_one();
            };
            auto recv_func = [&](){
                std::unique_lock<std::mutex> lk(m);
                cv.wait(lk,[&]{return flag;});
                std::cout<<"finish initializing!"<<std::endl;
            };
            auto fut1 = std::async(std::launch::async,recv_func);
            auto fut2 = std::async(std::launch::async,send_func);
        }
        {
            //设计2，只能使用一次
            std::promise<void> p;
            auto react_func = [](){
                std::cout<<"received, react"<<std::endl;
            };
            auto detect_func = [&]{
                std::thread t([&]{
                    p.get_future().wait();
                    react_func();
                });
                std::cout<<"setting value..."<<std::endl;
                p.set_value();
                std::cout<<"ready to send..."<<std::endl;
                t.join();
            };
            detect_func();
        }
        {
            // 发送多份的版本
            std::promise<void> p;
            auto detect = [&]{
                std::cout<<"sending..."<<std::endl;
                auto sf = p.get_future().share();
                std::vector<std::thread> vt{};
                vt.reserve(3);
                std::mutex m;
                for(int i=0;i<4-1;i++) {
                    vt.emplace_back([&m,sf,i](){
                        sf.wait();
                        std::lock_guard<std::mutex> lk{m};
                        std::cout<<"I'm "<<i+1<<" th, received"<<std::endl;
                        return;
                    });
                }
                p.set_value();
                for(auto &t:vt){
                    t.join();
                }
            };
            detect();
        }
    }
    /**
     * 对于并发使用std::atomic,对于特种内存使用volatile
     */
    {
        {
            // atomic 的原子方式只限于值的读取
            // atomic 上所有的成员函数都被其它线程视为原子的
            std::atomic<int> ai{0};
            ai  = 10;
            std::cout<<ai<<std::endl;
            ++ai;
            --ai;
        }
        {
            // volatile 几乎在多线程语境中不能提供任何保证
            // volatile 的意思是提示编译器这是特种内存，不要对在此内存上的操作作任何优化
        }
    }
}
