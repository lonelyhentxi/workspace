#include <vector>
#include <functional>
#include <iostream>
#include <chrono>
#include <iomanip>
#include <map>

namespace decltype_lambda {
    template<typename ...Args>
    void bar(Args ...params){
    }
}

int main() {
    /**
     * 避免默认捕获模式
     */
    {
        using FilterContainer = std::vector<std::function<bool(int)>>;
        FilterContainer filters;
        {
            int divisor = 5;
            filters.emplace_back([&divisor](int value) { return value % divisor == 0; }); // divisor 指针可能空悬
            filters.clear();
            filters.emplace_back([=](int value){return value%divisor==0;}); // 按值捕获，不会空悬
            // lambda 表达式只能捕获在创建lambda作用域内可见的非静态局部变量（包括形参）
            // 注意和this指针相关的“值”实际上捕获的是指针，而不是值本身
            // lambda 如果依赖静态储存期对象，即使使用按值捕获（实际上没有捕获任何东西），也存在被修改的风险
        }
    }
    /**
     * 使用初始化捕获将对象移入闭包
     */
    {
        auto foo = std::make_unique<int>(1);
        // 初始化捕获的等号左右处于不同的作用域
        auto func = [pw = std::move(foo)]{
            return *pw%2==0;
        };
        std::cout<<func()<<std::endl;
        // 在c++11中，还可以借助bind实现等价的代码
        auto foo1 = std::make_unique<int>(1);
        auto func1 = std::bind([](const auto &pw){return *pw%2==0;},std::move(foo1));
        std::cout<<func1()<<std::endl;
        // 默认情况下，闭包的operator() 带有const，为了防止使用 std::bind 所仿真的 "捕获参数" 被修改，可以标注为const
        // 需要修改的情况下，可以标注为 mutable
        auto foo2 = std::make_unique<int>(1);
        auto func2 = std::bind([](auto &pw) mutable {++(*pw);return *pw%2==0;},std::move(foo2));
        std::cout<<func2()<<std::endl;
    }
    /**
     * 对 auto&& 类型的形参使用 decltype 以 std::forward 之
     */
    {
        // c++ 14 支持泛型 lambda 式
        // 但是默认 auto 转发的值总是左值，如果想要传递的右值，那么就是传递失败
        auto f = [](auto x){return x;};
        auto f1 = [](auto &&x) {return Args(std::forward<decltype(x)>(x));};
        auto fs = [](auto &&...params){return Args(std::forward<decltype(params)>(params)...);};
    }
    /**
     * 优先使用 lambda 式，而非std::bind
     */
    {
        // lambda 具有更高的可读性
        using Time = std::chrono::system_clock::time_point;
        enum class Sound {Beep,Siren,Whistle};
        using Duration = std::chrono::system_clock::duration;
        auto setAlarm = [sounds = std::move(
                std::map<Sound,const std::string> {{Sound::Beep,"beep"},{Sound::Siren,"siren"},{Sound::Whistle,"whistle"}}
                )](const auto &t,const Sound &s,const Duration &d) {
            using namespace std;
            using namespace std::chrono;
            auto tmp_time = system_clock::to_time_t(t);
            cout<<"["<<std::put_time(std::localtime(&tmp_time),"%Y-%m-%d %H.%M.%S")<<"] "
            <<sounds.at(s)<<" for "<<d.count()<<endl;
        };
        auto setSoundL = [&](Sound s) {
            using namespace std::chrono;
            using namespace std::literals;
            setAlarm(system_clock::now()+1h,s,30s);
        };
        setSoundL(Sound::Beep);
        {
            using namespace std::chrono;
            using namespace std::literals;
            // wrong immediate calc
            auto setSoundB = std::bind(setAlarm,system_clock::now() + 1h,std::placeholders::_1,30s);
            setSoundB(Sound::Beep);
            // 在 c++ 14 中标准运算符模板的模板类型实参在大多数情况下可以不写
            auto setSoundB1 = std::bind(setAlarm,std::bind(std::plus<>(),system_clock::now(),1h),
                    std::placeholders::_1,30s);
            // 更有甚者，在重载的情况下，std::bind 由于转发，根本无法编译通过，除非指明特定的类型
            // 在 lambda 中，使用常规的函数唤起方式，便于编译器将之内联
            // 在 std::bind 中，实参是按值储存的，但是可以通过 std::ref 达到相同的效果
            // 在 std::bind 会按照完美转发的方式分发
            // 多态函数对象可以利用完美转发，可以被传递；移动捕获也可以实现；但是这些在c++14中都有了更加优雅的方式
        }
    }
}