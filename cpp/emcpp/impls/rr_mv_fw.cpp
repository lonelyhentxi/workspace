#include <memory>
#include <type_traits>
#include <set>
#include <string>
#include <iostream>
#include <chrono>
#include <iomanip>
#include <sstream>
#include <vector>

namespace move_forward {
    template<typename T>
    decltype(auto) move(T &&param) {
        using ReturnType = std::remove_reference_t<T> &&;
        return static_cast<ReturnType>(param);
    }
}

namespace alter_forward {
    std::multiset<std::string> names{};

    template<typename T>
    void log(const std::chrono::time_point<T> &time, const std::string &name) {
        auto t = std::chrono::system_clock::to_time_t(time);
        std::cout << "[" << std::put_time(std::localtime(&t), "%Y-%m-%d %H.%M.%S") << "]: " << "log name <" << name
                  << ">" << std::endl;
    }

    template<typename T>
    void logAndAddImpl(T &&name, std::false_type) {
        auto now = std::chrono::system_clock::now();
        log(now, "logAndAdd");
        names.emplace(std::forward<T>(name));
    }

    void logAndAddImpl(int idx, std::true_type);

    template<typename T>
    void logAndAdd(T &&name) {
        logAndAddImpl(std::forward<T>(name), std::is_integral<std::remove_reference_t<T>>());
    }

    std::string nameFromIdx(int idx) {
        std::stringstream a;
        a << "number name " << idx;
        return a.str();
    }

    void logAndAddImpl(int idx, std::true_type) {
        logAndAdd(nameFromIdx(idx));
    }

    // SFINAE
    class Person {
    public:
        std::string name;

        template<typename T, typename=std::enable_if_t<
                !std::is_base_of<Person, std::decay_t<T>>::value
                &&
                !std::is_integral<std::remove_reference_t<T>>::value
        >
        >
        explicit Person(T &&n):name(std::forward<T>(n)) {
            static_assert(
                    std::is_constructible<std::string, T>::value,
                    "Parameter n can't be used to construct a std::string");
        }

        explicit Person(int idx) : name(nameFromIdx(idx)) {}

        Person(const Person &rhs) = default;

        Person(Person &&rhs) noexcept = default;

        Person &operator=(const Person &rhs) = default;

        Person &operator=(Person &&rhs) noexcept = default;

        ~Person() noexcept = default;

    };
}

namespace reference_folding {
    template <typename T>
    T&& forward(std::remove_reference_t<T>& param) {
        return static_cast<T&&>(param);
    }
}

namespace fail_forward {
    auto f = [](const std::vector<int>& v) {};

    template <typename T>
    void fwd(T&& param) {
        f(param);
    }
}

int main() {
    /**
     * 理解 std::move 和 std::forward
     */
    /**
     * 区分万能引用和右值引用
     */
    {
        // && 可能是右值引用，也可能是万能引用，后者出现在有类型推导的地方
        // 形式必须是 T&&
        // 和类类型关联同样不是万能引用，必须自身涉及类型推导(如方法模板)
        auto timeFuncInvocation =
                [](auto &&func, auto &&params) {
                    std::forward<decltype(func)>(func)(
                            std::forward<decltype(params)>(params)
                    );
                };
    }
    /**
     * 针对右值引用实施 std::move 针对万能引用实施 std::forward
     */
    // 局部对象的返回有返回值优化，即使没有，它也本来就是个右值，如果有移动构造器，本来就有优化
    /**
     * 避免依万能引用型别进行重载
     */
    // 因为精确匹配优先于提升类型后匹配，所以有可能引发匹配错误
    /**
     * 熟悉依万能引用型别进行重载的替代方案
     */
    // 传值是轻松的方案，233
    {
        // 非构造函数的可以使用这种方案
        using namespace alter_forward;
        logAndAdd("somename");
        logAndAdd(1);
        for (const auto &name:names) {
            std::cout << name << std::endl;
        }
        Person p1{"foo"};
        Person p2{1};
        auto p{p2};
        using std::cout;
        using std::endl;
        cout << p1.name << " " << p2.name << " " << p.name << endl;
        //
    }
    /**
     * 理解引用折叠
     */
    {
        // 在引用折叠中，只要有一个左值引用就是左值引用
    }
    /**
     * 假定移动操作不存在、成本高、未使用
     */
    // 在没有移动操作、移动操作不能更快、移动不可用的情况下，移动语意不会带来好处
    /**
     * 熟悉完美转发的失败情形
     */
    {
        using namespace fail_forward;
        // 大括号初始化物
        // 在直接调用的情况下，编译器先领受实参类型，再领受形参类型，比较或隐式转换
        // 在“完美转发”的情况下，推导得到形参，再转发给需要处比较：1. 无法为一个或多个形参推导出结果，2. 编译器为一个或多个推导出错误的结果
        f({1,2,3});
        auto a = {1,2,3};
        fwd(a);
        // 0 和 NULL 用作空指针，已陈述过
        // 仅有声明的整型static_const成员变量
        // 整型static_const成员变量的值会实施常数传播，但是在按值传递时，若是只声明会失败
        // 重载的函数和名字由于无法调用指针，无法转发
        // 位域由于无法调用指针，无法转发，可以使用转换成有指针 的 uint的方式转发
    }
}

