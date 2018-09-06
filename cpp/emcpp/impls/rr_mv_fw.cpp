#include <memory>
#include <type_traits>
#include <set>
#include <string>
#include <iostream>
#include <chrono>
#include <iomanip>
#include <sstream>

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

}

