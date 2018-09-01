#include <vector>
#include <iostream>
#include <mutex>
#include <list>

void f(int c) { std::cout << "a" << std::endl; };

void f(bool a) { std::cout << "b" << std::endl; };

void f(void *c) { std::cout << "c" << std::endl; };

template<typename FuncType, typename MutType, typename PtrType>
decltype(auto) lockAndCall(FuncType func, MutType &mutex, PtrType ptr) {
    std::lock_guard g{mutex};
    return func(ptr);
}

namespace using_r_typedef {
    template<typename T, typename MyAlloc>
    using MyAllocList = std::list<T, MyAlloc>;

    template<typename T, typename MyAlloc>
    struct MyAllocList1 {
        typedef std::list<T, MyAlloc> type;
    };

    template<typename T>
    using remove_const_t = typename std::remove_const<T>::type;
}

namespace enum_class_r_enum {
    template<typename E>
    constexpr typename std::underlying_type<E>::type toUType(E enumerator) noexcept {
        return static_cast<typename std::underlying_type<E>::type>(enumerator);
    }
}

namespace delete_r_private {
    bool isLucky(int number) {return true;}
    bool isLucky(double number) = delete; // 避免隐式转型
}


int main() {
    /**
     * 在创建对象时注意 () 和 {}
     */
    // 只有在找不到任何方法可以把大括号初始化实参转化成 initializer_list 时，编译器才会退而检查重载协议
    std::vector<int> a{5};
    std::vector<int> b(5);
    std::cout << "a size:" << a.size() << std::endl;
    std::cout << "b size:" << b.size() << std::endl;
    // 在空大括号的情况下会进行默认构造，需要在构造器内部使用另外一个大括号
    std::vector<int> c{{}}; // initialize init
    std::vector<int> d{}; // default cor
    /**
     * 优先选用nullptr,而非0或者NULL
     */
    f(0);
    // f(NULL); // 在一些编译器中会勉强解释为int,在现代编译器中普遍不能通过编译
    f(nullptr); // 正确的代码
    // 当使用 auto 时，nullptr 可以正确的推导(对于阅读的程序员，语义更加明确)
    auto result = nullptr;
    /**
     * 优先选用别名声明，而非 typedef
     */
    // 它对于函数指针更容易理解
    typedef void (*FP)(int, const std::string &);
    using FP1 = void (*)(int, const std::string &);
    // 别名声明可以模板化，而 typedef 不行
    // using 可以减少 typename 的运用
    // std::transformation_t<T> 是对应 std::transformation<T>::type 的对应形式
    /**
     * 优先使用限定作用域的枚举类型
     */
    enum class color {
        black, white, red
    };

    auto white = false;

    enum chair_man {
        mao, deng, jiang, hu, xi
    };

    // auto xi = false; // err

    // 可以使用 enum class Status: std::uint32_t; 来推翻默认类型的定义，使得编译器知晓类型的大小
    using UserInfo = std::tuple<std::string, std::string, std::size_t>;
    // 几乎只有在这这种情况下，不带作用域的枚举才能发挥作用
    enum UserInfoFields {
        uiName, uiEmail, uiReputation
    };
    UserInfo userInfo{};
    auto val = std::get<uiName>(userInfo);
    // 几乎等价的 enum class 为
    enum class UserInfoClassFields {
        uiName, uiEmail, uiReputation
    };
    auto val1 = std::get<static_cast<std::size_t>(UserInfoClassFields::uiReputation)>(userInfo);
    auto val2 = std::get<enum_class_r_enum::toUType(UserInfoClassFields::uiReputation)>(userInfo);

    /**
     * 优先使用删除函数，而非 private 未定函数
     */
    // 删除无法通过任何方法使用，而 private 函数还可以通过友元使用
    // 编译器会先检查访问性，再检查删除状态,将删除函数定义为public可以更好进行代码提示
    // 删除函数可以在任意情况下使用，而private声明只能在成员函数中使用
    /**
     * 为意在改写的函数添加 override声明
     */
     // 在 C++ 11 中基类和派生类的函数引用装饰词必须完全相同
     class Base {
     public:
         virtual void doWork() {};
         void doOther() & {}; // 在*this是左值的时候调用
         void doOther() && {}; // 在*this是右值的时候调用
     };

     class Derived: public Base {
     public:
         void doWork() override {}
     };

     // 通过引用装饰词优化传递
     class Widget {
     public:
         using DataType = std::vector<double>;
         DataType & data() & {
             return values;
         }
         DataType data() && {
             return std::move(values);
         }
     private:
         DataType values;
     };
}