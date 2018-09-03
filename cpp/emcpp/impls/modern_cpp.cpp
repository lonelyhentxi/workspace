#include <vector>
#include <iostream>
#include <mutex>
#include <list>
#include <array>
#include <cmath>

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
    bool isLucky(int number) { return true; }

    bool isLucky(double number) = delete; // 避免隐式转型
}

namespace ci_r_i {
    // 第三方库的形式
    template<typename C, typename V>
    void findAndInsert(C &container, const V &targetVal, const V &insertV) {
        using std::cbegin;
        using std::cend;
        auto it = std::find(cbegin(container), cend(container), targetVal);
        container.insert(it, insertV);
    }
}

namespace noexcept_r {
    void f(int x) throw() {}; // 执行一系列动作后，调用栈开解
    // void f(int x) noexcept {}; 和 throw() 版本冲突
    void f1(int x) noexcept {}; // 调用栈可能会开解
    template<typename T, size_t N>
    void swap(T (&a)[N], T(&b)[N]) noexcept(noexcept(std::swap(*a, *b)));
}

namespace constexpr_r {
    constexpr int pow(int base, int exp) noexcept {
        return (exp == 0 ? 1 : base * pow(base, exp - 1));
    }

    constexpr int pow1(int base, int exp) noexcept {
        auto result = 1;
        for (int i = 0; i < exp; ++i) result *= base;
        return result;
    }

    class Point {
    private:
        double x, y;
    public:
        constexpr explicit Point(double xVal = 0, double yVal = 0) noexcept: x(xVal), y(yVal) {}

        constexpr double xValue() const noexcept {
            return x;
        }

        constexpr double yValue() const noexcept {
            return y;
        }

        // c++ 14 中 constexpr 限制不能 void 和 默认 const 的条件取消了
        constexpr void setX(double newX) noexcept {
            x = newX;
        }

        constexpr void setY(double newY) noexcept {
            y = newY;
        }
    };

    constexpr decltype(auto) midpoint(const Point &p1, const Point &p2) noexcept {
        return Point{
                (p1.xValue() + p2.xValue()) / 2, (p1.yValue() + p2.yValue()) / 2
        };
    }
}

namespace multithread_const {
    class Polynomial {
    public:
        using RootTypes = std::vector<double>;
        // 会使得之失去可复制性，但仍然保有可移动性
        // 引入互斥量不一定是必须的，很多操作只需要 std::atomic 即可
        RootTypes roots() const {
            std::lock_guard<std::mutex> g(m);
            if(!rootsAreValid) {
                rootsAreValid = true;
            }
            return rootVals;
        }
    private:
        mutable std::mutex m;
        mutable bool rootsAreValid{ false };
        mutable RootTypes rootVals{};
    };
    class Point {
    private:
        double x, y;
        mutable std::atomic<unsigned> callCount{0};
    public:
        constexpr explicit Point(double xVal = 0, double yVal = 0) noexcept: x(xVal), y(yVal) {}

        constexpr double xValue() const noexcept {
            return x;
        }

        constexpr double yValue() const noexcept {
            return y;
        }

        // c++ 14 中 constexpr 限制不能 void 和 默认 const 的条件取消了
        constexpr void setX(double newX) noexcept {
            x = newX;
        }

        constexpr void setY(double newY) noexcept {
            y = newY;
        }

        // atomic 型的开销往往较小
        double distanceFromOrigin() const noexcept {
            ++callCount;
            return std::sqrt((x*x)+(y*y));
        }
    };

    class Widget {
    private:
        mutable std::mutex m;
        mutable int cachedValue;
        mutable bool cacheValid{false};
    public:
        int magicValue() const {
            std::lock_guard<std::mutex> guard(m);
            if(cacheValid) return cachedValue;
            else {
                auto val1 = std::rand()%100+1;
                auto val2 = std::rand()%100+1;
                cachedValue = val1+val2;
                cacheValid = true;
                return cachedValue;
            }
        }
    };
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

        void doOther() &{}; // 在*this是左值的时候调用
        void doOther() &&{}; // 在*this是右值的时候调用
    };

    class Derived : public Base {
    public:
        void doWork() override {}
    };

    // 通过引用装饰词优化传递
    class Widget {
    public:
        using DataType = std::vector<double>;

        DataType &data() &{
            return values;
        }

        DataType data() &&{
            return std::move(values);
        }

    private:
        DataType values;
    };
    /**
     * 优先使用 const_iterator 而非 iterator
     */
    // 在 c++ 98 中，const_iterator 经常面临转型的问题
    // 用法
    std::vector<int> values{1983, 2000};
    auto it = std::find(values.cbegin(), values.cend(), 1983);
    values.insert(it, 1998);
    /**
     * 只要函数不会发射异常，就为其加上 noexcept 声明
     */
    // noexcept 声明是和 const 同等重要的信息，若是未为其加上的话，是接口规格缺陷
    // 在带有 noexcept 声明的函数中，优化器不需要在异常传出函数的前提下，将执行栈保持在可开解状态；
    // 也不需要在异常溢出函数的前提下，保证所有其中的对象以其被构造顺序的逆序完成析构
    // 移动相关函数建议不抛出异常，swap函数，delete相关函数、析构器不应该抛出异常
    /**
     * 只要有可能使用 constexpr 就使用它
     */
    constexpr auto arraySize = 10;
    std::array<int, arraySize> data{};
    {
        using namespace constexpr_r;
        constexpr Point p1(9.4, 27.7);
        constexpr Point p2(28.8, 5.3);
        std::cout << p1.xValue() << std::endl;
        std::cout << p2.xValue() << std::endl;
        constexpr auto mid = midpoint(p1, p2);
        std::cout << mid.xValue() << std::endl;
    }
    /**
     * 保证 const 成员函数的线程安全性
     */
    {
        // const 不能作多线程下的只读保证，因此还是可能出现数据竞险的情况
    }
    /**
     * 理解特种成员函数的生成机制
     */
    // 默认构造函数只会在没有任何构造函数的时候才会生成
    // 生成的特种成员都是public且inline的，它们都非虚，除了是一个析构函数
    // 移动相关特种成员函数都只是成员的移动请求
    // 两种复制之间相互独立，不会阻碍生成（历史遗留问题，废弃行为）；但是移动函数之间会阻碍
    // 显式声明了复制函数之后，会阻碍移动函数的生成
    // 显式声明了移动函数之后，会阻碍复制函数的生成
    // 声明了析构函数，复制函数仍然生成（历史遗留问题，废弃行为），但是移动函数会阻碍生成
    // 若编译器生成的代码具有正确的行为，使用 =default 来指明
    // 析构函数默认是 noexcept 的，只有当基类的析构函数是虚的时候，派生类默认的析构函数才是虚的
    // 成员函数模板的存在会阻止编译器生成任何特种成员函数
}