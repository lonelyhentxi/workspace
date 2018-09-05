#include <memory>
#include <iostream>
#include <unordered_map>
#include "widget.h"

namespace uptr {
    class Investment {
    };

    class Stock : public Investment {
    };

    class Bond : public Investment {
    };

    class RealEstate : public Investment {
    };

    auto delInvmt = [](Investment *p) {
        std::cout << "deleting unique_ptr..." << std::endl;
        delete p;
    };

    template<typename ...Ts>
    std::unique_ptr<Investment, decltype(delInvmt)> makeInvestment(Ts &&...params) {
        std::unique_ptr<Investment, decltype(delInvmt)> pInv{nullptr, delInvmt};
        int a = std::rand() % 3 + 1;
        if (a == 1) {
            pInv.reset(new Stock(std::forward<Ts>(params)...));
        } else if (a == 2) {
            pInv.reset(new Bond(std::forward<Ts>(params)...));
        } else {
            pInv.reset(new RealEstate(std::forward<Ts>(params)...));
        }
        return pInv;
    }
}

namespace wp {
    class Widget {
    public:
        using WidgetId = int;
        explicit Widget(WidgetId i):id_{std::forward<decltype(i)>(i)} {}
    private:
        WidgetId id_;
    };

    std::shared_ptr<const Widget> fastLoadWidget(const Widget::WidgetId &id) {
        static std::unordered_map<Widget::WidgetId,std::weak_ptr<const Widget>> cache;
        auto objPtr = cache[id].lock();
        if(!objPtr) {
            objPtr = std::make_shared<Widget>(std::rand()%10000+1);
            cache[id] = objPtr;
        }
        // 一般要添加过期机制
        return objPtr;
    }
}

int main() {
    /**
     * 使用 unique_ptr 管理具有专属所有权的资源
     */
    // unique_ptr 几乎和裸指针具有相同的尺寸，并且对于绝大多数操作，它都精确的执行了操作
    // 默认情况下，使用 delete 实现，但是可以被设定为使用自定义析构器
    {
        using namespace uptr;
        auto rand_pinv = makeInvestment();
        // 将一个裸指针赋给unique_ptr的尝试不会通过编译，因为会形成裸指针到智能指针的隐式转换
        // 单个对象形式的up不提供索引运算符，数组形式的up不提供领运算符
        std::shared_ptr<Investment> sp = makeInvestment();
        // up可以快速的转换成为sp，便于工厂函数的操作
    }
    /**
     * 使用 shared_ptr 管理具备共享所有权的资源
     */
    // shared_ptr 由于有一个指向控制块(包括计数器)的指针，其占用的资源是裸指针的两倍
    // shared_ptr 引用的资源必须动态分配
    // 引用技术的递增和递减必须是原子操作
    // 移动操作因为不需要改变其计数，因此速度比复制更快
    // sp 的也可以添加析构器，但是它并不是类型的一部分
    {
        using namespace uptr;
        std::shared_ptr<Investment> spw{new Investment{}, delInvmt};
        // std::make_shared 总是会创建一个控制块
        // 从专属控制权的指针生成时会创建一个控制块
        // 使用裸指针创建时候会创建一个控制块
        // 没有 shared_ptr<T[]>
    }
    /**
     * 对于类似 std::shared_ptr 但有可能空悬的指针使用 weak_ptr
     */
    // weak_ptr 依赖 shared_ptr 但是不占用引用计数
    {
        auto spw = std::make_shared<uptr::Investment>();
        std::weak_ptr<uptr::Investment> wpw(spw);
        spw = nullptr;
        std::cout << "wpw has expired? - " << (wpw.expired() ? "true" : "false") << std::endl;
        std::shared_ptr<uptr::Investment> spw1 = wpw.lock(); // 若 wpw 失效，则spw1为空，且这是一个“原子操作”
        try {
            std::shared_ptr<uptr::Investment> spw2{wpw}; // 若wpw失效，则抛出异常
        } catch (std::exception &err) {
            std::cout << err.what() << std::endl;
        }
        // 有时还用在观察者模式中判断指向观察者的指针是否空悬
        // std::weak_ptr 有时还用来解决 std::shared_ptr 指涉有可能引起的环路问题
        // 如果子节点的生存期不比父节点更长，那么由于不会出现空悬的情况，用 unique_ptr 即可
    }
    /**
     * 优先使用 std::make_unique 和 std::make_shared 而不是直接使用 new
     */
    // 直接使用 new 运算符可能由于异常等原因导致内存泄漏
    // make_shared 让编译器有机会利用更简洁的数据结构产生更小和更快的代码（make_xxx内存分配一次，new 本身和附加数据块各一次）
    // make 系列工厂函数不允许使用自定义构造器
    // make 对形参进行完美转发使用的是圆括号，但是大括号初始化物并不能完美转发，变通的方法是先用auto对initializer_list 初始化再传入
    // 使用 make 系列对有自定义版本 new 和 delete 操作符使用并不是好主义
    /**
     * 使用 pimpl 习惯用法时，将特殊成员函数的定义放在实现文件中
     */
    {
        using namespace sptr_pimpl;
        Widget widget;
    }
}

