#include <memory>
#include <iostream>

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

namespace sptr {
    class Widget : public std::enable_shared_from_this<Widget> {

    };
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
    }
}

