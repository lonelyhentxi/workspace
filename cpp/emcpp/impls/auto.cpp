#include <boost/type_index.hpp>
#include <iostream>

namespace auto_first {
    template <typename It>
    void dwim(It b,It e) {
        while(b!=e) {
            auto current = *b;
            std::cout << boost::typeindex::type_id_with_cvr<decltype(current)>() << std::endl;
            b = e;
        }
    }
}


std::vector<bool> features() {
    return std::vector<bool>(6);
}

int main() {
    int a =1;
    int b =2;
    auto_first::dwim(&a,&b);

    auto derefUPLess = [](const std::unique_ptr<int>& p1) {
        return *p1;
    };

    // 隐形代理类和 auto 无法和平共处
    // 对于隐形代理应该使用强制转型
    using std::vector;
    // 新版的c++已经消除了这个问题，在clion+clang+c++17 下已经可以正确类型推导
    auto highPriority = features()[5];
    auto betterPriority = static_cast<bool>(features()[5]);
}