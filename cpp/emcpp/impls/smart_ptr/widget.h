#ifndef EMCPP_WIDGET_H
#define EMCPP_WIDGET_H

#include <memory>

namespace sptr_pimpl {
    // 由于 shared_ptr 的中内容的类型不是一部分，使用它不需要如此操作
    class Widget {
    public:
        Widget();
        ~Widget(); // 此处必须声明以阻止编译器生成析构器，否则会因为生成析构器的定义导致析构时指涉的不是完整的类型，而抛出 static_assert

        Widget(Widget&&rhs) noexcept;
        Widget&operator=(Widget &&rhs) noexcept;
        // 移动函数和析构同理

        // 由于编译器不会为 unique_ptr 生成复制操作，复制需要特殊化自己编写
        Widget(const Widget & rhs);
        Widget&operator=(const Widget&rhs);
    private:
        struct Impl;
        std::unique_ptr<Impl> impl_;
    };
}

#endif //EMCPP_WIDGET_H
