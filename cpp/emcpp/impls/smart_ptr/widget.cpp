#include "widget.h"
#include <string>
#include <vector>

namespace sptr_pimpl {
    struct Widget::Impl {
        std::string name;
        std::vector<double> data;
    };

    Widget::Widget():impl_{std::make_unique<Impl>()} {}
    Widget::~Widget() = default;
    Widget::Widget(Widget&&rhs) noexcept = default;
    Widget& Widget::operator=(Widget&&rhs) noexcept = default;
    Widget::Widget(const Widget& rhs):impl_{std::make_unique<Impl>(*rhs.impl_)}{}
    Widget& Widget::operator=(const sptr_pimpl::Widget &rhs) {
        *impl_ = *rhs.impl_;
        return *this;
    }
}

