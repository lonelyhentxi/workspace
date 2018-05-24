#pragma once

#include <mutex>

namespace eevent
{
    template <typename SomeBigClass>
    void swap(SomeBigClass &lhs,SomeBigClass &rhs)
    {
        using std::lock;
        using std::mutex;
        using std::lock_guard;
        using std::adopt_lock;

        if(&lhs==&rhs) return;
        lock(lhs.m,rhs.m);
        lock_guard<mutex> lock_lhs(lhs.m,adopt_lock);
        lock_guard<mutex> lock_rhs(rhs.m,adopt_lock);
        swap(lhs.some_big_object,rhs.some_big_object)
    }
}
