#pragma once

#include <deque>
#include <utility>
#include "../utility/function_wrapper.hpp"

namespace eevent
{
    using std::deque;
    using std::move;
    using std::mutex;
    using std::lock_guard;

    class work_stealing_queue
    {
    private:
        using data_type = function_wrapper;
        deque<data_type> the_queue;
        mutable mutex the_mutex;

    public:

        work_stealing_queue() = default;

        work_stealing_queue(const work_stealing_queue& other) = delete;
        work_stealing_queue& operator=(const work_stealing_queue& other) = delete;

        void push(data_type data)
        {
            lock_guard<mutex> lock(the_mutex);
            the_queue.push_front(move(data));
        }

        bool empty() const
        {
            lock_guard<mutex> lock(the_mutex);
            return the_queue.empty();
        }

        bool try_pop(data_type& res)
        {
            lock_guard<mutex> lock(the_mutex);
            if (the_queue.empty())
            {
                return false;
            }

            res = move(the_queue.front());
            the_queue.pop_front();
            return true;
        }

        bool try_steal(data_type& res)
        {
            lock_guard<mutex> lock(the_mutex);
            if (the_queue.empty())
            {
                return false;
            }

            res = move(the_queue.back());
            the_queue.pop_back();
            return true;
        }
    };
}