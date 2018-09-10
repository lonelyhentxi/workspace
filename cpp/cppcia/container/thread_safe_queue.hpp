#pragma once

#include <queue>
#include <mutex>
#include <condition_variable>

namespace eevent {
    using std::queue;

    using std::mutex;
    using std::condition_variable;
    using std::lock_guard;
    using std::unique_lock;
    using std::shared_ptr;
    using std::make_shared;

    template<typename T>
    class thread_safe_queue
    {
    private:
        mutable mutex m;
        queue<T> data_queue;
        condition_variable data_cond;
    public:
        ~thread_safe_queue() = default;

        thread_safe_queue() = default;

        thread_safe_queue(const thread_safe_queue & other)
        {
            lock_guard<mutex> lk(other.m);
            data_queue = other.data_queue;
        }

        void push(T new_value)
        {
            lock_guard<mutex> lk{ m };
            data_queue.push(new_value);
            data_cond.notify_one();
        }

        void wait_and_pop(T &value)
        {
            unique_lock<mutex> lk(m);
            data_cond.wait(lk, [this] {return !data_queue.empty(); });
            value = data_queue.front();
            data_queue.pop();
        }

        shared_ptr<T> wait_and_pop()
        {
            unique_lock<mutex> lk(m);
            data_cond.wait(lk, [this] {return !data_queue.empty(); });
            shared_ptr<T> res(make_shared<T>(data_queue.front()));
            data_queue.pop();
            return res;
        }

        bool try_pop(T& value)
        {
            lock_guard<mutex> lk(m);
            if (data_queue.empty())
                return false;
            value = data_queue.front();
            data_queue.pop();
            return true;
        }

        shared_ptr<T> try_pop()
        {
            lock_guard<mutex> lk(m);
            if (data_queue.empty())
                return shared_ptr<T>();
            shared_ptr<T> res(make_shared<T>(data_queue.front()));
            data_queue.pop();
            return res;
        }

        bool empty() const {
            lock_guard<mutex> lk(m);
            return data_queue.empty();
        }
    };
}