#pragma once

#include <memory>
#include <stack>

#include <exception>
#include <mutex>

namespace eevent
{
    using std::mutex;
    using std::stack;
    using std::move;

    using sexception = std::exception;
    using std::lock_guard;
    using std::shared_ptr;
    using std::make_shared;

    struct empty_stack: sexception
    {
        const char *what() const noexcept override  {
            return "empty stack";
        }
    };
    
    template<typename T>
    class thread_safe_stack
    {
    private:
        stack<T> data;
        mutable mutex m;
    public:
        thread_safe_stack(){}
        thread_safe_stack(const thread_safe_stack& other)
        {
            lock_guard<mutex> lock(other.m);
            data=other.data;
        }
        thread_safe_stack& operator=(const thread_safe_stack&) = delete;
        void push(T new_value)
        {
            lock_guard<mutex> lock(m);
            data.push(move(new_value));
        }
        shared_ptr<T> pop()
        {
            lock_guard<mutex> lock(m);
            if(data.empty()) throw empty_stack();
            shared_ptr<T> const res(
                    make_shared<T>(move(data.top())));
            data.pop();
            return res;
        }
        void pop(T& value)
        {
            lock_guard<mutex> lock(m);
            if(data.empty()) throw empty_stack();
            value=move(data.top());
            data.pop();
        }
        bool empty() const
        {
            lock_guard<mutex> lock(m);
            return data.empty();
        }
    };
}
