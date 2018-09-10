#pragma once

#include <atomic>
#include <condition_variable>
#include <thread>
#include <future>
#include <exception>

namespace eevent
{

    using std::atomic;
    using std::condition_variable;
    using std::mutex;
    using std::condition_variable_any;
    using std::lock_guard;
    using std::thread;
    using std::promise;
    using std::exception;
    using std::memory_order_relaxed;

    struct thread_interrupted: public exception
    {
        const char *what() const noexcept override {
            return "thread_interrupt";
        }
    };

    void interruption_point();

    class interrupt_flag
    {
        friend struct custom_lock;
    private:
        std::atomic<bool> flag;
        condition_variable* thread_cond;
        mutex set_clear_mutex;
        condition_variable_any * thread_cond_any;

    public:

        interrupt_flag():
                thread_cond(0),thread_cond_any(0)
        {}

        ~interrupt_flag() = default;

        void set()
        {
            flag.store(true,memory_order_relaxed);
            lock_guard<mutex> lk(set_clear_mutex);
            if(thread_cond)
            {
                thread_cond->notify_all();
            }
            else if(thread_cond_any)
            {
                thread_cond_any->notify_all();
            }
        }

        bool is_set() const
        {
            return flag.load(memory_order_relaxed);
        }
        void set_condition_variable(condition_variable& cv)
        {
            lock_guard<mutex> lk(set_clear_mutex);
            thread_cond=&cv;
        }

        void clear_condition_variable()
        {
            lock_guard<mutex> lk(set_clear_mutex);
            thread_cond = 0;
        }

        template<typename Lockable>
        struct custom_lock
        {
            interrupt_flag* self;
            Lockable& lk;
            custom_lock(interrupt_flag* self_,
                        std::condition_variable_any& cond,
                        Lockable& lk_):
                    self(self_),lk(lk_)
            {
                self->set_clear_mutex.lock();
                self->thread_cond_any=&cond;
            }
            void unlock() // 3
            {
                lk.unlock();
                self->set_clear_mutex.unlock();
            }
            void lock()
            {
                std::lock(self->set_clear_mutex,lk);
            }
            ~custom_lock()
            {
                self->thread_cond_any=0;
                self->set_clear_mutex.unlock();
            }
        };

        template<typename Lockable>
        void wait(std::condition_variable_any& cv,Lockable& lk)
        {
            custom_lock<Lockable> cl(this,cv,lk);
            interruption_point();
            cv.wait(cl);
            interruption_point();
        }
    };

    thread_local interrupt_flag this_thread_interrupt_flag;

    void interruption_point()
    {
        if(this_thread_interrupt_flag.is_set())
        {
            throw thread_interrupted();
        }
    }

    template<typename Lockable>
    void interruptible_wait(condition_variable_any& cv,
                            Lockable& lk)
    {
        this_thread_interrupt_flag.wait(cv,lk);
    }

    class interruptible_thread
    {
        thread internal_thread;
        interrupt_flag* flag;
    public:
        template<typename FunctionType>
        explicit interruptible_thread(FunctionType f)
        {
            promise<interrupt_flag*> p;
            internal_thread=thread([f,&p]{
                p.set_value(&this_thread_interrupt_flag);
                f();
            });
            flag=p.get_future().get();
        }
        void interrupt()
        {
            if(flag)
            {
                flag->set();
            }
        }
    };
}