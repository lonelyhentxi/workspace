#pragma once

#include <functional>
#include <atomic>
#include <future>
#include <memory>

#include "./join_threads.hpp"
#include "../container/thread_safe_queue.hpp"
#include "function_wrapper.hpp"
#include <queue>
#include <utility>
#include "../container/work_stealing_queue.hpp"

namespace eevent
{
    using std::atomic_bool;
    namespace this_thread = std::this_thread;
    using std::result_of;
    using std::future;
    using std::packaged_task;
    using std::function;
    using std::make_unique;
    using std::unique_ptr;

    using eevent::join_threads;
    using eevent::thread_safe_queue;
    using eevent::function_wrapper;
    using eevent::work_stealing_queue;

    using std::move;
    using std::queue;

    class thread_pool
    {
        using task_type = function_wrapper;
        using local_queue_type = queue<function_wrapper>;

        atomic_bool done;
        thread_safe_queue<task_type> pool_work_queue;
        vector<unique_ptr<work_stealing_queue>> queues;
        vector<thread> threads;
        join_threads joiner;

        static thread_local unique_ptr<work_stealing_queue> local_work_queue;

        static thread_local unsigned long my_index;

        void worker_thread(unsigned my_index_)
        {
            my_index= my_index_;
            local_work_queue = queues[my_index].get();
            while(!done)
            {
                run_pending_task();
            }
        }

        bool pop_task_from_local_queue(task_type& task)
        {
            return local_work_queue && local_work_queue->try_pop(task);
        }

        bool pop_task_from_pool_queue(task_type& task)
        {
            return pool_work_queue.try_pop(task);
        }

        bool pop_task_from_other_thread_queue(task_type& task)
        {
            for(unsigned i=0;i<queues.size();++i)
            {
                const auto index = static_cast<unsigned long>((my_index+i+1)%queues.size());
                if(queues[index]->try_steal(task))
                {
                    return true;
                }
            }

            return false;
        }

    public:
        thread_pool():
                joiner(threads),done(false)
        {
            unsigned const thread_count=std::thread::hardware_concurrency();

            try
            {
                for(unsigned i=0;i<thread_count;++i)
                {
                    queues.push_back(make_unique<work_stealing_queue>());
                    threads.push_back(
                            std::thread(&thread_pool::worker_thread,this,i));
                }
            }
            catch(...)
            {
                done=true;
                throw;
            }
        }

        ~thread_pool()
        {
            done=true;
        }

        template<typename FunctionType>
        future<std::result_of<FunctionType()>::type> submit(
                FunctionType f)
        {
            using result_type = std::result_of<FunctionType()>::type;

            std::packaged_task<result_type()> task(f);
            future<result_type> res(task.get_future());
            if(local_work_queue)
            {
                local_work_queue->push(task_type{move(task)});
            }
            else
            {
                pool_work_queue.push(task_type{move(task)});
            }
            return res;
        }

        void run_pending_task()
        {
            task_type task(function<void()>{});
            if(pop_task_from_local_queue(task) ||
               pop_task_from_pool_queue(task) ||
               pop_task_from_other_thread_queue(task))
            {
                task.call();
            }
            else
            {
                this_thread::yield();
            }
        }
    };
}