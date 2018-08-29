#pragma once

#include <thread>
#include <future>

#include <iterator>
#include <algorithm>
#include <vector>
#include <utility>
#include <functional>

#include "../utility/join_threads.hpp"

namespace eevent
{
    using std::distance;
    using std::min;
    using std::vector;
    using std::advance;
    using std::for_each;

    using eevent::join_threads;

    using std::thread;
    using std::future;
    using std::packaged_task;

    template<typename Iterator,typename Func>
    void parallel_for_each(Iterator first,Iterator last,Func f)
    {
        unsigned long const length = static_cast<unsigned long>(distance(first,last));

        if(!length)
            return;

        unsigned long const min_per_thread=25;
        unsigned long const max_threads=
                (length+min_per_thread-1)/min_per_thread;

        unsigned long const hardware_threads=
                thread::hardware_concurrency();

        unsigned long const num_threads=
                min(hardware_threads!=0?hardware_threads:2,max_threads);

        unsigned long const block_size=length/num_threads;

        vector<std::future<void> > futures(num_threads-1);
        vector<thread> threads(num_threads-1);
        join_threads joiner(threads);

        Iterator block_start=first;
        for(unsigned long i=0;i<(num_threads-1);++i)
        {
            Iterator block_end=block_start;
            advance(block_end,block_size);
            packaged_task<void(void)> task(
                    [=]()
                    {
                        for_each(block_start,block_end,f);
                    });
            futures[i]=task.get_future();
            threads[i]=thread(move(task));
            block_start=block_end;
        }
        for_each(block_start,last,f);
        for(unsigned long i=0;i<(num_threads-1);++i)
        {
            futures[i].get();
        }
    }
}