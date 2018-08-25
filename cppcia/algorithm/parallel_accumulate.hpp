#pragma once

#include <thread>
#include <utility>
#include <functional>
#include <future>
#include <iterator>
#include <algorithm>
#include <vector>
#include <utility>
#include <functional>
#include <numeric>

namespace eevent
{
    using std::accumulate;
    using std::distance;
    using std::vector;
    using std::advance;
    using std::min;
    using std::for_each;
    using std::ref;
    using std::thread;
    using std::mem_fn;
    using std::future;
    using std::packaged_task;
    using std::async;

    template<typename Iterator,typename T>
    struct accumulate_block
    {
        void operator()(Iterator first,Iterator last)
        {
            return accumulate(first,last,T());
        }
    };

    template<typename Iterator,typename T>
    T parallel_accumulate(Iterator first,Iterator last,T init)
    {
        unsigned long const length= static_cast<unsigned long>(distance(first,last));
        unsigned long const max_chunk_size=25;
        if(length<=max_chunk_size)
        {
            return accumulate(first,last,init);
        }
        else
        {
            Iterator mid_point=first;
            advance(mid_point,length/2);
            future<T> first_half_result=
                    async(parallel_accumulate<Iterator,T>,
                               first,mid_point,init);
            T second_half_result= parallel_accumulate(mid_point,last,T());
            return first_half_result.get()+second_half_result;
        }
    }
}
