#pragma once

#include <atomic>
#include <future>
#include <exception>

#include <functional>
#include <algorithm>
#include <iterator>
#include <vector>
#include "../utility/join_threads.hpp"

namespace eevent
{

    using std::promise;
    using std::atomic;
    using std::current_exception;
    using std::thread;

    using std::advance;
    using std::min;
    using std::distance;
    using std::vector;
    using eevent::join_threads;

    template<typename Iterator,typename MatchType>
    Iterator parallel_find(Iterator first,Iterator last,MatchType match)
    {
        struct find_element
        {
            void operator()(Iterator begin,Iterator end,
                            MatchType match,
                            promise<Iterator>* result,
                            std::atomic<bool>* done_flag)
            {
                try
                {
                    for(;(begin!=end) && !done_flag->load();++begin)
                    {
                        if(*begin==match)
                        {
                            result->set_value(begin);
                            done_flag->store(true);
                            return;
                        }
                    }
                }
                catch(...)
                {
                    try
                    {
                        result->set_exception(current_exception());
                        done_flag->store(true);
                    }
                    catch(...)
                    {}
                }
            }
        };

        unsigned long const length = static_cast<unsigned long>(distance(first,last));

        if(!length)
            return last;

        unsigned long const min_per_thread=25;
        unsigned long const max_threads=
                (length+min_per_thread-1)/min_per_thread;

        unsigned long const hardware_threads=
                thread::hardware_concurrency();

        unsigned long const num_threads=
                min(hardware_threads!=0?hardware_threads:2,max_threads);

        unsigned long const block_size=length/num_threads;

        promise<Iterator> result;
        std::atomic<bool> done_flag(false);
        vector<thread> threads(num_threads-1);
        {
            join_threads joiner(threads);

            Iterator block_start=first;
            for(unsigned long i=0;i<(num_threads-1);++i)
            {
                Iterator block_end=block_start;
                advance(block_end,block_size);
                threads[i]=thread(find_element(),
                                       block_start,block_end,match,
                                       &result,&done_flag);
                block_start=block_end;
            }
            find_element()(block_start,last,match,&result,&done_flag);
        }
        if(!done_flag.load())
        {
            return last;
        }
        return result.get_future().get();
    }
}
