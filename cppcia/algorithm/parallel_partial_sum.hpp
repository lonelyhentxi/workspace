#pragma once

#include <future>

#include <algorithm>
#include <vector>
#include <numeric>

#include "../utility/join_threads.hpp"
#include "../utility/barrier.hpp"

namespace eevent
{

    using std::promise;
    using std::future;
    using std::thread;
    using std::current_exception;
    using std::ref;

    using std::partial_sum;
    using std::for_each;
    using std::advance;
    using std::distance;
    using std::min;
    using std::vector;

    using eevent::barrier;
    using eevent::join_threads;


    template<typename Iterator>
    void parallel_partial_sum(Iterator first,Iterator last)
    {
        typedef typename Iterator::value_type value_type;

        struct process_element
        {
            void operator()(Iterator first,Iterator last,
                            vector<value_type>& buffer,
                            unsigned i,barrier& b)
            {
                value_type& ith_element=*(first+i);
                bool update_source=false;
                for(unsigned step=0,stride=1;stride<=i;++step,stride*=2)
                {
                    value_type const& source=(step%2)?
                                             buffer[i]:ith_element;
                    value_type& dest=(step%2)?
                                     ith_element:buffer[i];
                    value_type const& addend=(step%2)?
                                             buffer[i-stride]:*(first+i-stride);
                    dest=source+addend;
                    update_source=!(step%2);
                    b.wait();
                }
                if(update_source)
                {
                    ith_element=buffer[i];
                }
                b.done_waiting();
            }
        };

        unsigned long const length = static_cast<unsigned long>(distance(first,last));

        if(length<=1)
            return;

        vector<value_type> buffer(length);
        barrier b(length);
        vector<thread> threads(length-1);
        join_threads joiner(threads);

        Iterator block_start=first;
        for(unsigned long i=0;i<(length-1);++i)
        {
            threads[i]=thread(process_element(),first,last,
                                   ref(buffer),i,ref(b));
        }
        process_element{}.operator()(first,last,buffer,length-1,b);
    }
}