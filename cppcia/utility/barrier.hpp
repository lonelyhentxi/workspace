#pragma once

#include <atomic>
#include <thread>

namespace eevent
{
    namespace this_thread = std::this_thread;

    class barrier
    {
    public:
        std::atomic<unsigned> count;
        std::atomic<unsigned> spaces;
        std::atomic<unsigned> generation;

        explicit barrier(unsigned count_):
                count(count_),spaces(count_),generation(0)
        {}
        void wait()
        {
            unsigned const gen=generation.load();
            if(!--spaces)
            {
                spaces=count.load();
                ++generation;
            }
            else
            {
                while(generation.load()==gen)
                {
                    this_thread::yield();
                }
            }
        }

        void done_waiting()
        {
            --count;
            if(!--spaces)
            {
                spaces=count.load();
                ++generation;
            }
        }
    };
}