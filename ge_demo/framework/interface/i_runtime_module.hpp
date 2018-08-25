#pragma once

#include "interface.hpp"

namespace enfw 
{
    namespace ge_demo
    {
        interface i_runtime_module 
        {
        public:
            virtual ~i_runtime_module() = default;
            virtual int initialize() = 0;
            virtual void finalize() = 0;
            virtual void tick() = 0;
        };
    }
}