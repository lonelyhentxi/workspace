#pragma once
#include "interface.hpp"
#include "i_runtime_module.hpp"

namespace enfw
{
    namespace ge_demo
    {
        interface i_application: implements i_runtime_module
        {
        public:
            int initialize() override = 0;
            void finalize() override = 0;
            // one cycle of the main loop
            void tick() override = 0;
            virtual bool is_quit() = 0;
        };
    }
}