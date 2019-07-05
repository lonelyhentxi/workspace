#pragma once
#include "i_runtime_module.hpp"

namespace enfw
{
    namespace ge_demo
    {
        class graphics_manager: implements i_runtime_module
        {
        public:
            ~graphics_manager() override = default;
        };
    }
}
