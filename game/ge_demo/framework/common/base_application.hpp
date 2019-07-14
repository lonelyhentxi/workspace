#pragma once
#include "i_application.hpp"

namespace enfw
{
    namespace ge_demo
    {
        class base_application: implements i_application
        {
        public:
            int initialize() override;
            void finalize() override;
            // one cycle of the main loop
            void tick() override;
            bool is_quit() override;
        protected:
            // flag if need quit the main loop of the application
            bool is_quit_;
        };
    }
}