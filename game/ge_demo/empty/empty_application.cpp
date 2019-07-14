#include <memory>
#include "base_application.hpp"

namespace enfw
{
    namespace ge_demo
    {
        std::unique_ptr<i_application> pg_app = std::make_unique<base_application>();
    }
}