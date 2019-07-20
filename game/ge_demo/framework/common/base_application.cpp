#include "base_application.hpp"

int enfw::ge_demo::base_application::initialize()
{
    is_quit_ = false;
    return 0;
}

void enfw::ge_demo::base_application::finalize()
{}

void enfw::ge_demo::base_application::tick()
{}

bool enfw::ge_demo::base_application::is_quit()
{
    return is_quit_;
}