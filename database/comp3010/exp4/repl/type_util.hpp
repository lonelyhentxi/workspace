#ifndef TINY_DB_ENGINE_TYPE_UTIL_HPP
#define TINY_DB_ENGINE_TYPE_UTIL_HPP

#include <string>
#include <boost/core/demangle.hpp>

namespace tinydb::util
{

    template <typename T>
    std::string type_name() {
        const char *name = typeid(T).name();
        return boost::core::demangle(name);
    }
}

#endif