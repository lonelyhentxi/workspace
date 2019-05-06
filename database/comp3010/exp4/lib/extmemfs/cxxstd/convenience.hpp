#ifndef TINY_DB_ENGINE_FILESYSTEM_CONVENIENCE_HPP
#define TINY_DB_ENGINE_FILESYSTEM_CONVENIENCE_HPP

#include <boost/config.hpp>
#include "operations.hpp"
#include "path.hpp"
#include <boost/system/error_code.hpp>
#include <boost/config/abi_prefix.hpp>

namespace tinydb::filesystem {

    inline std::string extension(const path &p) {
        return p.extension().string();
    }

    inline std::string basename(const path &p) {
        return p.stem().string();
    }

    inline path change_extension(const path &p, const path &new_extension) {
        path new_p(p);
        new_p.replace_extension(new_extension);
        return new_p;
    }
}

#include <boost/config/abi_suffix.hpp> 
#endif 
