#ifndef TINY_DB_ENGINE_SHIM_HPP
#define TINY_DB_ENGINE_SHIM_HPP

#include <string>

#ifndef __cpp_char8_t
using db_char_t = unsigned char;
#else
using db_char_t = char8_t;
#endif
using db_string_t = std::basic_string<db_char_t>;
using db_string_view_t = std::basic_string_view<db_char_t>;

namespace tinydb {

}

#endif //TINY_DB_ENGINE_SHIM_HPP
