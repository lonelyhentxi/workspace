#ifndef TINY_DB_ENGINE_REPL_HPP
#define TINY_DB_ENGINE_REPL_HPP
#include "shim.hpp"
#include <iostream>

namespace tinydb::repl {
	using std::ostream<db_char_t>;
	constexpr db_string_view_t prompt = "tinydb > ";
	inline ostream &write_prompt(ostream& os)
	{
		os << prompt;
		return os;
	}
}

#endif //TINY_DB_ENGINE_REPL_HPP
