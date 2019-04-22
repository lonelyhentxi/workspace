#ifndef TINY_DB_ENGINE_REPL_HPP
#define TINY_DB_ENGINE_REPL_HPP
#include "shim.hpp"
#include <iostream>

namespace tinydb::repl {
	using std::basic_ostream<char> ostream;
	constexpr db_string_view_t prompt = u8"tinydb > ";
	inline ostream &write_prompt(ostream& os)
	{
		os << prompt;
		return os;
	}
}

#endif //TINY_DB_ENGINE_REPL_HPP
