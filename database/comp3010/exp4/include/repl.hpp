#ifndef TINY_DB_ENGINE_REPL_HPP
#define TINY_DB_ENGINE_REPL_HPP
#include <iostream>
#include "tinyutf8.h"

namespace tinydb::frontend {
	using std::ostream;

    class repl_framework {
    private:
        utf8_string prompt_;
		
    public:
		repl_framework() : prompt_{ u8"tinydb> " } {}

		inline void log_prompt(ostream &os) const
		{
			os << prompt_;
		}
    };
}

#endif //TINY_DB_ENGINE_REPL_HPP
