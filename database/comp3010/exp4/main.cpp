#include <iostream>
#include "tinyutf8.h"
#include "repl.hpp"
#include "command.hpp"

using namespace tinydb;

int main(int argc, char* argv[]) {
	using frontend::command;
	using frontend::command_type;

	utf8_string current_line{};
	frontend::repl_framework repl{};
	while(true)
	{
		repl.log_prompt(std::cout);
		std::cin >> current_line;
		auto c = command::build_command(current_line);
		if(c.has_value()) {
			switch(c->type())
			{
				case command_type::exit:
					exit(EXIT_SUCCESS);
					break;
				default: // command_type::unrecognized
					c->warn_unrecognized(std::cout);
					break;
			}
			continue;
		}
	}
	return 0;
}