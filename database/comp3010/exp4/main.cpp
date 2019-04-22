#include "QtCore/qstring.h"
#include "QtCore/qtextstream.h"
#include "repl.hpp"
#include "command.hpp"
#include "statement.hpp"

using namespace tinydb;
using frontend::command;
using frontend::command_type;
using frontend::statement_type;
using frontend::statement;

int main(int argc, char* argv[]) {
	
	QTextStream in{stdin,QIODevice::ReadOnly};
	QTextStream out{stdout,QIODevice::WriteOnly};
	QString current_line{};
	frontend::repl_framework repl{};
	while(true)
	{
		repl.log_prompt(out);
		current_line = repl.input_preprocess(in.readLine());
		auto c = command::build_command(current_line);
		if(c.has_value()) {
			switch(c->type())
			{
				case command_type::exit:
					exit(EXIT_SUCCESS);
					break;
				default: // command_type::unrecognized
					c->warn_unrecognized(out);
					break;
			}
		} else
		{
			auto s = statement{ current_line };
			switch (s.type())
			{
			case statement_type::insert:
			case statement_type::select:
			case statement_type::empty:
				break;
			default: // statement_type::unrecognized
				s.warn_unrecognized(out);
				break;
			}
			
		}
	}
}