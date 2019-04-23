#include "command.hpp"

namespace tinydb::frontend
{
	utf8_string command::exit_command_ = u8".exit";
	utf8_string command::unrecognized_warning_prefix_ = u8"Unrecognized command";

	optional<command> command::build_command(utf8_string content)
	{
		if(!is_command(content))
		{
			return {};
		}
		return std::make_optional<command>(command(std::move(content)));
	}
}