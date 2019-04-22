#include "command.hpp"

namespace tinydb::frontend
{
	const QString command::exit_command = u8".exit";
	const QString command::unrecognized_warning_prefix = u8"Unrecognized command";

	optional<command> command::build_command(QString content)
	{
		if(!is_command(content))
		{
			return {};
		}
		return std::make_optional<command>(command(std::move(content)));
	}
}