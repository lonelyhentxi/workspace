#include "command.hpp"

namespace tinydb::frontend
{
	const QString exit_command::exit_command_match = u8".exit";
	const QString command::unrecognized_warning_prefix = u8"Unrecognized command";
}