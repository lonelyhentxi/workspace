#ifndef TINY_DB_ENGINE_COMMAND_HPP
#define TINY_DB_ENGINE_COMMAND_HPP
#include <optional>
#include <iostream>
#include "tinyutf8.h"

namespace tinydb::frontend
{
	using std::ostream;
	using std::optional;

	enum class command_type
	{
		unrecognized,
		exit,
	};
	using command_t = command_type;

	class command
	{
	private:
		static utf8_string exit_command_;
		static utf8_string unrecognized_warning_prefix_;
		utf8_string content_;
		command_type type_;
		command() = delete;
		explicit command(utf8_string content)
		{
			if(command::if_exit(content))
			{
				type_ = command_type::exit;
			}
			else
			{
				type_ = command_type::unrecognized;
			}
			content_ = std::move(content);
		}
	public:
		inline command_type type() const
		{
			return type_;
		}

		inline const utf8_string& content() const
		{
			return content_;
		}

		inline bool if_exit(const utf8_string& command) const
		{
			return command == exit_command_;
		}

		static std::optional<command> build_command(utf8_string content);

		inline void warn_unrecognized(ostream& os) const
		{
			os << unrecognized_warning_prefix_ << " '" << content() << "'." << std::endl;
		}

		inline bool static is_command(const utf8_string& content)
		{
			return !content.empty() && content.front() == utf8_string::value_type{ '.' };
		}
	};
}

#endif