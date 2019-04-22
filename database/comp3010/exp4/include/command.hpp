#ifndef TINY_DB_ENGINE_COMMAND_HPP
#define TINY_DB_ENGINE_COMMAND_HPP

#include <optional>
#include "QtCore/qstring.h"
#include "QtCore/qtextstream.h"

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
		const static QString exit_command;
		const static QString unrecognized_warning_prefix;
		QString content_;
		command_type type_;
		explicit command(QString content)
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
		command() = delete;
		inline command_type type() const
		{
			return type_;
		}

		inline const QString& content() const
		{
			return content_;
		}

		inline static bool if_exit(const QString& command)
		{
			return command == exit_command;
		}

		static std::optional<command> build_command(QString content);

		inline void warn_unrecognized(QTextStream &os) const
		{
			os << unrecognized_warning_prefix << " '" << content() << "'." << endl << flush;
		}

		inline bool static is_command(const QString& content)
		{
			return content.startsWith('.');
		}
	};
}

#endif