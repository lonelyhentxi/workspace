#ifndef TINY_DB_ENGINE_COMMAND_HPP
#define TINY_DB_ENGINE_COMMAND_HPP

#include <optional>
#include "QtCore/qstring.h"
#include "QtCore/qtextstream.h"
#include "factory.hpp"
#include "repl.hpp"

/**
 * do not directly use any constructor of command
 */

namespace tinydb::frontend {
    using std::ostream;
    using std::optional;

    struct command : util::sorter<command, QString> {
    protected:
        QString content_;
        const static QString unrecognized_warning_prefix;
    private:
        inline void warn_unrecognized(QTextStream &os) const {
            os << unrecognized_warning_prefix << " '" << content_ << "'" << endl << flush;
        }

    public:

        explicit command(QString content) : content_(std::move(content)) {}

        explicit command(key) {}

		command() = delete;
		command(const command&) = default;
		command(command&&) = default;
		command& operator=(const command&) = default;
		command& operator=(command&&) = default;

        virtual ~command() = default;

        virtual inline void do_command(repl_framework &repl) {
            warn_unrecognized(repl.out);
        }

        inline const QString &content() const {
            return content_;
        }

        static bool match(const QString &content) {
            return content.startsWith('.');
        }
    };

    class exit_command final: public command::registrar<exit_command> {
    private:
        const static QString exit_command_match;
    public:
		exit_command() = delete;
		exit_command(const exit_command&) = default;
		exit_command(exit_command&&) = default;
		exit_command& operator=(const exit_command&) = default;
		exit_command& operator=(exit_command&&) = default;

        explicit exit_command(QString content): registrar() {
            content_ = std::move(content);
        }

        ~exit_command() override = default;

        static bool match(const QString &content) {
            return content.startsWith(exit_command_match);
        }

        inline void do_command(repl_framework &) override {
            exit(EXIT_SUCCESS);
        }
    };
}

#endif