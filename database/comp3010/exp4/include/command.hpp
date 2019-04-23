#ifndef TINY_DB_ENGINE_COMMAND_HPP
#define TINY_DB_ENGINE_COMMAND_HPP

#include <optional>
#include "QtCore/qstring.h"
#include "QtCore/qtextstream.h"
#include "factory.hpp"
#include "repl.hpp"

namespace tinydb::frontend {
    using std::ostream;
    using std::optional;

    struct command : util::sorter<command, QString> {
    protected:
        QString content_;
        const static QString unrecognized_warning_prefix;
    private:
        inline void warn_unrecognized(QTextStream &os) {
            os << unrecognized_warning_prefix << " '" << content_ << "'" << endl << flush;
        }
    public:
        explicit command(QString content) : content_(std::move(content)) {}

        explicit command(key) {}

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

    class exit_command : public command::registrar<exit_command> {
    private:
        const static QString exit_command_match;
    public:
        explicit exit_command(QString content) {
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