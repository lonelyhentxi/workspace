#ifndef TINY_DB_ENGINE_STATEMENT_HPP
#define TINY_DB_ENGINE_STATEMENT_HPP

#include <memory>
#include "QtCore/qtextstream.h"
#include "QtCore/qstring.h"
#include "factory.hpp"
#include "repl.hpp"

namespace tinydb::frontend {
    using std::unique_ptr;

    struct statement : util::sorter<statement, QString> {
    protected:
        QString content_;
        const static QString unrecognized_warning_prefix;

        inline void warn_unrecognized(QTextStream &os) {
            os << unrecognized_warning_prefix << " '" << content_ << "'" << endl << flush;
        }
    public:
        explicit statement(QString content) : content_(std::move(content)) {}

        explicit statement(key) {}

        virtual ~statement() = default;

        inline const QString &content() const {
            return content_;
        }

        static bool match(const QString &content) {
            return !content.startsWith('.');
        }

        virtual inline void prepare(repl_framework &repl) {
            warn_unrecognized(repl.out);
        }
    };

    class select_statement : public statement::registrar<select_statement> {
    private:
        const static QString select_prefix;

    public:
        explicit select_statement(QString content) {
            content_ = std::move(content);
        }

        ~select_statement() override = default;

        static bool match(const QString &content) {
            return content.startsWith(select_prefix);
        }

        inline void prepare(repl_framework &) override {
        }
    };

    class insert_statement : public statement::registrar<insert_statement> {
    private:
        const static QString insert_prefix;
    public:
        explicit insert_statement(QString content) {
            content_ = std::move(content);
        }

        ~insert_statement() override = default;

        static bool match(const QString &content) {
            return content.startsWith(insert_prefix);
        }

        inline void prepare(repl_framework &) override {
        }
    };

    class empty_statement : public statement::registrar<empty_statement> {

    public:
        explicit empty_statement(QString content) {
            content_ = std::move(content);
        }

        ~empty_statement() override = default;

        static bool match(const QString &content) {
            return content.isEmpty();
        }

        inline void prepare(repl_framework &) override {
        }
    };
}

#endif
