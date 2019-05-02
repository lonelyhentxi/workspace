#ifndef TINY_DB_ENGINE_STATEMENT_HPP
#define TINY_DB_ENGINE_STATEMENT_HPP

#include <memory>
#include "QtCore/qtextstream.h"
#include "QtCore/qstring.h"
#include "factory.hpp"
#include "repl.hpp"

/**
 * do not directly use any constructor of statement
 */

namespace tinydb::frontend {

    struct statement: util::sorter<statement, QString> {
    protected:
        QString content_;
        const static QString unrecognized_warning_prefix;

        inline void warn_unrecognized(QTextStream &os) const {
            os << unrecognized_warning_prefix << " '" << content_ << "'" << endl << flush;
        }
    public:
		statement() = delete;
		statement(const statement&) = default;
		statement(statement&&) = default;
		statement& operator=(const statement&) = default;
		statement& operator=(statement&&) = default;
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

    class select_statement final: public statement::registrar<select_statement> {
    private:
        const static QString select_prefix;

    public:
		select_statement() = delete;
		select_statement(const select_statement&) = default;
		select_statement(select_statement&&) = default;
		select_statement& operator=(const select_statement&) = default;
		select_statement& operator=(select_statement&&) = default;

		explicit select_statement(QString content): registrar() {
            content_ = std::move(content);
        }

        ~select_statement() override = default;

        static bool match(const QString &content) {
            return content.startsWith(select_prefix);
        }

        inline void prepare(repl_framework &) override {
        }
    };

    class insert_statement final: public statement::registrar<insert_statement> {
    private:
        const static QString insert_prefix;
		
    public:
		insert_statement() = delete;
		insert_statement(const insert_statement&) = default;
		insert_statement(insert_statement&&) = default;
		insert_statement& operator=(const insert_statement&) = default;
		insert_statement& operator=(insert_statement&&) = default;

		explicit insert_statement(QString content): registrar() {
            content_ = std::move(content);
        }

        ~insert_statement() override = default;

        static bool match(const QString &content) {
            return content.startsWith(insert_prefix);
        }

        inline void prepare(repl_framework &) override {
        }
    };

    class empty_statement final: public statement::registrar<empty_statement> {

    public:
		empty_statement() = delete;
		empty_statement(const empty_statement&) = default;
		empty_statement(empty_statement&&) = default;
		empty_statement& operator=(const empty_statement&) = default;
		empty_statement& operator=(empty_statement&&) = default;

        explicit empty_statement(QString content) : registrar() {
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
