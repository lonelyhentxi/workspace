#ifndef TINY_DB_ENGINE_STATEMENT_HPP
#define TINY_DB_ENGINE_STATEMENT_HPP

#include "QtCore/qtextstream.h"
#include "QtCore/qstring.h"

namespace tinydb::frontend {
    enum class statement_type {
        insert,
        select,
		empty,
        unrecognized,
    };

    using statement_t = statement_type;

    class statement {
    private:
        statement_type type_;
        QString content_;
        const static QString insert_prefix;
        const static QString select_prefix;
        const static QString unrecognized_warning_prefix;
    public:
        inline statement_type type() const {
            return type_;
        }

        inline const QString &content() const {
            return content_;
        }

        explicit statement(QString content) {
            if (is_insert(content)) {
                type_ = statement_type::insert;
            } else if (is_select(content)) {
                type_ = statement_type::select;
			}
			else if (is_empty(content))
			{
				type_ = statement_type::empty;
			}
			else {
                type_ = statement_type::unrecognized;
            }
            content_ = std::move(content);
        }

        inline static bool is_insert(const QString &content) {
            return content.startsWith(insert_prefix);
        }

        inline static bool is_select(const QString &content) {
            return content.startsWith(select_prefix);
        }

		inline static bool is_empty(const QString &content)
        {
			return content.isEmpty();
        }

        inline void warn_unrecognized(QTextStream &os) const {
			os << unrecognized_warning_prefix << " '" << content() << "'."
				<< endl << flush;
        }
    };
}

#endif