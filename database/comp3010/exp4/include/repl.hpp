#ifndef TINY_DB_ENGINE_REPL_HPP
#define TINY_DB_ENGINE_REPL_HPP
#include "QtCore/qstring.h"
#include "QtCore/qtextstream.h"

namespace tinydb::frontend {
	using std::ostream;


    class repl_framework {
    private:
         QString prompt_;
		
    public:
		repl_framework() : prompt_{ u8"tinydb> " } {}

		inline void log_prompt(QTextStream &os) const
		{
			os << prompt_ << flush;
		}

		inline QString input_preprocess(const QString &content) const
		{
			return content.trimmed();
		}
    };
}

#endif //TINY_DB_ENGINE_REPL_HPP
