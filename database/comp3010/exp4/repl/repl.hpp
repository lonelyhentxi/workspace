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
        QTextStream in;
        QTextStream out;

        repl_framework() : prompt_{u8"tinydb> "}, in{stdin, QIODevice::ReadOnly}, out{stdout, QIODevice::WriteOnly} {}

        inline void log_prompt() {
            out << prompt_ << flush;
        }

        inline QString input_preprocess(const QString &content) const {
            return content.trimmed();
        }

        inline QString read_line() {
            return input_preprocess(in.readLine());
        }
    };
}

#endif //TINY_DB_ENGINE_REPL_HPP
