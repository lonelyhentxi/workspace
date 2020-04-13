#include <typeinfo>
#include "QtCore/qstring.h"
#include "QtCore/qtextstream.h"
#include "boost/assert.hpp"
#include "repl.hpp"
#include "command.hpp"
#include "statement.hpp"

using namespace tinydb;
using frontend::command;
using frontend::statement;
using std::unique_ptr;
using std::optional;

int main(int argc, char *argv[]) {
    frontend::repl_framework repl{};
    while (true) {
        repl.log_prompt();
        QString current_line = repl.read_line();
        optional<unique_ptr<command>> c = command::create(current_line);
        if (c.has_value()) {
            (*c)->do_command(repl);
            // useless check because compiler poor static analysis
            if (typeid(decltype(c)::value_type::element_type) == typeid(frontend::exit_command)) {
                exit(EXIT_SUCCESS);
            }
        } else {
            optional<unique_ptr<statement>> optional_statement = statement::create(current_line);
            BOOST_ASSERT_MSG(optional_statement.has_value(), "statement must valid");
            (*optional_statement)->prepare(repl);
        }
    }
}