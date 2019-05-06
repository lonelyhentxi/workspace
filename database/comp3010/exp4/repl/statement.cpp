#include "statement.hpp"
#include "QtCore/qstring.h"

namespace tinydb::frontend {
    const QString insert_statement::insert_prefix = u8"insert";
    const QString select_statement::select_prefix = u8"select";
    const QString statement::unrecognized_warning_prefix = u8"Unrecognized statement";
}