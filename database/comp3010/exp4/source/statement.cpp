#include "statement.hpp"
#include "QtCore/qstring.h"

namespace tinydb::frontend {
    const QString statement::insert_prefix = u8"insert";
    const QString statement::select_prefix = u8"select";
    const QString statement::unrecognized_warning_prefix = u8"Unrecognized statement";
}