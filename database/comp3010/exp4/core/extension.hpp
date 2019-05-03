#ifndef TINY_DB_ENGINE_EXTENSION_HPP
#define TINY_DB_ENGINE_EXTENSION_HPP

#include "core.hpp"
#include "functools.hpp"

namespace tinydb::extmem
{
	using namespace tinydb::core;

	using r_table = typename simple_table<fixed_record<tuple<int32_t, int32_t>>>;
	using s_table = typename simple_table<fixed_record<tuple<int32_t, int32_t>>>;
	using r_record = typename fixed_record<tuple<int32_t, int32_t>>;
	using s_record = typename fixed_record<tuple<int32_t, int32_t>>;
}

#endif
