#ifndef TINY_DB_ENGINE_RECORD_HPP
#define TINY_DB_ENGINE_RECORD_HPP

#include <cstdint>

namespace tinydb::backend
{
	class record_storage_interface {
	public:
		virtual void update(uint32_t record_id) = 0;
		virtual void remove(uint32_t record_id) = 0;
		virtual void create() = 0;

	};
}

#endif