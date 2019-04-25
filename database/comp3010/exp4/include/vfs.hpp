#ifndef TINY_DB_ENGINE_VFS_HPP
#define TINY_DB_ENGINE_VFS_HPP

#include <cstdint>
#include <bitset>

namespace tinydb::vfs {

	struct block_storage_interface
	{
		virtual size_t block_content_size() const = 0;
		virtual size_t block_header_size() const = 0;
		virtual size_t block_size() const = 0;
		virtual ~block_storage_interface() = default;
	};
}

#endif