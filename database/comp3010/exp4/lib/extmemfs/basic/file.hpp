#ifndef TINY_DB_ENGINE_FILESYSTEM_FILE_HPP
#define TINY_DB_ENGINE_FILESYSTEM_FILE_HPP

#include "util.hpp"
#include "vfs.hpp"
#include <cassert>

namespace tinydb::filesystem {
	using tinydb::filesystem::inode;
	using tinydb::filesystem::metadata;

	class file
	{
	private:
		shared_ptr<inode> inode_;
		size_t offset_;
		bool readable_;
		bool writable_;
	public:
		file(shared_ptr<inode> inode, const bool readable,const bool writable) : inode_{ std::move(inode) }, offset_{ 0 }, readable_{ readable }, writable_{writable} {}

		fs_result<size_t> read(valarray<byte>& buf, const slice& s)
		{
			assert(readable_);
			UNWRAP(inode_->read_at(offset_, buf, s), len, const);
			offset_ += len;
			OK(len);
		}

		fs_result<size_t> write(valarray<byte>& buf, const slice& s)
		{
			assert(writable_);
			UNWRAP(inode_->write_at(offset_, buf, s), len, const);
			offset_ += len;
			OK(len);
		}

		fs_result<metadata> info() {
			return inode_->metadata();
		}

		fs_result<path> entry(size_t id)
		{
			return inode_->entry(id);
		}
	};
}

#endif //TINY_DB_ENGINE_FILESYSTEM_FILE_HPP
