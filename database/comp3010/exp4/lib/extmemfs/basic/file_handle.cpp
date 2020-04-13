#include "file_handle.hpp"

namespace tinydb::filesystem
{
	fs_result<size_t> file_handle::seek_from(const struct seek_from& pos)
	{
		switch (pos.type)
		{
		case seek_type::start:
			offset_ = static_cast<size_t>(pos.value);
			break;
		case seek_type::current: // seek_type::current
			offset_ += pos.value;
			break;
		default:
			UNWRAP(node_->metadata(), l_metadata, const);
			offset_ = l_metadata.size + pos.value;
			break;
		}
		OK(offset_);
	}

	fs_result<size_t> file_handle::write(const valarray<byte>& buf, const slice& s)
	{
		size_t offset;
		if (options_.append)
		{
			UNWRAP(node_->metadata(), metadata, const);
			offset = metadata.size;
		}
		else
		{
			offset = offset_;
		}
		UNWRAP(write_at(offset, buf, s), len, const);
		offset_ = offset + len;
		OK(len);
	}
}