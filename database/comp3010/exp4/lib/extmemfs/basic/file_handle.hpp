#ifndef TINY_DB_ENGINE_FILESYSTEM_FILE_HANDLE_HPP
#define TINY_DB_ENGINE_FILESYSTEM_FILE_HANDLE_HPP

#include <memory>
#include <cstdint>
#include <valarray>
#include "vfs.hpp"
#include "fs_error.hpp"

namespace tinydb::filesystem
{
	using std::valarray;
	using std::slice;
	using std::shared_ptr;
	using std::monostate;

	struct open_options
	{
		bool read;
		bool write;
		bool append;
	};

	enum struct seek_type
	{
		start,
		end,
		current,
	};

	struct seek_from
	{
		seek_type type;
		int64_t value;
	};

	struct file_handle
	{
	private:
		shared_ptr<inode> node_;
		size_t offset_;
		open_options options_;
	public:
		file_handle(shared_ptr<inode> node, open_options options) : node_{std::move(node)}, offset_{0},
		                                                            options_{options}
		{
		};

		inline fs_result<size_t> read_at(const size_t offset, valarray<byte>& buf, const slice& s)
		{
			if (!options_.read)
			{
				ERR(fs_error::invalid_param);
			}
			return node_->read_at(offset, buf, s);
		}

		inline fs_result<size_t> read(valarray<byte>& buf, const slice& s)
		{
			UNWRAP(read_at(offset_, buf, s),len,const);
			offset_ += len;
			OK(len);
		}

		inline fs_result<size_t> write_at(const size_t offset, const valarray<byte>& buf, const slice& s)
		{
			if (!options_.write)
			{
				ERR(fs_error::invalid_param);
			}
			return node_->write_at(offset, buf, s);
		}

		fs_result<size_t> file_handle::write(const valarray<byte>& buf, const slice& s);

		fs_result<size_t> seek_from(const struct seek_from& pos);

		inline fs_result<monostate> resize(const size_t len)
		{
			if (!options_.write)
			{
				ERR(fs_error::invalid_param);
			}
			return node_->resize(len);
		}

		inline fs_result<monostate> sync_all()
		{
			return node_->sync_all();
		}

		inline fs_result<monostate> sync_data()
		{
			return node_->sync_data();
		}

		inline fs_result<metadata> metadata()
		{
			return node_->metadata();
		}

		inline fs_result<shared_ptr<inode>> lookup_follow(path pth, size_t max_follow)
		{
			return node_->lookup_follow(std::move(pth), std::move(max_follow));
		}

		inline fs_result<path> entry()
		{
			if (!options_.read)
			{
				ERR(fs_error::invalid_param);
			}
			UNWRAP(node_->entry(offset_), name, const);
			offset_ += 1;
			OK(name);
		}

		inline fs_result<poll_status> poll()
		{
			return node_->poll();
		}

		inline fs_result<monostate> io_control(const uint32_t cmd, const size_t arg)
		{
			return node_->io_control(cmd, arg);
		}
	};
}

#endif
