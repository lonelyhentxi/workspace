#ifndef TINY_DB_ENGINE_FILESYSTEM_FILE_HPP
#define TINY_DB_ENGINE_FILESYSTEM_FILE_HPP

#include <optional>
#include <valarray>
#include <cstddef>
#include <cstdint>
#include "util.hpp"

namespace tinydb::filesystem
{
	using std::optional;
	using std::valarray;
	using std::byte;
	using std::slice;
	using std::slice_array;

	struct stream_device
	{
		virtual optional<size_t> read_at(const size_t offset, valarray<byte>& buf, const slice& s) = 0;
		virtual optional<size_t> write_at(const size_t offset, const valarray<byte>& buf, const slice& s) = 0;
		virtual ~stream_device() = default;
	};

	struct block_device
	{
		virtual size_t block_size() const = 0;
		virtual bool read_at_b(const size_t block_id, valarray<byte>& buf, const slice& s) = 0;
		virtual bool write_at_b(const size_t block_id, const valarray<byte>& buf, const slice& s) = 0;
		virtual ~block_device() = default;
	};

	struct block_range
	{
		size_t begin;
		size_t end;
		size_t block;
		size_t block_size;

		size_t len() const
		{
			return end - begin;
		}

		bool is_full() const
		{
			return len() == block_size;
		}

		size_t raw_begin() const
		{
			return block_size * block + begin;
		}

		size_t raw_end() const
		{
			return block_size * block + end;
		}

		struct iterator
		{
			size_t begin;
			size_t end;
			size_t block_size;
			using value_type = block_range;

			iterator() = delete;

			iterator(const size_t l_begin, const size_t l_end, const size_t l_block_size):
				begin(l_begin), end(l_end), block_size(l_block_size)
			{
			}

			iterator(const iterator&) = default;
			iterator(iterator&&) noexcept = default;
			iterator& operator=(const iterator&) = default;
			iterator& operator=(iterator&&) noexcept = default;
			~iterator() noexcept = default;

			optional<value_type> next()
			{
				if (begin >= end)
				{
					return {};
				}
				const auto l_block = begin / block_size;
				const auto l_begin = begin % block_size;
				size_t l_end;
				if (l_block == end / block_size)
				{
					l_end = end % block_size;
				}
				else
				{
					l_end = block_size;
				}
				begin += l_end - l_begin;
				return {
					block_range{
						l_begin,
						l_end,
						l_block,
						block_size,
					}
				};
			}
		};
	};

#define RECHECK(len,res) \
	if( ! res ) { \
   return {len};		\
	}

	struct block_stream_device : public block_device, public stream_device
	{
	public:
		optional<size_t> read_at(const size_t offset, valarray<byte>& buf, const slice& s) override
		{
			auto iter = block_range::iterator{offset, offset + s.size(), block_size()};
			while (true)
			{
				auto range = iter.next();
				if (!range.has_value())
				{
					return {s.size()};
				}
				const auto sub_slice = slice(range->raw_begin() - offset, range->raw_end() - range->raw_begin(), 1);
				const auto len = range->raw_begin() - offset;
				if (range->is_full())
				{
					RECHECK(len, read_at_b(range->block, buf, sub_slice));
				}
				else
				{
					auto block_buf = valarray<byte>(range->block_size);
					const auto block_buf_slice = slice(0, range->block_size, 1);
					RECHECK(len, read_at_b(range->block, block_buf, block_buf_slice));
					const auto new_block_buf_slice = slice(0, sub_slice.size(), 1);
					const auto& buf_proxy = buf[sub_slice];
					buf_proxy = block_buf[new_block_buf_slice];
				}
			}
		}

		optional<size_t> write_at(const size_t offset, const valarray<byte>& buf, const slice& s) override
		{
			auto iter = block_range::iterator{offset, offset + s.size(), block_size()};
			while (true)
			{
				auto range = iter.next();
				if (!range.has_value())
				{
					return {s.size()};
				}
				const auto sub_slice = slice{range->raw_begin() - offset, range->raw_end() - range->raw_begin(), 1};
				const auto len = range->raw_begin() - offset;
				if (range->is_full())
				{
					RECHECK(len, write_at_b(range->block, buf, sub_slice));
				}
				else
				{
					auto block_buf = std::valarray<byte>(range->block_size);
					const auto block_buf_slice = slice(0, range->block_size, 1);
					RECHECK(len, read_at_b(range->block, block_buf, block_buf_slice ));
					const auto new_block_buf_slice = slice(0, sub_slice.size(), 1);
					const auto &block_buf_proxy = block_buf[new_block_buf_slice];
					block_buf_proxy = buf[sub_slice];
					RECHECK(len, write_at_b(range->block, block_buf, block_buf_slice));
				}
			}
		}
		~block_stream_device() override = default;
	};
}

#endif //TINY_DB_ENGINE_FILESYSTEM_FILE_HPP
