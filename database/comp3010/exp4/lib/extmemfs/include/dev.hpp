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
	using tinydb::filesystem::util::mem_page_size;

	class stream_device
	{
	public:
		virtual optional<size_t> read_at(size_t offset, valarray<byte>& buf, slice& s) = 0;
		virtual optional<size_t> write_at(size_t offset, const valarray<byte>& buf, const slice& s) = 0;
		virtual ~stream_device() = default;
	};

	class block_device
	{
	public:
		virtual uint8_t block_size_log2() = 0;
		virtual bool read_at_b(size_t block_id, valarray<byte>& buf, slice& s) = 0;
		virtual bool write_at_b(size_t block_id, const valarray<byte>& buf, const slice& s) = 0;
		virtual ~block_device() = default;
	};

	struct block_range
	{
		size_t begin;
		size_t end;
		size_t block;
		uint8_t block_size_log2;

		size_t len() const
		{
			return end - begin;
		}

		bool is_full() const
		{
			return len() == (1 << block_size_log2);
		}

		size_t raw_begin() const
		{
			return (block << block_size_log2) + begin;
		}

		size_t raw_end() const
		{
			return (block << block_size_log2) + end;
		}

		struct iterator
		{
			size_t begin;
			size_t end;
			uint8_t block_size_log2;
			using value_type = block_range;

			iterator() = delete;

			iterator(const size_t l_begin, const size_t l_end, const uint8_t l_block_size_log2):
				begin(l_begin), end(l_end), block_size_log2(l_block_size_log2)
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
				const auto l_block_size_log2 = block_size_log2;
				const size_t l_block_size = 1 << block_size_log2;
				const auto l_block = begin / l_block_size;
				const auto l_begin = begin % l_block_size;
				size_t l_end;
				if (l_block == end / l_block_size)
				{
					l_end = end % l_block_size;
				}
				else
				{
					l_end = l_block_size;
				}
				begin += l_end - begin;
				return {
					block_range{
						l_block,
						l_begin,
						l_end,
						l_block_size_log2,
					}
				};
			}
		};
	};

#define RECHECK(len,res) \
	if( ! res ) { \
   return {len};		\
	}

	class block_stream_device : block_device, stream_device
	{
	public:
		optional<size_t> read_at(size_t offset, valarray<byte>& buf, slice& s) override
		{
			auto iter = block_range::iterator{offset, offset + s.size(), block_size_log2()};
			while (true)
			{
				auto range = iter.next();
				if (!range.has_value())
				{
					return {s.size()};
				}
				auto sub_slice = slice{range->raw_begin() - offset, range->raw_end() - range->raw_begin(), 1};
				auto len = range->raw_begin() - offset;
				if (range->is_full())
				{
					RECHECK(len, read_at_b(range->block, buf, sub_slice));
				}
				else
				{
					auto block_buf = std::valarray<byte>{mem_page_size};
					auto block_buf_slice = slice{0, mem_page_size, 1};
					RECHECK(len, read_at_b(range->block, block_buf, block_buf_slice));
					buf[sub_slice] = block_buf[block_buf_slice];
				}
			}
		}

		optional<size_t> write_at(size_t offset, const valarray<byte>& buf, const slice& s) override
		{
			auto iter = block_range::iterator{offset, offset + s.size(), block_size_log2()};
			while (true)
			{
				auto range = iter.next();
				if (!range.has_value)
				{
					return {s.size()};
				}
				const auto sub_slice = slice{range->raw_begin() - offset, range->raw_end() - range->raw_begin(), 1};
				auto len = range->raw_begin() - offset;
				if (range->is_full())
				{
					RECHECK(len, write_at_b(range->block, buf, sub_slice));
				}
				else
				{
					auto block_buf = std::valarray<byte>{mem_page_size};
					auto block_buf_slice = slice{0, mem_page_size, 1};
					RECHECK(len, write_at_b(range->block, block_buf,block_buf_slice ));
					block_buf[slice{range->begin(), range->end() - range->begin(), 1}] = buf[sub_slice];
					RECHECK(len, write_at_b(range->block, block_buf,block_buf_slice));
				}
			}
		}
	};
}

#endif //TINY_DB_ENGINE_FILESYSTEM_FILE_HPP
