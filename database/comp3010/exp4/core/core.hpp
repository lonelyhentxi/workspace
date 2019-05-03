#ifndef TINY_DB_ENGINE_CORE_HPP
#define TINY_DB_ENGINE_CORE_HPP

#include <cstdint>
#include <memory>
#include <vector>
#include <optional>
#include <valarray>
#include <sstream>
#include <iomanip>
#include <cstddef>
#include <exception>
#include "extmemfs/basic/dev.hpp"
#include "extmemfs/extmem_dev/extmem_dev.hpp"

namespace tinydb::core
{
	using std::vector;
	using std::optional;
	using std::shared_ptr;
	using std::tuple;
	using std::valarray;
	using std::slice;
	using std::stringstream;
	using std::setw;
	using std::setbase;
	using std::byte;
	using std::exception;
	using std::make_shared;
	using std::dynamic_pointer_cast;
	using std::static_pointer_cast;
	using std::enable_shared_from_this;
	using tinydb::extmem::extmem_device;
	using tinydb::extmem::extmem_device_manager;
	using tinydb::filesystem::block_stream_device;

	template <typename I>
	void integer_to_valarray(const I value, valarray<byte>& buf, const slice& s)
	{
		stringstream ss;
		uint8_t tmp;
		ss << setbase(16) << setw(sizeof(I) * 2) << value;
		for (auto i = s.start(); i < s.size() * s.stride(); i += s.stride())
		{
			ss >> setbase(16) >> setw(sizeof(uint8_t) * 2) >> tmp;
			buf[i] = static_cast<byte>(tmp);
		}
	}

	template <typename I>
	void valarray_to_integer(I& value, const valarray<byte>& buf, const slice& s)
	{
		stringstream ss;
		for (auto i = s.start(); i < s.size() * s.stride(); i += s.stride())
		{
			ss << setbase(16) << setw(sizeof(uint8_t) * 2) << static_cast<uint8_t>(buf[i]);
		}
		ss >> setbase(16) >> setw(sizeof(I) * 2) >> value;
	}

	struct engine
	{
		shared_ptr<extmem_device> device;
		engine()
		{
			auto dev = extmem_device_manager::make("raw", 10 * 1024 * 1024, 64, 520);
			if (!dev.has_value())
			{
				throw exception{"mount device failed"};
			}
			device = make_shared<extmem_device>(std::move(*dev));
		}
	};

	struct record
	{
		virtual ~record() = default;
	};

	struct table_insertor;
	struct table_iterator;

	struct table: public std::enable_shared_from_this<table>
	{
		virtual size_t record_start_block_id(const size_t record_id)
		{
			return  (record_id / batch_size() + start() ) * (device()->block_size()) + ((record_id % batch_size()) *
				record_size());
		}
		virtual size_t record_end_block_id(const size_t record_id)
		{
			return  (record_id / batch_size() + start() ) * (device()->block_size() + 1) + ((record_id % batch_size()) *
				record_size());
		}
		virtual shared_ptr<extmem_device> device() = 0;
		virtual size_t start() = 0;
		virtual size_t record_size() = 0;
		virtual size_t batch_size() = 0;
		virtual size_t num() = 0;
		virtual optional<table_insertor> get_insertor(const shared_ptr<table>& t) = 0;
		virtual optional<table_iterator> get_iterator(const shared_ptr<table>& t) = 0;
		virtual valarray<byte> read_batch(const size_t start, const size_t end)
		{
			const auto start_byte = record_start_block_id(start);
			const auto block_byte = record_start_block_id(end) - start_byte;
			auto buf = valarray<byte>(byte{0}, block_byte);
			const auto s = slice(0, block_byte, 1);
			shared_ptr<block_stream_device> dev = device();
			const auto len_res = dev->read_at(start_byte, buf, s);
			if(!len_res.has_value()||*len_res!=block_byte)
			{
				throw exception{"read batch error"};
			}
			return buf;
		}
		virtual void write_batch(const valarray<byte> &buf, const size_t start, const size_t end)
		{
			const auto start_byte = record_start_block_id(start);
			const auto block_byte = record_start_block_id(end) - start_byte;
			const auto s = slice(0, buf.size(), 1);
			shared_ptr<block_stream_device> dev = device();
			const auto len_res = dev->write_at(start_byte, buf, s);
			if (!len_res.has_value() || *len_res != block_byte)
			{
				throw exception{ "write batch error" };
			}
		}
		virtual void write_batch_records(valarray<byte> buf, const vector<shared_ptr<record>> &rs, const size_t start,const size_t end, const vector<size_t>& offsets) = 0;
		virtual void write_batch_records(const vector<shared_ptr<record>>& rs, const size_t start, const size_t end) = 0;
		virtual vector<shared_ptr<record>> read_batch_records(const size_t start, const size_t end) = 0;
		virtual ~table() = default;
	};

	struct table_iterator
	{
		size_t offset;
		size_t start;
		size_t size;
		size_t end;
		shared_ptr<table> table;
		vector<shared_ptr<record>> records;

		optional<table_iterator> next() const
		{
			auto copy = *this;
			copy.offset++;
			if (copy.offset >= copy.size)
			{
				copy.offset = 0;
				copy.start = copy.end;
				if (copy.start >= table->num())
				{
					return {};
				}
				copy.end = copy.start + copy.size;
				copy.end = copy.end > table->num() ? table->num() : copy.end;
				copy.records.clear();
				copy.records = (copy.table->read_batch_records(copy.start, copy.end));
			}
			return {copy};
		}

		shared_ptr<record> retrieve() const
		{
			return records[offset];
		}
	};

	struct table_insertor
	{
		size_t offset;
		size_t start;
		size_t size;
		size_t end;
		shared_ptr<table> table;
		vector<shared_ptr<record>> records;

		table_insertor next() const
		{
			auto copy = *this;
			copy.offset++;
			if (copy.offset >= copy.size)
			{
				copy.offset = 0;
				copy.start = copy.end;
				if (copy.start >= table->num())
				{
					copy.end = copy.start + copy.size;
				}
				copy.table -> write_batch_records(copy.records, copy.start, copy.end);
				copy.records.clear();
			}
			return copy;
		}

		void retrieve(shared_ptr<record> re)
		{
			if(offset == size-1)
			{
				records[offset] = re;
			}
			else
			{
				records.push_back(re);
			}
		}

		void save()
		{
			table->write_batch_records(records, start, start + records.size());
		}
	};
}

#endif
