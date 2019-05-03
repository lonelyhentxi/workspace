#ifndef TINY_DB_ENGINE_EXTENSION_HPP
#define TINY_DB_ENGINE_EXTENSION_HPP

#include <cstdint>
#include <memory>
#include <vector>
#include <optional>
#include <valarray>
#include <sstream>
#include <iomanip>
#include <cstddef>
#include "core.hpp"

namespace tinydb::extmem
{
	using namespace tinydb::core;

	struct r_record final : public record
	{
		int32_t a;
		int32_t b;

		static shared_ptr<r_record> deserialize(const valarray<byte>& buf, const slice& s)
		{
			auto r = make_shared<r_record>();
			const auto s1 = slice(s.start(), sizeof(r->a), 1);
			valarray_to_integer(r->a, buf, s1);
			const auto s2 = slice(s.start() + sizeof(r->a), sizeof(r->b), 1);
			valarray_to_integer(r->b, buf, s2);
			return r;
		}

		static void serialize(const shared_ptr<r_record> &r, valarray<byte>& buf, const slice& s)
		{
			const auto s1 = slice(s.start(), sizeof(r->a), 1);
			integer_to_valarray(r->a, buf, s1);
			const auto s2 = slice(s.start() + sizeof(r->a), sizeof(r->b), 1);
			integer_to_valarray(r->b, buf, s2);
		}
		~r_record() override = default;
	};

	struct r_table final : public table
	{
	private:
		friend table_iterator;
		shared_ptr<extmem_device> device_;
		size_t start_block_id_;
		size_t record_num_;
	public:
		r_table(const shared_ptr<extmem_device>& device,const size_t start_block_id,const size_t record_num)
		{
			device_ = device;
			start_block_id_ = start_block_id;
			record_num_ = record_num;
		}
		using record_type = r_record;

		shared_ptr<extmem_device> device() override
		{
			return device_;
		}

		size_t start() override
		{
			return start_block_id_;
		}

		size_t record_size() override
		{
			return sizeof(record_type::a) + sizeof(record_type::b);
		}

		size_t batch_size() override
		{
			return (device()->block_size() - 4) / record_size();
		}

		size_t num() override
		{
			return record_num_;
		}

		optional<table_iterator> get_iterator(const shared_ptr<table>& t) override
		{
			if (t->num() == 0)
			{
				return {};
			}
			const auto size = t->batch_size() <= t->num() ? t->batch_size() : t->num();
			auto copy = table_iterator{
				0, 0, size, size, t, t->read_batch_records(0, size)
			};
			return { copy };
		}

		optional<table_insertor> get_insertor(const shared_ptr<table>& t) override
		{
			auto copy = table_insertor {
				0, 0, t->batch_size(), t->batch_size(), t, {}};
			return { copy };
		}
		

		vector<shared_ptr<record>> read_batch_records(const size_t start, const size_t end) override
		{
			auto res = vector<shared_ptr<record>>{};
			const auto buf = read_batch(start, end);
			const auto all_start = record_start_block_id(start);
			for (auto i = start; i < end; i++)
			{
				const auto block_start = record_start_block_id(i);
				const auto s = slice(block_start - all_start, record_size(), 1);
				res.push_back(record_type::deserialize(buf, s));
			}
			return res;
		}

		void write_batch_records(valarray<byte> buf, const vector<shared_ptr<record>>& rs, const size_t start,
		                         const size_t end, const vector<size_t>& offsets) override
		{
			const auto all_start = record_start_block_id(start);
			for (const auto i : offsets)
			{
				const auto block_start = record_start_block_id(i);
				const auto s = slice(block_start - all_start, record_size(), 1);
				record_type::serialize(dynamic_pointer_cast<r_record>(rs[i]), buf, s);
			}
			write_batch(buf, start, end);
		}

		void write_batch_records(const vector<shared_ptr<record>>& rs, const size_t start,
			const size_t end) override
		{
			const auto all_start = record_start_block_id(start);
			const auto all_end = record_start_block_id(end);
			const auto all_size = all_end - all_start;
			auto buf = valarray<byte>( byte( 0 ), all_size);
			for (size_t i=0,j=start; j<end;j++,i++)
			{
				const auto block_start = record_start_block_id(i);
				const auto s = slice(block_start - all_start, record_size(), 1);
				record_type::serialize(dynamic_pointer_cast<r_record>(rs[i]), buf, s);
			}
			write_batch(buf, start, end);
			if(end>num())
			{
				record_num_ = end;
			}
		}


		~r_table() override = default;
	};

	struct s_record final : public record
	{
		int32_t c;
		int32_t d;

		static shared_ptr<s_record> deserialize(const valarray<byte>& buf, const slice& s)
		{
			auto r = make_shared<s_record>();
			const auto s1 = slice(s.start(), sizeof(r->c), 1);
			valarray_to_integer(r->c, buf, s1);
			const auto s2 = slice(s.start() + sizeof(r->c), sizeof(r->d), 1);
			valarray_to_integer(r->d, buf, s2);
			return r;
		}

		static void serialize(const shared_ptr<s_record>& r, valarray<byte>& buf, const slice& s)
		{
			const auto s1 = slice(s.start(), sizeof(r->c), 1);
			integer_to_valarray(r->c, buf, s1);
			const auto s2 = slice(s.start() + sizeof(r->c), sizeof(r->d), 1);
			integer_to_valarray(r->d, buf, s2);
		}
		~s_record() override = default;
	};

	struct s_table final : public table
	{
	private:
		friend table_iterator;
		shared_ptr<extmem_device> device_;
		size_t start_block_id_;
		size_t record_num_;
	public:
		s_table(const shared_ptr<extmem_device>& device, const size_t start_block_id, const size_t record_num)
		{
			device_ = device;
			start_block_id_ = start_block_id;
			record_num_ = record_num;
		}

		using record_type = s_record;

		shared_ptr<extmem_device> device() override
		{
			return device_;
		}

		size_t start() override
		{
			return start_block_id_;
		}

		size_t record_size() override
		{
			return sizeof(record_type::c) + sizeof(record_type::d);
		}

		size_t batch_size() override
		{
			return (device()->block_size() - 4) / record_size();
		}

		size_t num() override
		{
			return record_num_;
		}

		optional<table_iterator> get_iterator(const shared_ptr<table>& t) override
		{
			if (t->num() == 0)
			{
				return {};
			}
			const auto size = t->batch_size() <= t->num() ? t->batch_size() : t->num();
			auto copy = table_iterator{
				0, 0, size, size, t, t->read_batch_records(0, size)
			};
			return { copy };
		}

		optional<table_insertor> get_insertor(const shared_ptr<table>& t) override
		{
			auto copy = table_insertor {
				0, 0, t->batch_size(), t->batch_size(), t, {} };
			return { copy };
		}

		vector<shared_ptr<record>> read_batch_records(const size_t start, const size_t end) override
		{
			auto res = vector<shared_ptr<record>>{};
			const auto buf = read_batch(start, end);
			const auto all_start = record_start_block_id(start);
			for (auto i = start; i < end; i++)
			{
				const auto block_start = record_start_block_id(i);
				const auto s = slice(block_start - all_start, record_size(), 1);
				res.push_back(record_type::deserialize(buf, s));
			}
			return res;
		}

		void write_batch_records(valarray<byte> buf, const vector<shared_ptr<record>> & rs, const size_t start,
			const size_t end, const vector<size_t> & offsets) override
		{
			const auto all_start = record_start_block_id(start);
			for (const auto i : offsets)
			{
				const auto block_start = record_start_block_id(i);
				const auto s = slice(block_start - all_start, record_size(), 1);
				record_type::serialize(dynamic_pointer_cast<s_record>(rs[i]), buf, s);
			}
			write_batch(buf, start, end);
		}

		void write_batch_records(const vector<shared_ptr<record>>& rs, const size_t start,
			const size_t end) override
		{
			const auto all_start = record_start_block_id(start);
			const auto all_end = record_start_block_id(end);
			const auto all_size = all_end - all_start;
			auto buf = valarray<byte>(byte(0), all_size);
			for (size_t i = 0, j = start; j < end; j++, i++)
			{
				const auto block_start = record_start_block_id(i);
				const auto s = slice(block_start - all_start, record_size(), 1);
				record_type::serialize(dynamic_pointer_cast<s_record>(rs[i]), buf, s);
			}
			write_batch(buf, start, end);
			if (end > num())
			{
				record_num_ = end;
			}
		}

		~s_table() override = default;
	};
}

#endif
