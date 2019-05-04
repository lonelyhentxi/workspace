#ifndef TINY_DB_ENGINE_CORE_HPP
#define TINY_DB_ENGINE_CORE_HPP

#include "functools.hpp"

namespace tinydb::core
{
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
		template<typename SimpleTable>
		void inject(shared_ptr<SimpleTable>& st, const size_t start_block_id,const size_t record_num) const {
			st->device_ = device;
			st->start_block_id_ = start_block_id;
			st->record_num_ = record_num;
		}
	};

	struct unit
	{
		virtual size_t size() const = 0;
		virtual ~unit() = default;
	};

	struct record
	{
		virtual size_t size() const = 0;
		virtual ~record() = default;
	};

	struct table_iterator;
	struct table_insertor;

	struct table
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
		virtual shared_ptr<record> deserialize(const valarray<byte>& buf, const slice& s) const = 0;
		virtual void serialize(const shared_ptr<record>& r, valarray<byte>& buf, const slice& s) const = 0;
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
			return { copy };
		}

		optional<table_iterator> skip(int64_t off) const
		{
			const auto target = start + off + offset;
			return to(target);
		}

		optional<table_iterator> to(int64_t off) const
		{
			auto copy = *this;
			const auto target = off;
			if (target < 0 || target >= table->num())
			{
				return {};
			}
			copy.start = size * (target / size);
			copy.offset = target - copy.start;
			copy.end = copy.start + copy.size;
			copy.end = copy.end > table->num() ? table->num() : copy.end;
			if (copy.start != start)
			{
				copy.records.clear();
				copy.records = (copy.table->read_batch_records(copy.start, copy.end));
			}
			return { copy };
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
				copy.table->write_batch_records(copy.records, copy.start, copy.end);
				copy.records.clear();
				copy.offset = 0;
				copy.start = copy.start + copy.size;
				copy.end = copy.end + copy.size;
			}
			return copy;
		}

		void retrieve(shared_ptr<record> re)
		{
			if (offset == records.size() - 1)
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
			if(records.size()!=0)
			{
				table->write_batch_records(records, start, start + records.size());
			}
		}
	};

	template <typename T>
	struct intergal_unit final : public unit
	{
		using value_type = typename enable_if<std::is_integral<T>::value, T>::type;
		value_type value;
		size_t size() const override
		{
			return sizeof(T);
		}

		~intergal_unit() override = default;
	};

	template <typename T>
	struct intergal_unit_builder
	{
		using value_type = typename enable_if<std::is_integral<T>::value, T>::type;

		static shared_ptr<intergal_unit<value_type>> deserialize(const valarray<byte>& buf, const slice& s)
		{
			auto r = make_shared<intergal_unit<value_type>>();
			const auto s1 = slice(s.start(), sizeof(T), 1);
			valarray_to_integer(r->value, buf, s1);
			return r;
		}

		static void serialize(const shared_ptr<intergal_unit<value_type>>& r, valarray<byte>& buf, const slice& s)
		{
			const auto s1 = slice(s.start(), sizeof(T), 1);
			integer_to_valarray(r->value, buf, s1);
		}

		constexpr size_t size() const
		{
			return sizeof(value_type);
		}
	};


	template <typename T>
	struct fixed_record : public record
	{
		using types_tag = typename enable_if<is_tuple<T>::value, T>::type;
		array<shared_ptr<unit>, tuple_size<types_tag>::value> values;

		template<size_t I>
		constexpr shared_ptr<intergal_unit<typename std::tuple_element<I, types_tag>::type>> get()
		{
			return dynamic_pointer_cast<intergal_unit<typename std::tuple_element<I, types_tag>::type>>(values[I]);
		}
		size_t size() const override
		{
			return tuple_size_adder<types_tag>();
		}

		~fixed_record() override = default;
	};

	template <typename T, size_t N, size_t C>
	constexpr void simple_table_iarchiver_rec(array<shared_ptr<unit>, N>& v, const valarray<byte>& buf,
		const slice& s)
	{
		if constexpr (C == N)
		{
			return;
		}
		else
		{
			size_t size = sizeof(typename std::tuple_element<C, T>::type);
			v[C] = intergal_unit_builder<typename std::tuple_element<C, T>::type>::deserialize(
				buf, std::slice(s.start(), size, 1));
			return simple_table_iarchiver_rec<T, N, C + 1>(v,buf,slice(s.start()+size,size,1));
		}
	}

	template <typename T>
	constexpr void simple_table_iarchiver(array<shared_ptr<unit>, std::tuple_size<T>::value>& v,
		const valarray<byte>& buf, const slice& s)
	{
		simple_table_iarchiver_rec<T, std::tuple_size<T>::value,0>(v, buf, s);
	}


	template <typename T, size_t N, size_t C>
	constexpr void simple_table_oarchiver_rec(const array<shared_ptr<unit>, N>& v, valarray<byte>& buf, const slice& s)
	{
		if constexpr (C == N)
		{
			return;
		}
		else
		{
			size_t size = sizeof(typename std::tuple_element<C, T>::type);
			intergal_unit_builder<typename std::tuple_element<C, T>::type>
				::serialize(dynamic_pointer_cast<intergal_unit<typename std::tuple_element<C, T>::type>>(v[C]), buf,
					std::slice(s.start(), size, 1));
			return simple_table_oarchiver_rec<T, N, C + 1>(v, buf, slice(s.start() + size, size,1));
		}
	}

	template <typename T>
	constexpr void simple_table_oarchiver(const array<shared_ptr<unit>, std::tuple_size<T>::value>& v,
		valarray<byte>& buf, const slice& s)
	{
		simple_table_oarchiver_rec<T, std::tuple_size<T>::value, 0>(v, buf, s);
	}

	template <typename Record>
	struct simple_table final : public table
	{
	private:
		shared_ptr<extmem_device> device_;
		size_t start_block_id_;
		size_t record_num_;
		friend struct table_insertor;
		friend struct table_iterator;
		friend struct engine;
	public:
		using record_type = Record;
		~simple_table() override = default;
		shared_ptr<record> deserialize(const valarray<byte>& buf, const slice& s) const override
		{
			auto res = make_shared<Record>();
			simple_table_iarchiver<typename Record::types_tag>(res->values, buf, s);
			return res;
		}

		void serialize(const shared_ptr<record>& r, valarray<byte>& buf, const slice& s) const override
		{
			simple_table_oarchiver<typename Record::types_tag>(dynamic_pointer_cast<Record>(r)->values, buf, s);
		}

		size_t record_start_block_id(const size_t record_id) override
		{
			return (record_id / batch_size() + start()) * (device()->block_size()) + ((record_id % batch_size()) *
				record_size());
		}

		size_t record_end_block_id(const size_t record_id) override
		{
			return (record_id / batch_size() + start()) * (device()->block_size() + 1) + ((record_id % batch_size()) *
				record_size());
		}

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
			return tuple_size_adder<typename record_type::types_tag> ();
		}

		size_t batch_size() override
		{
			return (device()->block_size() - 4) / record_size();
		}

		size_t num() override
		{
			return record_num_;
		}

		optional<table_iterator> get_iterator(const shared_ptr<table> & t) override
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

		optional<table_insertor> get_insertor(const shared_ptr<table> & t) override
		{
			auto copy = table_insertor{
				0, 0, t->batch_size(), t->batch_size(), t, {}
			};
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
				res.push_back(deserialize(buf, s));
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
				serialize(dynamic_pointer_cast<record_type>(rs[i]), buf, s);
			}
			write_batch(buf, start, end);
		}

		void write_batch_records(const vector<shared_ptr<record>> & rs, const size_t start,
			const size_t end) override
		{
			const auto all_start = record_start_block_id(start);
			const auto all_end = record_start_block_id(end);
			const auto all_size = all_end - all_start;
			auto buf = valarray<byte>(byte(0), all_size);
			for (size_t i = 0, j = start; j < end; j++, i++)
			{
				const auto block_start = record_start_block_id(i);
				const auto s = slice(block_start, record_size(), 1);
				serialize(dynamic_pointer_cast<record_type>(rs[i]), buf, s);
			}
			write_batch(buf, start, end);
			if (end > num())
			{
				record_num_ = end;
			}
		}
	};
}

#endif
