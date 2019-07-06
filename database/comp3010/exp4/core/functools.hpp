#ifndef TINY_DB_ENGINE_FUNCTOOLS_HPP 
#define TINY_DB_ENGINE_FUNCTOOLS_HPP

#include <cstdint>
#include <memory>
#include <vector>
#include <optional>
#include <valarray>
#include <sstream>
#include <iomanip>
#include <cstddef>
#include <exception>
#include <type_traits>
#include <optional>
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
	using std::enable_if;
	using std::make_shared;
	using std::dynamic_pointer_cast;
	using std::static_pointer_cast;
	using std::enable_shared_from_this;
	using std::array;
	using tinydb::extmem::extmem_device;
	using tinydb::extmem::extmem_device_manager;
	using tinydb::filesystem::block_stream_device;
	using std::tuple_size;


	template <typename> struct is_tuple : std::false_type {};

	template <typename ...T> struct is_tuple<std::tuple<T...>> : std::true_type {};

	template<typename T, size_t N>
	constexpr size_t tuple_size_adder_cal() {
		if constexpr(N==0) {
			return sizeof(std::tuple_element<0, T>::type);
		} else
		{
			return sizeof(std::tuple_element<N, T>::type) + tuple_size_adder_cal<T, N - 1>();
		}
	}

	template<typename T>
	constexpr size_t tuple_size_adder() {
		return tuple_size_adder_cal<T, std::tuple_size<T>::value-1>();
	}

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

}
#endif
