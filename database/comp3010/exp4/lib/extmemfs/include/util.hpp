#ifndef TINY_DB_ENGINE_FILESYSTEM_UTIL_HPP
#define TINY_DB_ENGINE_FILESYSTEM_UTIL_HPP

#include <valarray>
#include <chrono>

namespace tinydb::filesystem::util
{
	using std::valarray;

	template<typename ToIterator,typename FromType>
	void valarray_to_iterator(ToIterator lo,ToIterator hi,const valarray<FromType> &from,size_t start)
	{
		for(;lo<hi;++lo,start++)
		{
			*lo = static_cast<typename ToIterator::value_type>(from[start]);
		}
	}

	template<typename ToContainer,typename FromType>
	void valarray_to_container(ToContainer& to,const valarray<FromType>& from, size_t len=0, size_t start=0)
	{
		auto current = start;
		to.reserve(to.size() + (len == 0 ? from.size() : len));
		for (;start<start+len;start++)
		{
			to.push_back(static_cast<typename ToContainer::value_type>(from[start]));
		}
	}

	template<typename ToIterator,typename ToType>
	void iterator_to_valarray(ToIterator lo, ToIterator hi, valarray<ToType>& to, size_t start)
	{
		auto len = hi - lo;
		to.resize(start + len);
		for (;lo<hi;++lo,start++)
		{
			to[start] = static_cast<ToType>(*lo);
		}
	}

	using sys_time_spec = std::chrono::time_point<std::chrono::system_clock, std::chrono::milliseconds>;
	constexpr size_t mem_page_size = 4096;
}

#endif