#pragma once

#include <future>

#include <utility>
#include  <list>
#include <algorithm>

namespace eevent
{
	using std::partition;
	using std::async;
	using std::future;
	
	using estl::move;
	using estl::list;
	using estl::partition;

	template <typename T>
	list<T> parallel_quick_sort(list<T> input)
	{
		if (input.empty())
		{
			return input;
		}
		list<T> results;
		results.splice(results.begin(), input, input.begin());
		const T &pivot = *results.begin();
		auto divide_point = partition(input.begin(), input.end(), [&](const T &t) {return t < pivot; });
		list<T> low_part;
		low_part.splice(low_part.end(), input, input.begin(), divide_point);
		future<list<T>> new_lower{
			async(&parallel_quick_sort<T>,move(low_part)) };
		auto new_higher{ parallel_quick_sort(move(input)) };
		results.splice(results.end(), new_higher);
		results.splice(results.begin(), new_lower.get());
		return results;
	}
}