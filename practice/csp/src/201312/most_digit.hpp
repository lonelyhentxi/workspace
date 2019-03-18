#ifndef CSP_MOST_DIGIT_HPP
#define CSP_MOST_DIGIT_HPP

#include <unordered_map>
#include <cstdint>
#include <iostream>
#include <limits>
#include <vector>

using namespace std;

int32_t most_digit(int32_t size,vector<int32_t> numbers)
{
	int32_t temp_number;
	auto counter = unordered_map<int32_t, int32_t>{};
	for(auto i = 0;i<size;i++)
	{
		temp_number = numbers[i];
		counter[temp_number] += 1;
	}
	int32_t target = numeric_limits<int32_t>::max();
	int32_t count = 0;
	for(const auto &iter: counter)
	{
		if(iter.second>count||(iter.second==count&&iter.first<target))
		{
			target = iter.first;
			count = iter.second;
		}
	}
	return target;
}

#endif // CPP_MOST_DIGIT_HPP
