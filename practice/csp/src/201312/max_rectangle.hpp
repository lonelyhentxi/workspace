#ifndef CSP_MAX_RECTANGEL_HPP
#define CSP_MAX_RECTANGEL_HPP

#include <cstdint>
#include <vector>
#include <limits>
#include <algorithm>

using namespace std;

int32_t max_rectangle(int32_t size,vector<int32_t> numbers)
{
	int32_t max_area = 0;
	for(auto i=0;i<size;i++)
	{
		int32_t boundary = numeric_limits<int32_t>::max();
		for(auto j=i;j<size;j++) {
			boundary = min(boundary,numbers[j]);
			max_area = max(max_area,boundary*(j-i+1));
		}
	}
	return max_area;
}

#endif