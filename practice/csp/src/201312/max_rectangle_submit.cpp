#ifdef NOW

#include <cstdint>
#include <vector>
#include <limits>
#include <algorithm>
#include <iostream>

using namespace std;

int main()
{
    int32_t size;
    cin >> size;
    auto numbers = vector<int32_t>(size);
	int32_t max_area = 0;
	for(auto i=0;i<size;i++) {
	    cin>>numbers[i];
	}
	for(auto i=0;i<size;i++)
	{
		int32_t boundary = numeric_limits<int32_t>::max();
		for(auto j=i;j<size;j++) {
			boundary = min(boundary,numbers[j]);
			max_area = max(max_area,boundary*(j-i+1));
		}
	}
	cout << max_area << endl;
	return 0;
}

#endif NOW
