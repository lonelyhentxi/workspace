#include <unordered_map>
#include <cstdint>
#include <iostream>
#include <limits>


#ifdef NOW

using namespace std;

int main()
{
	int32_t size;
    int32_t temp_number;
    auto counter = unordered_map<int32_t, int32_t>{};
    cin >> size;
    for(auto i = 0;i<size;i++)
    {
        cin >> temp_number;
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
    cout << target << endl;
    return 0;
}

#endif
