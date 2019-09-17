#ifndef CODEWARS_SUM_OF_INTERVALS_HPP
#define CODEWARS_SUM_OF_INTERVALS_HPP

#include <vector>
#include <algorithm>
#include <utility>

namespace codewars {
    namespace fourkyu {
        using namespace std;

        int sum_intervals(vector<pair<int, int>> intervals) {
            sort(intervals.begin(), intervals.end(),
                 [](const std::pair<int,int> &left, const std::pair<int,int> &right) -> bool { return left.first < right.first; });
            auto overlap_removed_nodes = vector<pair<int,int>>{};
            overlap_removed_nodes.push_back(intervals[0]);
            for(auto i=1;i<intervals.size();i++) {
                auto &prev = overlap_removed_nodes.back();
                const auto &current = intervals[i];
                if(current.first <= prev.second) {
                    prev.second = max(prev.second,current.second);
                } else {
                    overlap_removed_nodes.push_back(intervals[i]);
                }
            }
            int sum = 0;
            for(const auto &item: overlap_removed_nodes) {
                sum+= item.second - item.first;
            }
            return sum;
        }
    }
}

#endif //CODEWARS_SUM_OF_INTERVALS_HPP
