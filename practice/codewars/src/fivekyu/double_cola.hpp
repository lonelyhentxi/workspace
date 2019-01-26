#ifndef CODEWARS_DOUBLE_COLA_HPP
#define CODEWARS_DOUBLE_COLA_HPP

#include <vector>
#include <cassert>
#include <cmath>

namespace codewars {
    namespace double_cola {
        using std::string;
        using std::vector;

        string who_is_next(const vector<string> &names, long long r) {
            assert(r >= 1);
            int repeat = 1;
            long long drunk_times = 1;
            const size_t size = names.size();
            while (drunk_times * size < r) {
                drunk_times += repeat*2;
                repeat *= 2;
            }
            size_t index = (drunk_times*size-r)/ repeat;
            return names[size-index-1];
        }
    }
}

#endif //CODEWARS_DOUBLE_COLA_HPP
