#ifndef CODEWARS_NUMBER_OF_TRAILING_ZEROS_OF_N_HPP
#define CODEWARS_NUMBER_OF_TRAILING_ZEROS_OF_N_HPP

#include <cmath>

namespace codewars {
    namespace number_of_trailing_zeros_of_n {
        long zeros(long n) {
            using std::log2;
            using std::floor;
            using std::pow;
            auto k = static_cast<int>(floor(log2(n)/log2(5)));
            long sum = 0;
            for(auto i=1;i<=k;i++) {
                sum+=n/ static_cast<long>(pow(5,i));
            }
            return sum;
        }
    }
}

#endif //CODEWARS_NUMBER_OF_TRAILING_ZEROS_OF_N_HPP
