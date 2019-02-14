#ifndef CODEWARS_COUNT_ONE_IN_A_SEGMENT_HPP
#define CODEWARS_COUNT_ONE_IN_A_SEGMENT_HPP

#include <cstdint>
#include <bitset>

namespace codewars {
    namespace fourkyu {

        using namespace std;

        template<typename U>
        size_t bits_length(U value){
            constexpr size_t USIZE = sizeof(U)*8;
            bitset<USIZE> bits{value};
            size_t idx = USIZE;
            for(;idx>0;idx--) {
                if(bits.test(idx-1)) {
                    break;
                }
            }
            if(idx==0) {
                return 1;
            } else {
                return idx;
            }
        }

        uint64_t count(uint32_t target) {
            uint64_t sum = 0;
            while (target > 0) {
                auto length = bits_length(target) - 1;
                uint32_t pow2 = 1u << length;
                target -= pow2;
                sum += length * (pow2>>1) + 1 + target;
            }
            return sum;
        }

        int64_t count_ones(int32_t left, int32_t right) {
            return count(static_cast<uint32_t>(right)) - count(static_cast<uint32_t>(left - 1));
        }
    }
}

#endif //CODEWARS_COUNT_ONE_IN_A_SEGMENT_HPP
