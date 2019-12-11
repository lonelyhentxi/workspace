#ifndef CODEWARS_FIND_THE_MISSING_TERM_IN_AN_ARITHMETIC_PROGRESSION_HPP
#define CODEWARS_FIND_THE_MISSING_TERM_IN_AN_ARITHMETIC_PROGRESSION_HPP

#include <vector>

namespace codewars {
    using std::vector;
    namespace find_the_missing_term_in_an_arithmetic_progression {
        static long findMissing(std::vector<long> list) {
            const size_t max_distance = list.size();
            const auto step = (list.back() - list.front()) / static_cast<decltype(list)::value_type>(max_distance);
            auto current = list.front();
            for (const auto term: list) {
                if(term!=current) {
                    break;
                }
                current+=step;
            }
            return current;
        }
    }
}

#endif