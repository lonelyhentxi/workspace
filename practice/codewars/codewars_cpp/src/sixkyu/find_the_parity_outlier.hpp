#ifndef CODEWARS_FIND_THE_PARITY_OUTLIER_HPP
#define CODEWARS_FIND_THE_PARITY_OUTLIER_HPP

#include <vector>
#include <exception>

namespace codewars {
    namespace find_the_parity_outlier {
        using std::vector;
        using std::exception;

        int FindOutlier(std::vector<int> arr) {
            size_t even = 0;
            size_t odd = 0;
            int even_item = 0;
            int odd_item = 0;
            for (const auto &item: arr) {
                if (item % 2 == 0) {
                    even += 1;
                    if (even == 1) {
                        even_item = item;
                        if (odd > 1) {
                            return even_item;
                        }
                    } else if (even > 1 && odd > 0) {
                        return odd_item;
                    }
                } else {
                    odd += 1;
                    if (odd == 1) {
                        odd_item = item;
                        if (even > 1) {
                            return item;
                        }
                    } else if (odd > 1 && even > 0) {
                        return even_item;
                    }
                }
            }
            throw exception{};
        }
    }
}

#endif //CODEWARS_FIND_THE_PARITY_OUTLIER_H
