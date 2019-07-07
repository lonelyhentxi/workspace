#ifndef CODEWARS_DOES_MY_NUMBER_LOOK_BIG_IN_THIS_HPP
#define CODEWARS_DOES_MY_NUMBER_LOOK_BIG_IN_THIS_HPP

#include <vector>
#include <cmath>

namespace codewars {
    namespace does_my_number_look_big_in_this {
        using std::vector;


        bool narcissistic(int value) {
            int temp = value;
            vector<int> digits{};
            while (temp != 0) {
                digits.push_back(temp % 10);
                temp = temp / 10;
            }
            const size_t length = digits.size();
            int sum = 0;
            for(auto d: digits) {
                sum += static_cast<int>(std::pow(d,length));
            }
            return sum==value;
        }
    }
}

#endif //CODEWARS_DOES_MY_NUMBER_LOOK_BIG_IN_THIS_H
