#ifndef CODEWARS_ARE_THEY_THE_SAME_HPP
#define CODEWARS_ARE_THEY_THE_SAME_HPP


#include <vector>
#include <algorithm>
#include <cmath>
#include <functional>

namespace codewars {
    namespace are_they_the_same {
        using std::vector;
        using std::sort;
        using std::function;

        class Same {
        public :
            static bool comp(vector<int> as, vector<int> bs) {
                if (as.size() != bs.size()) {
                    return false;
                }
                const function<bool(int,int)> abs_comp = [](int a, int b) -> bool { return std::abs(b) < std::abs(a); };
                sort(as.begin(), as.end(), abs_comp);
                sort(bs.begin(), bs.end(), abs_comp);
                bool flag = true;
                for (auto i = 0; i < as.size(); i++) {
                    if (static_cast<int>(std::pow(as[i], 2)) != bs[i]) {
                        flag = false;
                    }
                }
                return flag;
            }
        };
    }
}

#endif //CODEWARS_ARE_THEY_THE_SAME_HPP
