#ifndef CODEWARS_PLAYING_WITH_PASSPHRASES_HPP
#define CODEWARS_PLAYING_WITH_PASSPHRASES_HPP

#include <iostream>
#include <algorithm>
#include <locale>

namespace codewars {
    using std::string;
    using std::locale;
    using std::isdigit;
    using std::isalpha;
    using std::tolower;
    using std::for_each;
    using std::toupper;
    using std::reverse;
    namespace playing_with_passphrases {
        class PlayPass {
        public:
            static string playPass(const std::string &s, int n) {
                const int a_num = static_cast<int>('a');
                const int zero_num = static_cast<int>('0');
                locale loc{"C"};
                // step 1, 2
                string current = {};
                for (auto ch: s) {
                    if (isalpha(ch, loc)) {
                        const auto shifted_s = static_cast<string::value_type>(
                                (static_cast<int>(tolower(ch, loc)) - a_num + n + 26) % 26 + a_num);
                        current.push_back(shifted_s);
                    } else if (isdigit(ch, loc)) {
                        current.push_back(static_cast<string::value_type>(9 - (static_cast<int>(ch) - zero_num) + zero_num));
                    } else {
                        current.push_back(ch);
                    }
                }
                size_t i = 0;
                for_each(current.begin(), current.end(), [&i, &loc](string::value_type &ch)
                        -> void {
                    if (i % 2 == 0) { ch = toupper(ch, loc); } else { ch = tolower(ch, loc); }
                    i++;
                });
                reverse(current.begin(), current.end());
                return current;
            }
        };
    }
}

#endif //CODEWARS_PLAYING_WITH_PASSPHRASES_HPP
