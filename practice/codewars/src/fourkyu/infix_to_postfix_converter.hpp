#ifndef INFIX_TO_POSTFIX_CONVERTER_HPP
#define INFIX_TO_POSTFIX_CONVERTER_HPP

#include <string>
#include <regex>
#include <locale>
#include <unordered_map>
#include <stack>

namespace codewars {
    namespace fourkyu {
        using namespace std;

        static unordered_map<char, int32_t> operator_priority = {{'+', 1},
                                                                 {'*', 2},
                                                                 {'-', 1},
                                                                 {'/', 2},
                                                                 {'^', 3}};

        string to_postfix(const string &infix) {
            const auto loc = locale{};
            auto processed = stack<char>{};
            auto res = string{};
            for (const auto &c: infix) {
                if (isdigit(c, loc)) {
                    res.push_back(c);
                } else if (c == '(') {
                    processed.push(c);
                } else if (c == ')') {
                    while (!processed.empty()) {
                        auto top = processed.top();
                        if (top != '(') {
                            res.push_back(top);
                            processed.pop();
                        } else {
                            processed.pop();
                            break;
                        }
                    }
                } else {
                    const auto current_priority = operator_priority[c];
                    while (!processed.empty()) {
                        auto top = processed.top();
                        if (operator_priority[top] >= current_priority) {
                            res.push_back(top);
                            processed.pop();
                        } else {
                            break;
                        }
                    }
                    processed.push(c);
                }
            }
            while (!processed.empty()) {
                res.push_back(processed.top());
                processed.pop();
            }
            return res;
        }
    }
}

#endif // INFIX_TO_POSTFIX_CONVERTER_HPP
