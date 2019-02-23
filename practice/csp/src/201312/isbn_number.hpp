#ifndef CSP_ISBN_NUMBER_HPP
#define CSP_ISBN_NUMBER_HPP

#ifdef WITH_CMAKE

#include <utility>
#include <string>
#include <cstdint>
#include <cstdlib>
#include <sstream>
#include <vector>

using namespace std;

pair<bool,string> isbn_number(const string &input) {
    vector<int32_t> numbers{};
    for(const auto &ch: input) {
        if(isdigit(ch)) {
			stringstream ss;
        	ss << ch;
            int32_t digit;
            ss >> digit;
            numbers.push_back(digit);
        }
    }
    if(input[input.size()-1]=='X') {
        numbers.push_back(10);
    }
    int32_t sum = 0;
    for(auto i=0;i<9;i++) {
        sum += (i+1)*numbers[i];
    }
    pair<bool,string> res {true,""};
    sum = sum%11;
    if(sum==numbers[numbers.size()-1]) {
        res.first = true;
    } else {
        res.first = false;
        res.second = input;
        if(sum==10) {
            res.second.back() = 'X';
        } else {
            res.second.back() = static_cast<char>(static_cast<int32_t>('0') + sum);
        }
    }
    return res;
}

#endif

#endif //CSP_ISBN_NUMBER_HPP
