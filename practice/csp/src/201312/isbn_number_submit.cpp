#ifdef NOW

#include <string>
#include <cstdint>
#include <cstdlib>
#include <sstream>
#include <vector>
#include <iostream>

using namespace std;

int main() {
	string input;
	cin >> input; 
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
    sum = sum%11;
    if(sum==numbers[numbers.size()-1]) {
       cout << "Right" << endl;
    } else {
        string output = input;
        if(sum==10) {
            output.back() = 'X';
        } else {
            output.back() = static_cast<char>(static_cast<int32_t>('0') + sum);
        }
        cout << output;
    }
    return 0;
}

#endif
