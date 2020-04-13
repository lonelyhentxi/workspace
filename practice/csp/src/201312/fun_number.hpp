#ifndef CSP_FUN_NUMBER_HPP
#define CSP_FUN_NUMBER_HPP

#include <cstdint>
#include <vector>

using namespace std;

constexpr int32_t MOD = 1000000007;

int64_t fun_number(int32_t size)
{
    auto status = vector<vector<int64_t>>(1001,vector<int32_t>(6,0));

    // DP
    status[1][0] = 1;
    for(int i=2; i<=size; i++) {
        status[i][0] = 1;
        status[i][1] = (status[i - 1][1] * 2 + status[i - 1] [0]) % MOD;
        status[i][2] = (status[i - 1][2] + status[i - 1][0]) % MOD;
        status[i][3] = (status[i - 1][3] * 2 + status[i - 1][1] ) % MOD;
        status[i][4] = (status[i - 1][4] * 2 + status[i - 1][1] + status[i - 1][2]) % MOD;
        status[i][5] = (status[i - 1][5] * 2 + status[i - 1][3] + status[i - 1][4]) % MOD;
    }

    return status[size][5];
}


#endif //CSP_FUN_NUMBER_HPP
