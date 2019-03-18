#ifndef CODEWARS_DIGIT_OF_A_HUGE_NUMBER_HPP
#define CODEWARS_DIGIT_OF_A_HUGE_NUMBER_HPP

#include <cstdint>
#include <list>
#include <numeric>
#include <cmath>
#include <iterator>


namespace codewars
{
	namespace threekyu
	{
		using namespace std;

		constexpr int64_t TEN_PRIME = 10;
		constexpr int64_t TEN_EULER_DIGIT = 4;

		// exp lower than 8
		int64_t pow_int_mod(int64_t base, int64_t exp, int64_t mod, bool pre_add = true)
		{
			int64_t res = 1;
			if (pre_add && (exp==0||(exp==1&&base<4)||(exp>1&&base<2)))
			{
				pre_add = false;
			}
			while (exp > 0) {
				exp -= 1;
				res = ((res%mod)*(base%mod)) % mod;
			}
			return res + (pre_add ? mod : 0);
		}

		int64_t last_digit(const list<int32_t> &all_digits)
		{
			// Euler's theorem
			size_t length = all_digits.size();
			if (length <= 0)
			{
				return 1;
			}
			else
			{
				int64_t others = 1;
				auto first = all_digits.rend(); --first;
				for (auto curr = all_digits.rbegin(); curr != first; ++curr)
				{
					others = pow_int_mod(*curr, others, TEN_EULER_DIGIT);
				}
				return pow_int_mod(all_digits.front(),others,TEN_PRIME,false);
			}
		}

		int better_last_digit(list<int> array) {
			int64_t p = 1;
			auto it = array.rbegin();
			while (it != array.rend()) {
				int a = p >= 4 ? 4 + (p % 4) : p;
				int b = *it >= 20 ? 20 + (*it % 20) : *it;
				p = pow(b, a);
				it++;
			}
			return p % 10;
		}
	}
}

#endif
