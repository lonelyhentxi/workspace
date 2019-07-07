#ifndef CODEWARS_SUM_STRINGS_HPP
#define CODEWARS_SUM_STRINGS_HPP

#include <string>
#include <vector>
#include <cstdint>
#include <sstream>
#include <algorithm>
#include <cmath>
#include <iomanip>

namespace codewars
{
	namespace fourkyu
	{
		using namespace std;

		class BigPositiveInteger
		{
		public:
			friend BigPositiveInteger operator+(const BigPositiveInteger& lhs, const BigPositiveInteger& rhs);

			BigPositiveInteger() noexcept = delete;

			BigPositiveInteger(const BigPositiveInteger& rhs) = default;

			BigPositiveInteger(BigPositiveInteger&& rhs) noexcept = default;

			BigPositiveInteger& operator=(const BigPositiveInteger& rhs) = default;

			BigPositiveInteger& operator=(BigPositiveInteger&& rhs) noexcept = default;

			~BigPositiveInteger() noexcept = default;

			static BigPositiveInteger from_string(const string& a)
			{
				return BigPositiveInteger{a};
			}

			string to_string()
			{
				stringstream ss{};
				ss << inners[size() - 1];
				for (int64_t i = size() - 2; i >= 0; i--)
				{
					ss << setfill('0') << setw(TRUNK_SIZE) << inners[i];
				}
				string s{};
				ss >> s;
				// you do not need the following part except return
				// becase this version of compiler of codewars has a bug (maybe gcc?)
				// that following setw will affect the first one output
				reverse(s.begin(), s.end());
				while (s.back() == '0') {
					s.pop_back();
				}
				if (s == "") {
					s = "0";
				}
				reverse(s.begin(), s.end());
				return s;
			}

			size_t size() const
			{
				return inners.size();
			}

		private:
			explicit BigPositiveInteger(string a)
			{
				if (a.empty())
				{
					a = string{"0"};
				}
				const auto trunks_num = cal_trunks_num(a);
				inners = vector<uint32_t>(trunks_num, 0);
				reverse(a.begin(), a.end());
				for (auto i = a.size(); i < trunks_num * TRUNK_SIZE; i++)
				{
					a.push_back('0');
				}
				reverse(a.begin(), a.end());
				for (auto i = 0; i < size(); i++)
				{
					const size_t start = i * TRUNK_SIZE;
					stringstream trunk_stream;
					trunk_stream.write(a.c_str() + start, TRUNK_SIZE);
					trunk_stream >> inners[size() - 1 - i];
				}
			}

			vector<uint32_t> inners;

			static size_t cal_trunks_num(const string& a)
			{
				return static_cast<size_t>(ceil(static_cast<float>(a.size()) / TRUNK_SIZE));
			}

			const static uint32_t TRUNK_SIZE;
			const static uint32_t TRUNK_MAX;
		};


		const uint32_t BigPositiveInteger::TRUNK_SIZE = static_cast<uint32_t>(
			log10(static_cast<uint32_t>((static_cast<uint64_t>(1) << 8 * sizeof(uint32_t)) - 1)));
		const uint32_t BigPositiveInteger::TRUNK_MAX = static_cast<uint32_t>(pow(10, TRUNK_SIZE));

		BigPositiveInteger operator+(const BigPositiveInteger& lhs, const BigPositiveInteger& rhs)
		{
			BigPositiveInteger res = lhs;
			for (auto i = res.size(); i < max(lhs.size(), rhs.size()) + 1; i++)
			{
				res.inners.push_back(0);
			}
			uint32_t carry = 0;
			for (auto i = 0u; i < rhs.size(); i++)
			{
				auto sum = carry + res.inners[i] + rhs.inners[i];
				if (sum >= BigPositiveInteger::TRUNK_MAX)
				{
					carry = 1;
					sum = sum - BigPositiveInteger::TRUNK_MAX;
				}
				else
				{
					carry = 0;
				}
				res.inners[i] = sum;
			}
			res.inners[rhs.size()] += carry;
			if (res.inners.back() == 0)
			{
				res.inners.pop_back();
			}
			return res;
		}

		string sum_strings(const string& a, const string& b)
		{
			const auto lhs = BigPositiveInteger::from_string(a);
			const auto rhs = BigPositiveInteger::from_string(b);
			return (lhs + rhs).to_string();
		}
	}
}

#endif //CODEWARS_SUM_STRINGS_HPP
