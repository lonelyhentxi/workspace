#ifndef CODEWARS_LONGEST_PALINDROME_HPP
#define CODEWARS_LONGEST_PALINDROME_HPP
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>

namespace codewars
{
	namespace sixkyu
	{
		using namespace std;

		int longest_palindrome(const string& s)
		{
			auto t = string{"$#"};
			for (const auto& c : s)
			{
				t.push_back(c);
				t.push_back('#');
			}
			t.push_back('@');
			const int length = static_cast<int>(t.size());
			auto p = vector<int>(length, 0);
			auto r = 0;
			auto c = 0;
			for (auto i = 1; i < length - 1; i++)
			{
				if (i < r)
				{
					p[i] = min(p[2 * c - i], r - i);
				}
				while (t[i + (p[i] + 1)] == t[i - (p[i] + 1)])
				{
					p[i] += 1;
				}
				if (p[i] + i > r)
				{
					r = p[i] + i;
					c = i;
				}
			}
			return accumulate(p.cbegin(), p.cend(), 0, [](auto x, auto y) { return max(x, y); });
		}
	}
}

#endif
