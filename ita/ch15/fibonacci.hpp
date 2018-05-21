#pragma once
#include <vector>

namespace enfw
{
	namespace ita
	{
		namespace dp
		{
			/**
			* ita-15.1-5
			*/
			class fibonacci
			{
			public:

				template <typename T, typename A = std::vector<T>>
				T operator()(const T &n) const
				{
					auto memoization = A(n + 1);
					memoization[0] = 1;
					memoization[1] = 1;
					if (n == 0 || n == 1) return memoization[n];
					for (T i = 2; i <= n; ++i)
					{
						memoization[i] = memoization[i - 1] + memoization[i - 2];
					}
					return memoization[n];
				}
			};
		}
	}
}
