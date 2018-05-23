#pragma once

#include <utility>
#include <vector>
#include <functional>

namespace enfw
{
	namespace ita
	{
		namespace dp
		{
			class common_dp_sln
			{
			public:
				template<typename Iterator, typename A = std::vector<typename Iterator::value_type>, 
				typename B = std::vector<typename A::size_type>>
					static std::pair<A, B> common_linear_dp(Iterator begin, const typename A::size_type &size, 
						const typename Iterator::value_type &cost, 
						const std::function<bool(typename Iterator::value_type, typename Iterator::value_type)> &compare = std::less<typename Iterator::value_type>(),
						const std::function<typename Iterator::value_type(typename Iterator::value_type, typename Iterator::value_type)> &plus = std::plus<typename Iterator::value_type>(),
						const std::function<typename Iterator::value_type(typename Iterator::value_type, typename Iterator::value_type)> &mult = std::multiplies<typename Iterator::value_type>(),
						const std::function<typename Iterator::value_type(typename Iterator::value_type)> &neg = std::negate<typename Iterator::value_type>()
						)
				{
					using size_type = typename A::size_type;
					using array_type = A;
					using value_type = typename Iterator::value_type;
					using size_array_type = B;

					auto worth = array_type(size + 1);
					auto method = size_array_type(size + 1);
					worth[0] = value_type{};
					for (size_type i = 1; i <= size; ++i)
					{
						size_type j = 1;
						value_type current_q = plus(*(begin + j),plus(worth[i - j], neg(cost)));
						value_type q = current_q;
						size_type s = j;
						for (j = 2; j <= i; ++j)
						{
							current_q = plus(*(begin + j), plus(worth[i - j], neg(cost)));
							if (compare(q,current_q))
							{
								q = current_q;
								s = j;
							}
						}
						worth[i] = q;
						method[i] = s;
					}
					return std::make_pair(std::move(worth), std::move(method));
				}
			};
		}
	}
}
