#pragma once
#include <vector>
#include <algorithm>

namespace enfw
{
	namespace ita
	{
		namespace dp
		{
			class cutting_pod
			{
			public:
				template<typename Iterator, typename T = long,
					typename A = std::vector<T>, typename B = std::vector<typename A::size_type> >
					static std::pair<A, B> with_method_cost_cutting_pod(Iterator begin, const typename A::size_type &size, const T &cost)
				{
					using size_type = typename A::size_type;
					using array_type = A;
					using value_type = T;
					using size_array_type = B;

					auto worth = array_type(size + 1);
					auto method = size_array_type(size + 1);
					worth[0] = 0;
					for (size_type i = 1; i <= size; ++i)
					{
						size_type j = 1;
						value_type current_q = *(begin + j) + worth[i - j] - cost;
						value_type q = current_q;
						size_type s = j;
						for (j = 2; j <= i; ++j)
						{
							current_q = *(begin + j) + worth[i - j] - cost;
							if (q < current_q)
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

				/**
				 * ita-15.1-3
				 */
				template<typename Iterator, typename T = long, typename A = std::vector<T>>
				static long with_cost_cutting_pod(Iterator begin, const typename A::size_type &size, const T &cost)
				{
					return cutting_pod::with_method_cost_cutting_pod(begin, size, cost).first[size];
				}
			};
		}
	}
}