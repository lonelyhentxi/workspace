#pragma once
#include <functional>
#include <algorithm>
#include <boost/numeric/ublas/matrix.hpp>

namespace enfw
{
	namespace ita
	{
		namespace temp
		{
			using boost::numeric::ublas::matrix;

			class hill_climbing
			{
			public:
				matrix<long>::value_type optimal_solution_lower_bound(
					const matrix<long> &m,
					std::function<bool(const matrix<long>::value_type &, const matrix<long>::value_type &)> less
					= std::less<matrix<long>::value_type>{}
				)
				{
					using size_type = matrix<long>::size_type;
					using value_type = matrix<long>::value_type;
					using matrix_type = matrix<long>;
					matrix_type temp = m;
					matrix_type left = m;
					matrix_type right = m;
					value_type min;
					for(auto pi = m.begin1(); pi<m.end1() ; ++pi)
					{
					}
				}
			};
		}
	}
}