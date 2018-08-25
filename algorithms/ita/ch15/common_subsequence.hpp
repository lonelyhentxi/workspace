#pragma once
#include "tiny_matrix.hpp"

namespace enfw
{
	namespace ita
	{
		namespace dp
		{
			class common_subsequence
			{
			public:
			    enum class direction { z, x, y, xy };

				template <typename Iterator, typename Length = unsigned,
					typename DirectionMatrix = ds::tiny_matrix<direction>,
					typename SizeMatrix = ds::tiny_matrix<Length>>
					static std::pair<DirectionMatrix, SizeMatrix> longest_common_subsequence_length(Iterator x,Iterator y,
						const Length &x_size,const Length &y_size)
				{
					using size_type = Length;
					auto directions = DirectionMatrix(x_size+1, y_size+1,direction::z);
					auto methods = SizeMatrix(x_size + 1, y_size + 1,0);
					for(size_type i=1;i<=x_size;++i)
					{
						methods.insert_element(i, 0, 0);
					}
					for(size_type j=0;j<=y_size;++j)
					{
						methods.insert_element(0, j, 0);
					}
					for(size_type i=1;i<=x_size;++i)
					{
						for(size_type j=1;j<=y_size;++j)
						{
							if(*(x+i)==*(y+j))
							{
								methods.insert_element(i,j,methods.at_element(i - 1, j - 1) + 1);
								directions.insert_element(i, j, direction::xy);
							}
							const auto &latest_x = methods.at_element(i - 1, j);
							const auto &latest_y = methods.at_element(i, j - 1);
							if(latest_x>=latest_y)
							{
								methods.insert_element(i, j, latest_x);
								directions.insert_element(i, j, direction::x);
							} else
							{
								methods.insert_element(i, j, latest_y);
								directions.insert_element(i, j, direction::y);
							}
						}
					}
					return std::make_pair(std::move(directions), std::move(methods));
				}
			};
		}
	}
}
