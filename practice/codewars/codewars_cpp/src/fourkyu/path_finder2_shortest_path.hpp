#ifndef CODEWARS_PATH_FINDER2_SHORTEST_PATH_HPP
#define CODEWARS_PATH_FINDER2_SHORTEST_PATH_HPP

#include <string>
#include <cmath>
#include <queue>
#include <utility>
#include <cassert>
#include <tuple>

namespace codewars
{
	namespace fourkyu
	{
		using namespace std;

		vector<pair<int32_t, int32_t>> path_finder2_directions = { {0, 1}, {0, -1}, {1, 0}, {-1, 0} };

		int32_t path_finder2(const string& literal_maze)
		{
			const size_t size = static_cast<size_t>(floor(sqrt(literal_maze.size())));
			assert(size > 0);
			auto maze = vector<string>(size, "");
			for (auto i = 0; i < size; i++)
			{
				for (auto j = 0; j < size; j++)
				{
					maze[i].push_back(literal_maze[i * (size + 1) + j]);
				}
			}
			auto remains = queue<tuple<int32_t, int32_t,int32_t>>{};
			remains.push({ 0, 0, 0 });
			while (!remains.empty())
			{
				const auto current = remains.front();
				remains.pop();
				for (const auto& dir : path_finder2_directions)
				{
					const auto next_x = dir.first + get<0>(current);
					const auto next_y = dir.second + get<1>(current);
					const auto next_cost = get<2>(current) + 1;
					if (next_x >= 0 && next_x < size && next_y >= 0 && next_y < size && maze[next_x][next_y] != 'W')
					{
						if (next_x == size - 1 && next_y == size - 1)
						{
							return next_cost;
						}
						else
						{
							maze[next_x][next_y] = 'W';
							remains.push(make_tuple(next_x, next_y, next_cost));
						}
					}
				}
			}
			return -1;
		}
	}
}

#endif
