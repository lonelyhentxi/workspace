#ifndef CODEWARS_PATH_FINDER1_CAN_YOU_REACH_THE_EXIT_HPP
#define CODEWARS_PATH_FINDER1_CAN_YOU_REACH_THE_EXIT_HPP

#include <string>
#include <cmath>
#include <queue>
#include <utility>
#include <cassert>

namespace codewars
{
	namespace fourkyu
	{
		using namespace std;

		vector<pair<int32_t, int32_t>> path_finder1_directions = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

		bool path_finder1(const string& literal_maze)
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
			auto remains = queue<pair<int32_t, int32_t>>{};
			remains.push({0, 0});
			while (!remains.empty())
			{
				const auto current = remains.front();
				remains.pop();
				for (const auto& dir : path_finder1_directions)
				{
					const auto next_x = dir.first + current.first;
					const auto next_y = dir.second + current.second;
					if (next_x >= 0 && next_x < size && next_y >= 0 && next_y < size && maze[next_x][next_y] != 'W')
					{
						if (next_x == size - 1 && next_y == size - 1)
						{
							return true;
						}
						else
						{
							maze[next_x][next_y] = 'W';
							remains.push(make_pair(next_x, next_y));
						}
					}
				}
			}
			return false;
		}
	}
}

#endif
