#ifndef CODEWARS_PATH_FINDER3_THE_ALPINIST_HPP
#define CODWEARS_PATH_FINDER3_THE_ALPINIST_HPP

#include <vector>
#include <string>
#include <cstdint>
#include <cmath>
#include <cassert>
#include <unordered_map>
#include <unordered_set>
#include <limits>
#include <algorithm>
#include <queue>

namespace codewars
{
	namespace threekyu
	{
		namespace path_finder3_worse
		{
			using namespace std;

			vector<pair<int32_t, int32_t>> directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};

			using position_index_t = int32_t;

			int32_t inline try_parse_char_to_digit(const char source)
			{
				constexpr int32_t ZERO_DIGIT = static_cast<int32_t>('0');
				const auto res = static_cast<int32_t>(source) - ZERO_DIGIT;
				assert(res < 10 && res >= 0);
				return res;
			}

			struct ClimbNode
			{
				int32_t x;
				int32_t y;
				int32_t cost;
				int32_t altitude;
				friend bool operator<(const ClimbNode&, const ClimbNode&);
			};

			bool operator<(const ClimbNode& lhs, const ClimbNode& rhs)
			{
				return lhs.cost < rhs.cost;
			}

			class UnionFind
			{
			private:
				vector<position_index_t> id;
				int32_t count;
				vector<size_t> tree_size;
			public:

				UnionFind(int32_t size):count{size},id{},tree_size{}
				{
					for(auto i=0;i<size;i++)
					{
						id.push_back(i);
						tree_size.push_back(1);
					}
				}
				bool connected(position_index_t p,position_index_t q)
				{
					return find(p) == find(q);
				}
				position_index_t find(position_index_t p)
				{
					while(p!=id[p])
					{
						p = id[p];
					}
					return p;
				}
				void to_union(position_index_t p,position_index_t q)
				{
					const auto p_root = find(p);
					const auto q_root = find(q);
					if (p_root == q_root) return;
					if (tree_size[p_root]<tree_size[q_root])
					{
						id[p_root] = q_root;
						tree_size[q_root] += tree_size[p_root];
					}
					else
					{
						id[q_root] = p_root;
						tree_size[p_root] += tree_size[q_root];
					}
					count--;
				}

				void flatten()
				{
					for(auto i=0;i<id.size();i++)
					{
						const auto root = find(i);
						if(root!=i)
						{
							tree_size[i] = 1;
							id[i] = root;
						}
					}
				}
			};

			int32_t path_finder(const std::string& literal_maze)
			{
				const auto size = static_cast<int32_t>(floor(sqrt(literal_maze.size())));
				assert(size > 0);
				if(size==1)
				{
					return 0;
				}
				using position_index_t = int32_t;

				const auto position_to_index = [size](int32_t x, int32_t y) -> position_index_t { return x * (size)+y; };

				// initialize maze matrix
				auto maze = unordered_map<position_index_t, ClimbNode>{};
				for (auto i = 0; i < size; i++)
				{
					for (auto j = 0; j < size; j++)
					{
						maze[position_to_index(i, j)] = ClimbNode{ i,j,numeric_limits<int32_t>::max(),try_parse_char_to_digit(literal_maze[i * (size + 1) + j]) };
					}
				}
				position_index_t entry_index = position_to_index(0, 0);
				position_index_t exit_index = position_to_index(size - 1, size - 1);

				auto known_nodes = unordered_set<position_index_t>{entry_index};
				auto unknown_nodes = unordered_set<position_index_t>();
				auto neighbors = unordered_map<position_index_t,unordered_map<position_index_t,int32_t>>{};
				auto unions = UnionFind{ size*size };

				for (auto curr = maze.begin();curr!=maze.end();++curr)
				{
						const auto current_key = curr->first;
						const ClimbNode &current_grid = curr->second;
						neighbors[current_key] = {};
						for(const auto &dir: directions)
						{
							const auto next_x = dir.first + current_grid.x;
							const auto next_y = dir.second + current_grid.y;
							if(next_x>=0&&next_y>=0&&next_x<size&&next_y<size)
							{
								const auto next_key = position_to_index(next_x, next_y);
								const ClimbNode &next_grid = maze[next_key];
								const auto altitude_round = abs(current_grid.altitude - next_grid.altitude);
								if(altitude_round==0)
								{
									unions.to_union(current_key, next_key);
								}
								neighbors[current_key][next_key] = altitude_round;
							}
						}
				}
				// unions.flatten();
				for(auto curr_ptr = neighbors.begin();curr_ptr!=neighbors.end();)
				{
					const auto curr_key = curr_ptr->first;
					const auto curr_neis = curr_ptr->second;
					const auto root_key = unions.find(curr_key);
					for (auto nei_ptr = curr_neis.begin(); nei_ptr != curr_neis.end(); ++nei_ptr)
					{
						const auto nei_key = nei_ptr->first;
						const auto nei_root_key = unions.find(nei_key);
						if(neighbors[root_key].find(nei_root_key)!= neighbors[root_key].end())
						{
							neighbors[root_key][nei_root_key] = min(neighbors[root_key][nei_root_key], nei_ptr->second);
						}
						else
						{
							neighbors[root_key][nei_root_key] = nei_ptr->second;
						}
					}
					if(root_key!=curr_key)
					{
						neighbors.erase(curr_ptr++);
					}
					else
					{
						++curr_ptr;
					}
				}
				for (auto curr_ptr = neighbors.begin(); curr_ptr != neighbors.end(); ++curr_ptr)
				{
					const auto curr_key = curr_ptr->first;
					auto &curr_neis = curr_ptr->second;
					for (auto nei_ptr = curr_neis.begin(); nei_ptr != curr_neis.end(); )
					{
						const auto nei_key = nei_ptr->first;
						const auto nei_root_key = unions.find(nei_key);
						
						if(nei_root_key != nei_key||nei_root_key==curr_key)
						{
							curr_neis.erase(nei_ptr++);
						}
						else
						{
							++nei_ptr;
						}
					}
				}

				entry_index = unions.find(entry_index);
				exit_index = unions.find(exit_index);
				if(entry_index==exit_index)
				{
					return 0;
				}
				for(const auto &entry_neighbor:neighbors[entry_index])
				{
					maze[entry_neighbor.first].cost = entry_neighbor.second;
				}
				for (auto curr = maze.begin(); curr != maze.end(); ++curr)
				{
					if (curr->first != entry_index)
					{
						unknown_nodes.insert(curr->first);
					}
				}
				while(!unknown_nodes.empty())
				{
					const auto current_key = *min_element(unknown_nodes.cbegin(), unknown_nodes.cend(),
						[&maze](const auto&lhs, const auto&rhs) -> bool { return maze[lhs] < maze[rhs]; });
					unknown_nodes.erase(current_key);
					const auto &current_node = maze[current_key];
					if (current_key == exit_index)
					{
						return current_node.cost;
					}
					for(auto &neighbor: neighbors[current_key])
					{
						const auto neighbor_key = neighbor.first;
						if(unknown_nodes.find(neighbor_key) != unknown_nodes.end())
						{
							maze[neighbor_key].cost = min(maze[neighbor_key].cost, maze[current_key].cost + neighbor.second);
						}
					}
					known_nodes.insert(current_key);
				}
				// will never happen
				assert(false);
				return -1;
			}

		}

		namespace path_finder3_vec
		{
			using namespace std;

				int32_t path_finder(const string &literal_maze) {
					constexpr int32_t CELL_UNKNOWN = -2;
					constexpr int32_t CELL_EMPTY = 0;
					const auto size = static_cast<int32_t>(floor(sqrt(literal_maze.size())));
					assert(size > 0);
					if (size == 1)
					{
						return 0;
					}
					auto maze = vector<vector<int32_t>>(size,vector<int32_t>(size,0));
					auto field = vector<vector<int32_t>>(size, vector<int32_t>(size, 0));
					for(auto i=0;i<size;i++)
					{
						for(auto j=0;j<size;j++)
						{
							maze[i][j] = literal_maze[i*(size + 1) + j];
						}
					}
					bool made_change;
					field[0][0] = 1; // avoid be see as EMPTY
					const auto get_value = [=,&field](int32_t i, int32_t j) -> int32_t {
						if (i >= 0 && j >= 0 && i < size  && j < size) {
							return field[i][j];
						}
						return CELL_UNKNOWN;
					};
					const auto update_value = [=,&get_value,&maze,&field,&made_change](int32_t i, int32_t j, int32_t delta_i, int32_t delta_j) -> void {
						const auto old_value = get_value(i + delta_i, j + delta_j);
						if (old_value < 0) {
							return;
						}

						const auto prev_altitude = maze[i][j];
						const auto curr_altitude = maze[i + delta_i][j + delta_j];

						const auto new_value = get_value(i, j) + abs(prev_altitude - curr_altitude);

						if (old_value == CELL_EMPTY || new_value < old_value) {
							field[i + delta_i][j + delta_j] = new_value;
							made_change = true;
						}
					};
					do {
						made_change = false;
						for (auto ii = 0; ii < size; ii++) {
							for (auto jj = 0; jj < size; jj++) {
								const auto value = get_value(ii, jj);
								if (value > 0) {
									update_value(ii, jj, 1, 0);
									update_value(ii, jj, 0, 1);
									update_value(ii, jj, -1, 0);
									update_value(ii, jj, 0, -1);
								}
							}
						}
					} while (made_change);

					if (field[size - 1][size - 1] > 0) {
						return field[size - 1][size - 1] - 1;
					}

					return -1;
				}
		}

		namespace path_finder3
		{
			using namespace std;

			using node_id_t = int32_t;

			vector<pair<int32_t, int32_t>> directions = { {0, 1}, {1, 0}, {0, -1}, {-1, 0} };

			struct node
			{
				node_id_t next;
				int32_t cost;
				friend  bool operator<(const node&, const node&);
			};

			bool operator<(const node& lhs,const node &rhs)
			{
				return lhs.cost > rhs.cost;
			}



			int32_t path_finder(const string &literal_maze)
			{
				const auto size = static_cast<int32_t>(floor(sqrt(literal_maze.size())));
				const auto point_size = size * size;

				const auto point_to_id = [size](const int32_t x, const int32_t y) -> node_id_t { return x * size + y;  };
				const auto point_to_literal_id = [size](const int32_t x,const int32_t y) -> node_id_t { return x * (size + 1) + y;  };

				vector<vector<node>> edge(point_size, vector<node>{});
				for(auto x=0;x<size;x++)
				{
					for(auto y=0;y<size;y++)
					{
						for(const auto &dir: directions)
						{
							const auto next_x = x + dir.first;
							const auto next_y = y + dir.second;
							if(next_x>=0&&next_y>=0&&next_x<size&&next_y<size)
							{
								edge[point_to_id(x, y)]
								.push_back(
									node{
										point_to_id(next_x,next_y),
										abs(literal_maze[point_to_literal_id(x,y)]-literal_maze[point_to_literal_id(next_x,next_y)])
									}
								);
							}
						}
					}
				}

				const node_id_t entry = 0;

				vector<int32_t> dist(point_size, -1);
				dist[entry] = 0;

				priority_queue<node> unknown{};
				unknown.push(node{ entry,dist[entry] });

				while(!unknown.empty())
				{
					auto current = unknown.top();
					unknown.pop();
					node_id_t u = current.next;
					if(u == point_size-1)
					{
						return dist[u];
					}
					for (auto i = 0; i < edge[u].size(); i++)
					{
						const auto v = edge[u][i].next;
						const auto cost = edge[u][i].cost;
						if (dist[v] == -1 || dist[u] + cost < dist[v])
						{
							dist[v] = dist[u] + cost;
							current.cost = dist[v];
							current.next = v;
							unknown.push(current);
						}
					}
				}
				return dist[point_size - 1];
			}
		}
	}
}

#endif
