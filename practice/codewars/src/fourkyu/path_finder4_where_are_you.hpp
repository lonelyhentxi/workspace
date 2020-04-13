#ifndef CODEWARS_PATH_FINDER4_WHERE_ARE_YOU_HPP
#define CODEWARS_PATH_FINDER4_WHERE_ARE_YOU_HPP

#include <vector>
#include <algorithm>
#include <cctype>

namespace codewars
{
	namespace fourkyu
	{
		namespace path_finder4
		{
			using namespace std;

			int dir = 0;
			std::vector<int> pos = { 0, 0 };

			std::vector<int> i_am_here(std::string path)
			{
				for (auto it = path.begin(); it != path.end(); ++it) {
					switch (*it) {
					case 'r': dir += 1; break;
					case 'R': dir += 2; break;
					case 'l': dir -= 1; break;
					case 'L': dir -= 2; break;
					default: {
						auto it2 = std::find_if(it, path.end(), isalpha);
						const int step = std::stoi(std::string(it, it2));
						it = it2 - 1;
						switch ((dir % 4 + 4) % 4) {
						case 0: pos[0] -= step; break;
						case 1: pos[1] += step; break;
						case 2: pos[0] += step; break;
						case 3: pos[1] -= step; break;
						}
					}
					}
				}
				return pos;
			}
		}
	}
}

#endif // !CODEWARS_PATH_FINDER4_WHERE_ARE_YOU_HPP