#ifndef CODEWARS_PARSEINT_PRELOAD_HPP
#define CODEWARS_PARSEINT_PRELOAD_HPP

#include <string>
#include <unordered_map>
#include <numeric>

namespace codewars
{
	namespace twokyu
	{
		using namespace std;

		unordered_map<string, long> parse_int_small_map =
		{
			{"zero", 0}, {"one", 1}, {"two", 2}, {"three", 3}, {"four", 4}, {"five", 5}, {"six", 6}, {"seven", 7},
			{"eight", 8}, {"nine", 9}, {"ten", 10},
			{"eleven", 11}, {"twelve", 12}, {"thirteen", 13}, {"fourteen", 14}, {"fifteen", 15}, {"sixteen", 16},
			{"seventeen", 17}, {"eighteen", 18}, {"nineteen", 19},
			{"twenty", 20}, {"thirty", 30}, {"forty", 40}, {"fifty", 50}, {"sixty", 60}, {"seventy", 70},
			{"eighty", 80}, {"ninety", 90},
		};
		unordered_map<string, long> parse_int_big_map = {
			{"hundred", 100}, {"thousand", 1000}, {"million", 1000000}
		};


		vector<string> split(const string & number, const string& delims)
		{
			auto res = vector<string>{};
			string::size_type beg_idx = number.find_first_not_of(delims);
			while (beg_idx != string::npos)
			{
				string::size_type end_idx = number.find_first_of(delims,beg_idx);
				if (end_idx == string::npos)
				{
					end_idx = number.length();
				}
				res.push_back(number.substr(beg_idx, end_idx - beg_idx));
				beg_idx = number.find_first_not_of(delims, end_idx);
			}
			return res;
		}

		long parse_int(std::string number)
		{
			const string delims{" -"};
			const vector<string> splits = split(number, delims);
			return 
			accumulate(splits.cbegin(), splits.cend(), 0,
			                  [](long acc,const string& word) -> long
			                  {
				                  const auto small_idx = parse_int_small_map.find(word);
				                  if (small_idx != parse_int_small_map.end())
				                  {
					                  acc += small_idx->second;
				                  }
				                  const auto big_idx = parse_int_big_map.find(word);
				                  if (big_idx != parse_int_big_map.end())
				                  {
					                  const auto big_num = big_idx->second;
					                  acc += (big_num) * (acc % big_num) - (acc % big_num);
				                  }
				                  return acc;
			                  });
		}
	}
}

#endif // CODEWARS_PARSEINT_PRELOAD_HPP
