#ifndef CODEWARS_CHECK_IF_DIVISIBLE_BY_0B111_HPP
#define CODEWARS_CHECK_IF_DIVISIBLE_BY_0B111_HPP

#include <regex>
#include <unordered_set>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <tuple>
#include <initializer_list>

namespace codewars
{
	namespace twokyu
	{
		using namespace std;

		pair<bool, string> one_accept_no_mid_dfa_to_regex(
			const size_t i,
			const unordered_map<size_t, tuple<unordered_set<size_t>, unordered_set<size_t>, bool>>& adjs,
			const unordered_map<size_t, unordered_map<size_t, string>>& links,
			const unordered_set<size_t>& accepts,
			const size_t start,
			const unordered_set<size_t>& disabled)
		{
			auto also_disabled = disabled;
			auto copy_adjs = adjs;
			auto copy_links = links;
			for (const auto j : accepts)
			{
				if (i != j && j != start)
				{
					auto _ins = get<1>(copy_adjs[j]);
					auto _outs = get<0>(copy_adjs[j]);
					auto circle = get<2>(copy_adjs[j]);
					for (const auto in : _ins)
					{
						if (also_disabled.find(in) == also_disabled.end())
						{
							for (const auto out : _outs)
							{
								if (also_disabled.find(out) == also_disabled.end())
								{
									auto temp_link =
										copy_links[in][i] + (circle ? "(" + copy_links[i][i] + ")" + "*" : "") +
										copy_links[i][out];
									auto new_link = (copy_links[in].find(out) == copy_links[in].end()
										                 ? temp_link
										                 : "(" + copy_links[in][out] + "|" + temp_link + ")");
									copy_links[in][out] = new_link;
									if (in == out)
									{
										get<2>(copy_adjs[in]) = true;
									}
									else
									{
										get<0>(copy_adjs[in]).insert(out);
										get<1>(copy_adjs[out]).insert(in);
									}
								}
							}
						}
					}
					also_disabled.insert(j);
				}
			}
			if (i == start)
			{
				return make_pair(true, copy_links[i][i]);
			}
			else
			{
				return make_pair(false, copy_links[start][i] + "(" + copy_links[i][i] + ")" + "*");
			}
		}

		string dfa_to_regex(const vector<vector<size_t>>& transitions, const vector<char>& symbols, const size_t start,
		                    const unordered_set<size_t>& accepts)
		{
			auto med_symbols = vector<size_t>{};
			auto adjs = unordered_map<size_t, tuple<
				                          unordered_set<size_t>,
				                          unordered_set<size_t>,
				                          bool>>{};
			auto disabled = unordered_set<size_t>{};
			auto links = unordered_map<size_t, unordered_map<size_t, string>>{};
			for (size_t i = 0; i < transitions.size(); i++)
			{
				// assert(transitions[i].size() == symbols.size());
				if (i != start && accepts.find(i) == accepts.end())
				{
					med_symbols.push_back(i);
				}
				links[i] = {};
				adjs[i] = {};
			}
			for (size_t i = 0; i < transitions.size(); i++)
			{
				for (size_t j = 0; j < symbols.size(); j++)
				{
					size_t adj = transitions[i][j];
					if (i == adj)
					{
						get<2>(adjs[i]) = true;
					}
					else
					{
						get<0>(adjs[i]).insert(adj);
						get<1>(adjs[adj]).insert(i);
					}
					links[i][adj] = {symbols[j]};
				}
			}
			for (const auto i : med_symbols)
			{
				auto _ins = get<1>(adjs[i]);
				auto _outs = get<0>(adjs[i]);
				auto circle = get<2>(adjs[i]);
				for (const auto in : _ins)
				{
					if (disabled.find(in) == disabled.end())
					{
						for (const auto out : _outs)
						{
							if (disabled.find(out) == disabled.end())
							{
								auto temp_link =
									links[in][i] + (circle ? "(" + links[i][i] + ")" + "*" : "") + links[i][out];
								auto new_link = (links[in].find(out) == links[in].end()
									                 ? temp_link
									                 : "(" + links[in][out] +
									                 "|" + temp_link +
									                 ")");
								links[in][out] = new_link;
								if (in == out)
								{
									get<2>(adjs[in]) = true;
								}
								else
								{
									get<0>(adjs[in]).insert(out);
									get<1>(adjs[out]).insert(in);
								}
							}
						}
					}
				}
				disabled.insert(i);
			}
			auto start_circle = links[start].find(start) != links[start].end() ? "(" + links[start][start] + ")*" : "";
			auto back_switch = string{"("};
			for (const auto i : accepts)
			{
				auto one_regex = one_accept_no_mid_dfa_to_regex(i, adjs, links, accepts, start, disabled);
				if (one_regex.first)
				{
					start_circle = "(" + one_regex.second + ")*";
				}
				else
				{
					back_switch += one_regex.second + "|";
				}
			}
			if (back_switch.back() == '|')
			{
				back_switch.pop_back();
			}
			back_switch.push_back(')');
			return start_circle + back_switch;
		}
	}
}

#endif/**/ // CODEWARS_CHECK_IF_DIVISIBLE_BY_0B111_HPP/**/
