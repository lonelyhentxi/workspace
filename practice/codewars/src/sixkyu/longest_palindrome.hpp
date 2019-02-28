#ifndef CODEWARS_LONGEST_PALINDROME_HPP
#define CODEWARS_LONGEST_PALINDROME_HPP
#include <string>
#include <vector>
#include <algorithm>

namespace codewars
{
	namespace sixkyu
	{
		using namespace std;
		int longest_palindrome(const string &s)
		{
			string workstation = "$#";
			for(auto i=0;i<s.size();i++)
			{
				workstation += s[i];
				workstation += '#';
			}
			auto p = vector<int>(workstation.size(), 0);
			int id = 0, max=0, res_id = 0, res_max = 0;
			for(int i=0;i<workstation.size();i++)
			{
				p[i] = max > i ? min(p[2 * id - i], max - i) : 1;
				while(workstation[i+p[i]]==workstation[i-p[i]])
				{
					++p[i];
				}
				if(max<i+p[i])
				{
					max = i + p[i];
					id = i;
				}
				if (res_max < p[i])
				{
					res_max = p[i];
					res_id = i;
				}
			}
			return res_max - 1;
		}
	}
}

#endif