#ifndef CODEWARS_A_CHAIN_ADDING_FUNCTION_HPP
#define CODEWARS_A_CHAIN_ADDING_FUNCTION_HPP

#include <iostream>

namespace codewars
{
	namespace fivekyu
	{
		using std::ostream;

		class add
		{
		private:
			int sum;
		public:
			add() :sum{ 0 } {}
			add(int init) :sum{ init } {}
			add operator() (int curr) const
			{
				return add{ curr + this->sum };
			}
			operator int() const {
				return this->sum;
			}
		};

		ostream &operator<<(ostream &os, const add &other) {
			os << static_cast<int>(other);
			return os;
		}
	}
}

#endif // CODEWARS_A_CHAIN_ADDING_FUNCTION_HPP
