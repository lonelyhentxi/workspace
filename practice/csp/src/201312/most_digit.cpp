#include "most_digit.hpp"
#include <cassert>
#ifdef NOW

int main() {
	int32_t size = 6;
	auto numbers = vector<int32_t>{10,1,10,20,30,20};
	int32_t expected = 10;
	assert(most_digit(size,numbers)==expected);
	return 0;
}

#endif
