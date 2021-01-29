export module Foo;

#define ANSWER 42

namespace Bar
{
	int f_internal() {
		return ANSWER;
	}

	export int f() {
		return f_internal();
	}
}