#include <initializer_list>
#include <vector>
#include <iostream>

class MagicFoo
{
public:
	std::vector<int> vec;

	MagicFoo(std::initializer_list<int> list)
	{
		for (auto it = list.begin(); it != list.end(); ++it)
		{
			vec.push_back(*it);
		}
	}
};

template<typename T, typename U>
auto add2(T x, U y) -> decltype(x + y)
{
	return x + y;
}

template<typename T, typename U>
auto add3(T x, U y)
{
	return x + y;
}

int main()
{
	MagicFoo magic_foo = { 1, 2, 3, 4, 5 };
	std::cout << "magicFoo: ";
	for(auto it = magic_foo.vec.begin(); it != magic_foo.vec.end() ; ++ it)
	{
		std::cout << *it << ", ";
	}
	std::cout << std::endl;

	auto x = 1;
	auto y = 2;
	decltype(x + y) z;
	if (std::is_same<decltype(x), int>::value)
		std::cout << "type x == int" << std::endl;
	if (std::is_same<decltype(x), float>::value)
		std::cout << "type x == float" << std::endl;
	if (std::is_same<decltype(x), decltype(z)>::value)
		std::cout << "type z == type x" << std::endl;

	auto w = add2<int, double>(1, 2.0);
	if (std::is_same<decltype(w), double>::value)
	{
		std::cout << "w is double: ";
	}
	std::cout << w << std::endl;

	auto q = add3<double, int>(1.0, 2);
	std::cout << "q: " << q << std::endl;
	
	return 0;
}