#include <iostream>
#include <vector>
#include <algorithm>
#include <initializer_list>
#include <tuple>

class Foo
{
public:
	int value_a;
	int value_b;
	Foo(int a, int b): value_a(a), value_b(b) {}
};

class MagicFoo
{
public:
	std::vector<int> vec;
	MagicFoo(std::initializer_list<int> list)
	{
		for(std::initializer_list<int>::iterator it = list.begin(); it != list.end(); ++it)
		{
			vec.push_back(*it);
		}
	}
};

std::tuple<int, double, std::string> f()
{
	return std::make_tuple(1, 2.3, "456");
}

int main()
{
	std::vector<int> vec = { 1, 2, 3, 4 };
	if (const std::vector<int>::iterator itr = std::find(vec.begin(), vec.end(), 3); itr != vec.end())
	{
		*itr = 4;
	}

	// before c++ 11
	int arr[3] = { 1, 2, 3 };
	Foo foo(1, 2);
	vec = { 1, 2, 3, 4, 5 };

	std::cout << "arr[0]: " << arr[0] << std::endl;
	std::cout << "foo: " << foo.value_a << ", " << foo.value_b << std::endl;

	for(std::vector<int>::const_iterator it = vec.begin(); it != vec.end(); ++ it)
	{
		std::cout << *it << std::endl;
	}

	MagicFoo magic_foo = { 1, 2, 3, 4, 5 };
	std::cout << "magicFoo: ";
	for(std::vector<int>::const_iterator it = magic_foo.vec.begin(); it != magic_foo.vec.end(); ++it)
	{
		std::cout << *it << std::endl;
	}

	auto [x, y, z] = f();
	std::cout << x << ", " << y << ", " << z << std::endl;
	return 0;
}