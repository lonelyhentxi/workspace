#include "head.hpp"
#include <string>
#include <iostream>

extern template size_t size_of_t<int>();

int size_of_int_to_int()
{
	return static_cast<int>(size_of_t<int>());
}

template <typename T, typename U>
class MagicType
{
public:
	T dark;
	U magic;
};

typedef int (*process)(void*);
using NewProcess = int(*)(void*);

template<typename T>
using TrueDarkMagic = MagicType<std::vector<T>, std::string>;

template<typename T = int,typename U = int>
auto add(T x, U y) -> decltype(x + y)
{
	return x + y;
}

template<typename Require, typename...Args> class Magic;

template <typename... Ts>
void magic(Ts...args)
{
	// get number of args
	std::cout << sizeof...(args) << std::endl;
}

template <typename T0>
void printf1(T0 value)
{
	std::cout << value << std::endl;
}

template <typename T, typename ...Ts>
void printf1(T value, Ts ...args)
{
	std::cout << value << std::endl;
	printf1(args...);
}

template <typename T0, typename ...Ts>
auto printf2(T0 t0, Ts... t)
{
	std::cout << t0 << std::endl;
	if constexpr (sizeof...(t) > 0) printf2(t ...);
}

template<typename T, typename ...Ts>
auto printf3(T value, Ts ...args)
{
	std::cout << value << std::endl;
	(void)std::initializer_list<T>{([&args]
		{
			std::cout << args << std::endl;
		}(), value)...};
}

template <typename ...T>
auto sum(T ...t)
{
	return (t + ...);
}

template <typename T, int BufSize>
class buffer_t
{
public:
	T& alloc();
	void free(T& item);
private:
	T data[BufSize];
};

template <auto value> void foo()
{
	std::cout << value << std::endl;
	return;
}

template<typename ...Ts>
auto average(Ts...t)
{
	return (t + ...) / sizeof...(t);
}

int main()
{
	std::cout << size_of_int() << std::endl;
	std::cout << size_of_int_to_int() << std::endl;
	TrueDarkMagic<bool> you;
	magic();
	magic(1);
	magic(1, "");

	printf1(1, 2, "123", 1.1);
	printf2(1, 2, "123", 1.1);
	printf3(1, 2, "123", 1.1);
	std::cout << sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) << std::endl;
	buffer_t<int, 100> buf;
	foo<10>();
	std::cout << average(1, 2.3, 4, 5) << std::endl;
	return 0;
}