#include <iostream>
#include <map>

class Base
{
public:
	int value1;
	int value2;
	Base()
	{
		value1 = 1;
	}
	Base(int value): Base()
	{
		value2 = value;
	}
};;

class SubClass: public Base
{
public:
	using Base::Base;
};

struct Base1
{
	virtual void foo(int);
};

struct SubClass1: Base1
{
	virtual void foo(int) override;
	// virtual void foo(float) override;
};

struct Base2
{
	virtual void foo() final;
};

struct SubClass2 final : Base2 {};

class Magic
{
public:
	Magic() = default;
	Magic& operator=(const Magic&) = delete;
	Magic(int magic_number);
};

enum class new_enum : unsigned int {
	value1,
	value2,
	value3 = 100,
	value4 = 100
};

template <typename T>
std::ostream& operator<<(typename std::enable_if<std::is_enum<T>::value, std::ostream>::type& stream, const T& e)
{
	return stream << static_cast<typename std::underlying_type<T>::type>(e);
}

template <typename Key, typename Value, typename F>
void update(std::map<Key, Value> &m, F foo)
{
	for (auto&& [k, v] : m) { v = foo(k); }
}

int main()
{
	Base b(2);
	std::cout << b.value2 << std::endl;
	std::cout << b.value2 << std::endl;
	SubClass s(3);
	std::cout << s.value1 << std::endl;
	std::cout << s.value2 << std::endl;
	if (new_enum::value3 == new_enum::value4)
	{
		std::cout << "new_enum::value3 == new_enum::value4" << std::endl;
	}
	std::cout << new_enum::value3 << std::endl;
	std::map<std::string, long long int> m{ {"a", 1 }, { "b", 2 }, { "c", 3 } };

	update(m, [](std::string key)
		{
			return std::hash<std::string>{}(key);
		});

	for (auto&& [key, value] : m)
		std::cout << key << ":" << value << std::endl;
}