#include <iostream>
#include <utility>
#include <functional>
#include <vector>
#include <string>

#define let const auto

void lambda_value_capture()
{
	int value = 1;
	auto copy_value = [value]
	{
		return value;
	};
	value = 100;
	const auto stored_value = copy_value();
	std::cout << "stored_value = " << stored_value << std::endl;
}

void lambda_reference_capture()
{
	int value = 1;
	auto copy_value = [&value]
	{
		return value;
	};
	value = 100;
	let stored_value = copy_value();
	std::cout << "store_value = " << stored_value << std::endl;
}

using foo = void(int);
void functional(foo f)
{
	f(1);
}

int bar(int para)
{
	return para;
}

int foo2(int a, int b, int c)
{
	return 0;
}

void reference(std::string &str)
{
	std::cout << "left value" << std::endl;
}

void reference(std::string &&str)
{
	std::cout << "right value" << std::endl;
}

void reference_i(int& str)
{
	std::cout << "left value" << std::endl;
}

void reference_i(int&& str)
{
	std::cout << "right value" << std::endl;
}

class A
{
public:
	int* pointer;
	A(): pointer(new int(1))
	{
		std::cout << "construt" << pointer << std::endl;
	}
	A(A& a): pointer(new int(*a.pointer))
	{
		std::cout << "copy" << pointer << std::endl;
	}
	A(A&& a): pointer(a.pointer)
	{
		a.pointer = nullptr;
		std::cout << "move" << pointer << std::endl;
	}
	~A()
	{
		std::cout << "destruct" << pointer << std::endl;
		delete pointer;
	}
};

A return_rvalue(bool test)
{
	A a,b;
	if (test) { return a; }
	else { return b; }
}

template <typename T>
void pass(T&& v)
{
	std::cout << "common pass:";
	reference_i(v);
	std::cout << "move pass:";
	reference_i(std::move(v));
	std::cout << "forward pass:";
	reference_i(std::forward<T>(v));
	std::cout << "static_cast<T&&> pass:";
	reference_i(static_cast<T&&>(v));
}

int main()
{
	auto important = std::make_unique<int>(1);
	let add = [v1 = 1, v2 = std::move(important)](int x, int y) ->int
	{
		return x + y + v1 + (*v2);
	};
	std::cout << add(3, 4) << std::endl;

	let add1 = [](auto x, auto y)
	{
		return x + y;
	};

	std::cout << add(1, 2) << std::endl;
	std::cout << add(1.1, 2.2) << std::endl;

	let f = [](int value)
	{
		std::cout << value << std::endl;
	};
	functional(f);
	f(1);

	std::function<int(int)> func = bar;
	int important1 = 10;
	std::function<int(int)> func2 = [&](int value) -> int
	{
		return 1 + value + important1;
	};
	std::cout << func(10) << std::endl;
	std::cout << func2(10) << std::endl;

	let bindFoo = std::bind(foo2, std::placeholders::_1, 1, 2);
	bindFoo(1);

	std::string lv1 = "string,";
	// std::string&& r1  = lv1;
	std::string&& rv1 = std::move(lv1);
	std::cout << rv1 << std::endl;

	const std::string& lv2 = lv1 + lv1;
	// lv2 += "Test";
	std::cout << lv2 << std::endl;

	// although referencing a right value, it is a left value now
	std::string&& rv2 = lv1 + lv2;
	rv2 += "Test";
	std::cout << rv2 << std::endl;

	reference(rv2);

	// int &a = std::move(1);
	const int& b = std::move(1);
	std::cout << b << std::endl;

	A obj = return_rvalue(false);
	std::cout << "obj" << std::endl;
	std::cout << obj.pointer << std::endl;
	std::cout << *obj.pointer << std::endl;

	std::string str = "Hello world.";
	std::vector<std::string> v;

	v.push_back(str);
	std::cout << "str: " << str << std::endl;

	v.push_back(std::move(str));
	std::cout << "str: " << str << std::endl;

	std::cout << "pass right reference:" << std::endl;
	pass(1);
	
	std::cout << "pass left reference" << std::endl;
	int l = 1;
	pass(l);
	return 0;
}