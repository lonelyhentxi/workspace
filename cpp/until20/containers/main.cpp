#include <algorithm>
#include <vector>
#include <iostream>
#include <array>
#include <map>
#include <unordered_map>
#include <tuple>
#include <variant>

#define let const auto

template <typename T>
void repl(const T& t)
{
	std::cout << t << std::endl;
}

void try_with_vector()
{
	std::vector<int> v;
	std::cout << "size:" << v.size() << std::endl; // 输出 0
	std::cout << "capacity:" << v.capacity() << std::endl; // 输出 0

	v.push_back(1);
	v.push_back(2);
	v.push_back(3);
	std::cout << "size:" << v.size() << std::endl; // 输出 3
	std::cout << "capacity:" << v.capacity() << std::endl; // 输出 4

	v.push_back(4);
	v.push_back(5);
	std::cout << "size:" << v.size() << std::endl; // 输出 5
	std::cout << "capacity:" << v.capacity() << std::endl; // 输出 8

	v.clear();
	std::cout << "size:" << v.size() << std::endl; // 输出 0
	std::cout << "capacity:" << v.capacity() << std::endl; // 输出 8

	v.shrink_to_fit();
	std::cout << "size:" << v.size() << std::endl; // 输出 0
	std::cout << "capacity:" << v.capacity() << std::endl; // 输出 0
}

void try_with_array()
{
	std::array<int, 4> arr = {1, 2, 3, 4};
	repl(arr.empty());
	repl(arr.size());

	for (auto& i : arr)
	{
		i += 0;
	}

	std::sort(arr.begin(), arr.end(), [](const int a, const int b)
	{
		return b < a;
	});

	constexpr int len = 4;
	std::array<int, len> arr1 = {1, 2, 3, 4};
	{
		let foo = [](int* p, int len)
		{
		};
		std::array<int, 4> arr = {1, 2, 3, 4};
		foo(&arr[0], arr.size());
		foo(arr.data(), arr.size());

		std::sort(arr.begin(), arr.end());
	}
}

void try_with_unordered()
{
	std::unordered_map<int, std::string> u = {
		{1, "1"},
		{3, "3"},
		{2, "2"}
	};
	std::map<int, std::string> v = {
		{1, "1"},
		{3, "3"},
		{2, "2"}
	};

	repl("std::unordered_map");
	for (let & n : u)
		std::cout << "Key:[" << n.first << "] Value:[" << n.second << "]\n";

	repl("");
	repl("std::map");
	for (let & n : v)
	{
		std::cout << "Key:[" << n.first << "] Value:[" << n.second << "]\n";
	}
}

void try_with_tuple ()
{
	let get_student = [](int id)
	{
		if (id == 0)
		{
			return std::make_tuple(3.8, 'A', "张三");
		}
		if (id == 1)
			return std::make_tuple(2.9, 'C', "李四");
		if (id == 2)
			return std::make_tuple(1.7, 'D', "王五");
		return std::make_tuple(0.0, 'D', "null");
	};
	let student = get_student(0);
	std::cout << "ID: 0, "
		<< "GPA: " << std::get<0>(student) << ", "
		<< "成绩: " << std::get<1>(student) << ", "
		<< "姓名: " << std::get<2>(student) << std::endl;
	
	double gpa;
	char grade;
	std::string name;

	std::tie(gpa, grade, name) = get_student(1);
	std::cout << "ID: 1, "
		<< "GPA: " << gpa << ", "
		<< "成绩: " << grade << ", "
		<< "姓名: " << name << std::endl;

	std::tuple<std::string, double, double, int> t("123", 4.5, 6.7, 8);
	repl(std::get<std::string>(t));
	// repl(std::get<std::double>(t));
	repl(std::get<3>(t));
	auto new_tuple = std::tuple_cat(get_student(1), std::move(t));
}

template <size_t n, typename... T>
constexpr std::variant<T...> _tuple_index(const std::tuple<T...>& tpl, size_t i) {
	if constexpr (n >= sizeof...(T))
		throw std::out_of_range("out of range.");
	if (i == n)
		return std::variant<T...>{ std::in_place_index<n>, std::get<n>(tpl) };
	return _tuple_index<(n < sizeof...(T) - 1 ? n + 1 : 0)>(tpl, i);
}

template <typename... T>
constexpr std::variant<T...> tuple_index(const std::tuple<T...>& tpl, size_t i) {
	return _tuple_index<0>(tpl, i);
}

template <typename T0, typename ...Ts>
std::ostream &operator << (std::ostream & s, std::variant<T0, Ts...> const & v)
{
	std::visit([&](auto&& x) { s << x; }, v);
	return s;
}

template<typename T>
size_t tuple_len(T &tpl)
{
	return std::tuple_size<T>::value;
}

void try_with_variant()
{
	let t = std::tuple<std::string, double, double, int>{ "123", 4.5, 6.7, 8 };
	let index = 1;
	std::cout << tuple_index(t, index) << std::endl;
	for(size_t i = 0; i!=tuple_len(t); ++i)
	{
		std::cout << tuple_index(t, i) << std::endl;
	}
}

int main()
{
	try_with_vector();
	try_with_array();
	try_with_unordered();
	try_with_tuple();
	try_with_variant();
	return 0;
}
