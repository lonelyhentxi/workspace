#include <iostream>
#include <memory>
#define let const auto

struct Foo {
	Foo() { std::cout << "Foo::Foo" << std::endl; }
	~Foo() { std::cout << "Foo::~Foo" << std::endl; }
	void foo() { std::cout << "Foo::foo" << std::endl; }
};

void f(const Foo&) {
	std::cout << "f(const Foo&)" << std::endl;
}

int main()
{
	{
		let foo = [](const std::shared_ptr<int>& i)
		{
			(*i)++;
		};
		let pointer = std::make_shared<int>(10);
		foo(pointer);
		std::cout << *pointer << std::endl;
	}
	{
		let pointer = std::make_shared<int>(10);
		auto pointer2 = pointer;
		auto pointer3 = pointer;
		int* p = pointer.get();
		std::cout << "pointer.use_count() = " << pointer.use_count() << std::endl; // 3
		std::cout << "pointer2.use_count() = " << pointer2.use_count() << std::endl; // 3
		std::cout << "pointer3.use_count() = " << pointer3.use_count() << std::endl; // 3

		pointer2.reset();
		std::cout << "reset pointer2:" << std::endl;
		std::cout << "pointer.use_count() = " << pointer.use_count() << std::endl; // 2
		std::cout << "pointer2.use_count() = " << pointer2.use_count() << std::endl; // 0, pointer2 ря reset
		std::cout << "pointer3.use_count() = " << pointer3.use_count() << std::endl; // 2

		pointer3.reset();
		std::cout << "reset pointer3:" << std::endl;
		std::cout << "pointer.use_count() = " << pointer.use_count() << std::endl; // 1
		std::cout << "pointer2.use_count() = " << pointer2.use_count() << std::endl; // 0
		std::cout << "pointer3.use_count() = " << pointer3.use_count() << std::endl; // 0, pointer3 ря reset
	}
	{
		std::unique_ptr<int> pointer = std::make_unique<int>(10);
		// std::unique_ptr<int> pointer2 = pointer;
	}
	{
		std::unique_ptr<Foo> p1(std::make_unique<Foo>());
		if (p1) p1->foo();
		{
			std::unique_ptr<Foo> p2(std::move(p1));
			f(*p2);
			if (p2) p2->foo();
			if (p1) p1->foo();
			p1 = std::move(p2);
			if (p2) p2->foo();
			std::cout << "p2 destruct" << std::endl;
		}
		if (p1) p1->foo();
	}
	return 0;
}