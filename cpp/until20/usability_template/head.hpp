#pragma once
#include <vector>
#include <cstddef>

template <typename T>
size_t size_of_t()
{
	return sizeof(T);
}

size_t size_of_int();