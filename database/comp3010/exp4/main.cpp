#include <iostream>
#include <cstdio>
#include "extmem.h"

int main() {
    int a = 40;
    const auto b = static_cast<unsigned char>(a);
    std::printf("%c\n",b);
    std::cout << b;
    return 0;
}