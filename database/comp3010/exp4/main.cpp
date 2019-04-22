#include <iostream>
<<<<<<< HEAD
#include <repl.hpp>
#include <unicode/utf8.h>

using std::cin;
using std::cout;

int main(int argc, char* argv[]) {
=======
#include <cstdio>
#include "extmem.h"

int main() {
    int a = 40;
    const auto b = static_cast<unsigned char>(a);
    std::printf("%c\n",b);
    std::cout << b;
    return 0;
>>>>>>> ca41912a34097351d76dcfb618c7f0ae3f39e17f
}