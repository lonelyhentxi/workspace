#include <stdlib.h>
#include <stdio.h>

extern void rust_capitalize(char *);

int main() {
    char str[] = "hello world";
    rust_capitalize(str);
    printf("%s\n",str);
    return 0;
}