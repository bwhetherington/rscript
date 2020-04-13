#include <limits.h>
#include <stdio.h>

typedef unsigned int uint_t;

int test1(int a) { return (!(a + a) & !!a); }

int test2(int a) { return !(!a | (a + a)); }

void printNum(char* label, int n) { printf("%s :: %d[%X]\n", label, n, n); }

int main(void) {
  // for (int x = 0; x < INT_MAX; x++) {
  //   printNum("x", x);
  //   printNum("test", test(x));
  // }

  printNum("test", test1(INT_MIN));

  // printf("test1 :: %d")
}