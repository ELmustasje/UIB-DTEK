#include <stdio.h>
#include <stdlib.h>

int main(void) {
  int a;

  printf("ikke inital: %p\n", &a);
  a = 4;
  printf("inital: %p\n", &a);
  return 0;
}
