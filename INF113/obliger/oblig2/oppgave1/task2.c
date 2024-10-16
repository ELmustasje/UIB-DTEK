#include <stdio.h>
#include <stdlib.h>

int main(void) {
  int *p = malloc(1000000);
  printf("%p\n", p);
  return 0;
}
