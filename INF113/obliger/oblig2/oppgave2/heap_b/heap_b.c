#include <stdio.h>
#include <stdlib.h>

int main(void) {
  printf("running\n");
  int *m = malloc(1024 * 1024 * 2);

  printf("malloc gives address: %p\n", m);

  for (int i = 0; i < 4; i++) {
    printf("in the space before, i = %i, value: %i\n", i, *&m[-i]);
  }

  for (int i = 0; i < sizeof(int) * 1024; i++) {
    m[i] = 10;
  }

  getchar();
  return 0;
}
