#include <stdio.h>
void f(int i) {
  if (i == 0)
    return;

  // Din kode her
  int *j = &i;
  printf("%p\n", j);
  f(i - 1);
}
int main(int argc, char *argv[]) {
  f(10);
  return 0;
}
