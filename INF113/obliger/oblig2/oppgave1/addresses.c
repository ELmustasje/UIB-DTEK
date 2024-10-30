#include <stdio.h>
#include <stdlib.h>

int unInst_global_var;
int inst_global_var = 1;
const int const_global_var;

void f(int i) {
  FILE *file = fopen("addresses.txt", "a");

  if (i == 0)
    return;
  int stack_var;
  if (i == 1) {
    fprintf(file, "[%p] [stack]\n", (void *)&stack_var);
  } else {
    fprintf(file, "[%p] [stack]\n---------\n", (void *)&stack_var);
  }

  fclose(file);
  f(i - 1);
}

int main(void) {
  FILE *file = fopen("addresses.txt", "w");
  fprintf(file, "=========\n");

  fprintf(file, "[%p] [programkode]\n---------\n", (void *)&main);
  fprintf(file, "[%p] [global]\n---------\n", &unInst_global_var);
  fprintf(file, "[%p] [global]\n---------\n", &inst_global_var);
  fprintf(file, "[%p] [global]\n---------\n", &const_global_var);
  int *heap_var = malloc(1224 * 1224 * sizeof(int));
  heap_var[1] = 2;
  fprintf(file, "[%p] [heap]\n---------\n", heap_var);
  fprintf(file, "[%p] [heap]\n---------\n", &heap_var[1]);
  fclose(file);
  f(3);
  file = fopen("addresses.txt", "a");
  fprintf(file, "=========");
  fclose(file);
  free(heap_var);
  return 0;
}
