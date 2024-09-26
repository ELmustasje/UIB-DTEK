#include <stdio.h>
#include <stdlib.h>

int sum_file(const char *filepath) {
  FILE *file = fopen(filepath, "r");
  if (!file) {
    printf("Could not open file %s\n", filepath);
    return -1;
  }

  char buffer[256];
  int sum = 0;
  while (fgets(buffer, sizeof(buffer), file)) {
    sum += atoi(buffer);
  }

  fclose(file);
  return sum;
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: %s <filepath>\n", argv[0]);
    return 1;
  }

  int sum = sum_file(argv[1]);
  printf("Sum: %d\n", sum);

  return 0;
}
