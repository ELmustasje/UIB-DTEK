#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void max_single_malloc() {
  size_t size = 1024 * 1024 * 1024;
  void *ptr = NULL;
  printf("Allocationg larger and larger blocks\n");

  while ((ptr = malloc(size)) != NULL) {
    printf("Allocated %.2fGB succsessfully\n", size / 1024.0 / 1024.0 / 1024.0);
    free(ptr);
    size += 1024 * 1024 * 1024;
  }

  printf("Failed to allocate %.2fGB\n", size / 1024.0 / 1024.0 / 1024.0);
}

const size_t BLOCK_SIZE = 206 * 1024 * 1024;

void multiple_mallocs() {
  size_t total_allocated = 0;
  void *ptr = NULL;

  printf("Allocating blocks of %zu bytes\n", BLOCK_SIZE);
  while ((ptr = malloc(BLOCK_SIZE)) != NULL) {

    total_allocated += BLOCK_SIZE;
  }

  printf("Total allocated memory: %zu bytes (%.2f GB)\n", total_allocated,
         total_allocated / 1024.0 / 1024.0 / 1024.0);
}

void multiple_mallocs_with_write() {
  size_t total_allocated = 0;
  void *ptr = NULL;

  while ((ptr = malloc(BLOCK_SIZE)) != NULL) {
    FILE *f = fopen("heap_c.txt", "a");
    memset(ptr, 1, BLOCK_SIZE);
    total_allocated += BLOCK_SIZE;

    fprintf(f, "written to %.2f GB\n",
            total_allocated / 1024.0 / 1024.0 / 1024.0);
    fclose(f);
  }

  printf("Total allocated memory: %zu bytes (%.2f GB)\n", total_allocated,
         total_allocated / 1024.0 / 1024.0 / 1024.0);
}

int main() {
  max_single_malloc();
  return 0;
}
