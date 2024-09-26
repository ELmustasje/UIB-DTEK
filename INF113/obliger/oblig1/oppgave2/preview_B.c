#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

/*
 * Function: preview
 * ----------------------------
 *   Opens a file and prints the first n lines of the file.
 *
 *   filepath: The path to the file to preview.
 *   n: The number of lines to print.
 *
 *   returns: void
 */
void preview(const char *filepath, int n) {
  // your code here

  FILE *file = fopen(filepath, "r");
  if (file == NULL) {
    printf("ERROR: cannot open file");
    fflush(stdout);
    return;
  };

  char buff[1024];
  int count = 0;
  while (fgets(buff, sizeof(buff), file) != NULL) {
    if (count >= n) {
      break;
    }
    printf("%s", buff);
    fflush(stdout);
    count++;
  }
  fclose(file);
}

int main(int argc, char *argv[]) {
  if (argc != 3 || access(argv[1], 0) != 0 || atoi(argv[2]) <= 0) {
    printf("Usage: %s <filepath> <output_lines>\noutput_lines should be "
           "integer and larger than 0.\n",
           argv[0]);
    fflush(stdout);
    return 1;
  }
  int n = atoi(argv[2]); // atoi() converts a string to an integer

  preview(argv[1], n);
  return 0;
}
