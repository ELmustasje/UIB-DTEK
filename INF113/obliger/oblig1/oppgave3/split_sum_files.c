#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

#define BUFFER_SIZE 256

// Function Prototypes
int sumOfSubArr(int start, int end, int arr[]);
int count_lines(const char *filepath);
int split_sum_file(const char *filepath, int n);
int *fileToArr(const char *filepath, int size);
void writeIntToFile(const char *filepath, int value);
int getValueFromFile(const char *filepath);

// Main Function
int main(int argc, char *argv[]) {
  if (argc != 3) {
    printf("Usage: %s <filepath> <n_split>\n", argv[0]);
    return 1;
  }

  int n = atoi(argv[2]);
  if (n <= 0) {
    printf("Sum: 0\n");
    return 0;
  }

  int sum = split_sum_file(argv[1], n);
  printf("Sum: %d\n", sum);
  return 0;
}

// Splits the workload among n child processes to compute the sum
int split_sum_file(const char *filepath, int n) {
  int id;
  int linesInFile = count_lines(filepath);
  if (linesInFile <= 0) {
    return 0; // Return 0 if there is an error counting lines or file is empty
  }

  int *intArr = fileToArr(filepath, linesInFile);
  if (intArr == NULL) {
    return 0; // Return 0 if memory allocation failed
  }

  int linesPrChild = linesInFile / n;
  int remainingLines = linesInFile % n;

  for (int i = 0; i < n; i++) {
    id = fork();
    if (id == 0) {
      // Child process
      int start = i * linesPrChild;
      int end = start + linesPrChild - 1 + (i == n - 1 ? remainingLines : 0);
      char childFile[BUFFER_SIZE];
      snprintf(childFile, sizeof(childFile), "partial_sum_%d.txt", i + 1);
      int sum = sumOfSubArr(start, end, intArr);
      writeIntToFile(childFile, sum);
      free(intArr); // Each child should free memory when done
      exit(0);
    }
  }

  // Parent process
  int sum = 0;
  for (int i = 0; i < n; i++) {
    wait(NULL);
  }

  for (int i = 0; i < n; i++) {
    char childFile[BUFFER_SIZE];
    snprintf(childFile, sizeof(childFile), "partial_sum_%d.txt", i + 1);
    sum += getValueFromFile(childFile);
  }

  free(intArr); // Free memory in parent process
  return sum;
}

// Counts the lines in the file
int count_lines(const char *filepath) {
  FILE *file = fopen(filepath, "r");
  if (!file) {
    printf("Error opening file for reading\n");
    return -1;
  }

  int count = 0;
  char buffer[BUFFER_SIZE];
  while (fgets(buffer, sizeof(buffer), file)) {
    count++;
  }

  fclose(file);
  return count;
}

// Reads the file into an integer array
int *fileToArr(const char *filepath, int size) {
  FILE *file = fopen(filepath, "r");
  if (!file) {
    printf("Error opening file for reading\n");
    return NULL;
  }

  int *arr = malloc(sizeof(int) * size);
  if (!arr) {
    printf("Memory allocation failed\n");
    fclose(file);
    return NULL;
  }

  char buffer[BUFFER_SIZE];
  for (int i = 0; i < size && fgets(buffer, sizeof(buffer), file); i++) {
    arr[i] = atoi(buffer);
  }

  fclose(file);
  return arr;
}

// Writes an integer value to a file
void writeIntToFile(const char *filepath, int value) {
  FILE *file = fopen(filepath, "w");
  if (!file) {
    printf("Error opening file for writing\n");
    return;
  }
  fprintf(file, "%d", value);
  fclose(file);
}

// Reads an integer value from a file
int getValueFromFile(const char *filepath) {
  FILE *file = fopen(filepath, "r");
  if (!file) {
    printf("Could not open file\n");
    return 0; // Returning 0 if file cannot be opened
  }

  char buffer[BUFFER_SIZE];
  if (!fgets(buffer, sizeof(buffer), file)) {
    fclose(file);
    return 0; // Returning 0 if no data can be read
  }

  fclose(file);
  return atoi(buffer);
}

// Calculates the sum of a subarray
int sumOfSubArr(int start, int end, int arr[]) {
  int sum = 0;
  for (int i = start; i <= end; i++) {
    sum += arr[i];
  }
  return sum;
}
