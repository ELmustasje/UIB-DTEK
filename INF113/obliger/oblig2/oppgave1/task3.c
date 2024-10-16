#include <stdio.h>
#include <stdlib.h>

void func1();
void func2();
void func3();
void func4();
int main(void) {
  func1();
  printf("we are at: %p\n", (void *)func1);
  func2();
  printf("we are at: %p\n", (void *)func2);
  func3();
  printf("we are at: %p\n", (void *)func3);
  func4();
  printf("we are at: %p\n", (void *)func4);
  return 0;
}
void func1() { printf("func1\n"); }
void func2() { printf("func2\n"); }
void func3() { printf("func3\n"); }
void func4() { printf("func4\n"); }
