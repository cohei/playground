#include <stdio.h>

int main (void) {
  FILE *file = fopen ("test.txt", "w");

  fprintf(file, "Hello, world!");

  fclose (file);

  return 0;
}
