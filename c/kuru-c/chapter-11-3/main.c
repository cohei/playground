#include <stdio.h>

int sum (int, int);

int main (void) {
  printf ("%d\n", sum (50, 100));
  return 0;
}

int sum (int from, int to) {
  return (from + to) * (to - from + 1) / 2;
}
