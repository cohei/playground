#include <stdio.h>

int main (void) {
  int i;
  int *p = &i;

  printf ("p = %p\n", p);
  printf ("&i = %p\n", &i);

  *p = 10;

  printf ("*p = %d\n", *p);
  printf ("i = %d\n", i);

  return 0;
}
