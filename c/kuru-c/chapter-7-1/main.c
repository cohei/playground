#include <stdio.h>

int main(void) {
  printf ("数字を入力: ");

  int n;
  scanf ("%d", &n);

  if (n == 10)
    printf ("入力値は 10 です。\n");
  else
    printf ("入力値 %d は 10 ではありません。\n", n);

  return 0;
}
