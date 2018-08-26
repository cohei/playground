#include <stdio.h>

int max(int n1, int n2)
{
  if( n1 > n2 )
  {
    return n1;
  } else {
    return n2;
  }
}

struct maxn {
  int x;
  int y;
  int (*f) (int n1, int n2);
};

struct maxn m[5];

#define CALL(index) ( (*m[index].f)(m[index].x, m[index].y) )

int main(void)
{
  int i;

  for( i=0; i<5; ++i )
  {
    m[i].x = i;
    m[i].y = 5 - i;
    m[i].f = max;
  }

  for( i=0; i<5; ++i )
  {
    printf( "%d\n", CALL(i) );
  }

  return 0;
}
