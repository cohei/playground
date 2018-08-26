#include <stdio.h>

int add(int n1, int n2);
int sub(int n1, int n2);
int mul(int n1, int n2);
int div(int n1, int n2);
int mod(int n1, int n2);

int main(void)
{
  int (*p[5]) (int n1, int n2) = { add, sub, mul, div, mod };
  int n1, n2;
  int i;

  puts( "enter two integers" );
  scanf ( "%d %d", &n1, &n2 );

  for(i=0; i<5; ++i)
  {
    printf( "%d\n", (*p[i])(n1, n2));
  }

  return 0;
}

int add(int n1, int n2)
{
  return ( n1 + n2 );
}

int sub(int n1, int n2)
{
  return ( n1 - n2 );
}

int mul(int n1, int n2)
{
  return ( n1 * n2 );
}

int div(int n1, int n2)
{
  if( n2 == 0){ return 0; }
  return ( n1 / n2 );
}

int mod(int n1, int n2)
{
  if( n2 == 0){ return 0; }
  return ( n1 % n2 );
}
