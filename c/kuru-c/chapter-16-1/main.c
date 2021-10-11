typedef struct {
  int grade;
  int class;
  int number;
  char name [64];
  double height;
  double weight;
} student;

int main (void) {
  student tanaka;

  tanaka.number = 10;
  strcpy (tanaka.name, "MARIO");

  printf ("%d\n", tanaka.number);
  printf("%s\n", tanaka.name);

  return 0;
}
