#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include <sys/time.h>

typedef struct {
  int32_t n;
  double x;
  double y;
} Point;

Point arrayofpoints_sum(Point* points, size_t n) {
  Point sum = { 0, 0, 0 };

  for (size_t i = 0; i < n; i++) {
    if ((points[i].n & 1) == 0) {
      sum.x += points[i].x;
      sum.y += points[i].y;
    }
  }

  return sum;
}

Point* arrayofpoints_create(size_t n) {
  Point* points = (Point*) malloc(n * sizeof(Point));
  for (size_t i = 0; i < n; i++) {
    points[i].n = i;
    points[i].x = i * 0.1 + 0.1;
    points[i].y = i * 0.9 - 0.1;
  }
  return points;
}

void arrayofpoints_free(Point* array) {
  free(array);
}

static uint64_t now() {
  struct timeval t;
  gettimeofday(&t, NULL);
  return ((uint64_t) t.tv_sec) * 1000000 + (uint64_t) t.tv_usec;
}

void test_arrayofpoints() {
  static const size_t kArraySize = 10000;
  static const size_t kIterations = 10000;

  uint64_t create_total = 0;
  uint64_t sum_total = 0;

  for (size_t i = 0; i < kIterations; i++) {
    uint64_t t1 = now();
    Point* array = arrayofpoints_create(kArraySize);
    uint64_t t2 = now();
    Point sum = arrayofpoints_sum(array, kArraySize);
    uint64_t t3 = now();
    assert((int)sum.x == 2500000);
    assert((int)sum.y == 22495000);
    assert(sum.n == 0);
    arrayofpoints_free(array);

    create_total += (t2 - t1);
    sum_total += (t3 - t2);
  }

  printf("create: %lld [%.2f per iteration] usec, sum: %lld [%.2f per iteration] usec\n",
         create_total, (double)create_total / kIterations,
         sum_total, (double)sum_total / kIterations);
}

int main(int argc, char* argv[]) {
  test_arrayofpoints();
  return 0;
}
