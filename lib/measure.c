#include <time.h>
#include "measure.h"

double measure(size_t iterations, measure_f fn, void *context) {
	clock_t start = clock();
	for (size_t i = 0; i < iterations; ++i) 
		fn(context);
	clock_t end = clock();
	return (double)(end - start) / CLOCKS_PER_SEC / iterations;
}