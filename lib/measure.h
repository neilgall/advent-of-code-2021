
typedef void (*measure_f)(void *context);

double measure(size_t iterations, measure_f fn, void *context);
