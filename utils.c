
#ifndef UTILS
#define UTILS

#include <stdio.h>
#include <time.h>
//#define PRINT_NULLS

#ifdef PRINT_NULLS

#define null(type) ({printf("nullifying at line %d\n", __LINE__); ((type){0});})

#else

#define null(type) ((type){0})

#endif

#define print_err(...) ({printf("ERROR on line #%d:\n\t", __LINE__); printf(__VA_ARGS__); printf("\n"); getchar();})

#define print_warning(...) ({printf("WARNING on line #%d:\n\t", __LINE__); printf(__VA_ARGS__); printf("\n");})

#define print_todo(...) {printf("TODO: \n\t"); printf(__VA_ARGS__); printf("\n\t on line #%d \n", __LINE__);}

#define return_err(...) do {print_err(__VA_ARGS__); return;} while (0);

#define in_range(a, b, c) ((a >= b) && (a <= c))

#define debug if (1)

#define assert(cond, ...) if (!(cond)) {print_err("Assertion failed! "__VA_ARGS__); exit(1);}

#define min(a, b) (a < b ? a : b)
#define max(a, b) (a > b ? a : b)

double get_current_process_time_seconds() {
    return (double)clock() / CLOCKS_PER_SEC; 
}

long get_current_process_time() {
    return clock();
}

double trunc(double a) {
    return a > 0 ? (long long)a : -(long long)(-a);
}

double fmod(double a, double b) {
    return a - trunc(a / b) * b;
}


#define lerp(a, b, w) (a + (b - a) * w)

#endif