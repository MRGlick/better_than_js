
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

#define print_err(...) {printf("ERROR on line #%d:\n\t", __LINE__); printf(__VA_ARGS__); printf("\n");}

#define pause_err(...) {print_err(__VA_ARGS__); getchar();}

#define in_range(a, b, c) ((a >= b) && (a <= c))

double get_current_process_time_seconds() {
    return (double)clock() / CLOCKS_PER_SEC; 
}

long get_current_process_time() {
    return clock();
}

#endif