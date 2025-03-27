
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

void print_err(const char *text) {
    printf("ERROR:\n\t%s \n", text);
}

void pause_err(const char *text) {
    print_err(text);
    getchar();
}

double get_current_process_time_seconds() {
    return (double)clock() / CLOCKS_PER_SEC; 
}

long get_current_process_time() {
    return clock();
}

#endif