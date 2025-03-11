
#ifndef UTILS
#define UTILS

#include <stdio.h>

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

#endif