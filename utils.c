
#ifndef UTILS
#define UTILS

#include <stdio.h>

#define null(type) ((type){0})

void print_err(const char *text) {
    printf("ERROR:\n\t%s \n", text);
}

void pause_err(const char *text) {
    print_err(text);
    getchar();
}

#endif