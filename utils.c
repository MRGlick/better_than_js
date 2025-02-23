
#include <stdio.h>

void print_err(const char *text) {
    printf("ERROR:\n\t%s \n", text);
}

void pause_err(const char *text) {
    print_err(text);
    getchar();
}