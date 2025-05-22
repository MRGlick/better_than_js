#pragma once

#include "inttypes.h"
#include <stdlib.h>


typedef i32 Error;


// Approach 1: Error(int) as return value
// Problem: need to return both the error AND the value
Error errable_program(int a, int b) {
    if (b == 0) return -1;

    return a / b;
}

// Approach 2:
// Problem: output variable is a good idea, but if (err != NULL) is kinda boilerplate + magic numbers
int errable_program(int a, int b, Error *err) {
    if (b == 0) {
        if (err != NULL) *err = -1;
        return 0;
    }

    return a / b;
}

// Approach 3:
#define return_error(type, err_code) \
        { \
            if (err != NULL) *err = err_code; \
            return (type){0}; \
        }

#define ERRORS(...) enum {__VA_ARGS__};

ERRORS(divide_err_DIVISION_BY_ZERO);
int divide(int a, int b, Error *err) {
    if (b == 0) return_error(int, divide_err_DIVISION_BY_ZERO);

    return a / b;
}

// Approach 4 - Try to use less macros:
int divide(int a, int b, Error *err) {
    if (b == 0) return_error(int, divide_err_DIVISION_BY_ZERO);

    return a / b;
}
