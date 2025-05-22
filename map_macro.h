#pragma once


#define COUNT1(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, n, ...) n
#define COUNT(...) COUNT1(__VA_ARGS__, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)


#define MAP(f, ...) hi COUNT(__VA_ARGS__)


#define X(a) a
MAP(f, a, b, c, d, e)



