#pragma once


#define COUNT1(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, n, ...) n
#define COUNT(...) COUNT1(__VA_ARGS__, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

#define SEP

#define MAP_END(...) 

#define CHOOSE_0(a, b) a
#define CHOOSE_1(a, b) b

#define FIRST(a, ...) a

#define MAP_GET_END() 0, MAP_END
#define MAP_NEXT0(test, next, ...) next MAP_OUT
#define MAP_NEXT1(test, next) MAP_NEXT0 (test, next, 0)
#define MAP_NEXT(test, next)  MAP_NEXT1 (MAP_GET_END test, next)



#define MAP0(f, a, ...) f(a) IS_EMPTY(MAP1, MAP_END, __VA_ARGS__) SEP (f, __VA_ARGS__)
#define MAP1(f, a, ...) f(a) IS_EMPTY(MAP0, MAP_END, __VA_ARGS__) SEP (f, __VA_ARGS__)
#define EVAL(...) __VA_ARGS__
#define EVAL2(...) EVAL(EVAL(EVAL(EVAL(EVAL(EVAL(__VA_ARGS__))))))
#define EVAL3(...) EVAL2(EVAL2(EVAL2(EVAL2(EVAL2(EVAL2(__VA_ARGS__))))))

#define MAP(f, ...) EVAL3(MAP1(f, __VA_ARGS__))
