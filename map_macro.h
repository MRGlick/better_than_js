
#define SEP 

#define MAP_END(...)

#define EVAL1(...) __VA_ARGS__

#define MACK() 0, MAP_END

#define PICK_SECOND(a, b, ...) b
#define EXPAND_NEXT(test, next, ...) PICK_SECOND(test, next)
#define NEXT(test, next) EXPAND_NEXT(MACK test, next)  

#define EVAL2(...) EVAL1(EVAL1(EVAL1(EVAL1(EVAL1(__VA_ARGS__)))))
#define EVAL3(...) EVAL2(EVAL2(EVAL2(EVAL2(EVAL2(__VA_ARGS__)))))

#define MAP1_END(...)
#define MAP2_END(...)

#define MAP1(f, current, next, ...) f(current) NEXT (next, MAP2) SEP (f, next, __VA_ARGS__)
#define MAP2(f, current, next, ...) f(current) NEXT (next, MAP1) SEP (f, next, __VA_ARGS__)

#define MAP(f, ...) EVAL3(MAP1(f, __VA_ARGS__, ()))


#define PICK_SECOND_WITH_COMMA(a, b, ...) ,b
#define EXPAND_NEXT_WITH_COMMA(test, next, ...) PICK_SECOND_WITH_COMMA(test, next)
#define NEXT_WITH_COMMA(test, next) EXPAND_NEXT_WITH_COMMA(MACK test, next)  

#define MAP_COMMA1(f, current, next, ...) f(current) NEXT_WITH_COMMA (next, MAP_COMMA2) SEP (f, next, __VA_ARGS__)
#define MAP_COMMA2(f, current, next, ...) f(current) NEXT_WITH_COMMA (next, MAP_COMMA1) SEP (f, next, __VA_ARGS__)


#define MAP_COMMA(f, ...) EVAL3(MAP_COMMA1(f, __VA_ARGS__, ()))
