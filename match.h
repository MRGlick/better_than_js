
#ifndef MATCH_H
#define MATCH_H

#define _NUMBERS 10, 9, 8, 7, 6, 5, 4, 3, 2, 1
#define _GET_COUNT(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, N, ...) N 

#define _ARG_COUNT(...) _COMBINE_ARGS(__VA_ARGS__, _NUMBERS)
#define _COMBINE_ARGS(...) _GET_COUNT(__VA_ARGS__)

#define _MAKE_CASED_1(_1) case _1:
#define _MAKE_CASED_2(_1, _2) case _1: case _2:
#define _MAKE_CASED_3(_1, _2, _3) case _1: case _2: case _3:
#define _MAKE_CASED_4(_1, _2, _3, _4) case _1: case _2: case _3: case _4:
#define _MAKE_CASED_5(_1, _2, _3, _4, _5) case _1: case _2: case _3: case _4: case _5:
#define _MAKE_CASED_6(_1, _2, _3, _4, _5, _6) case _1: case _2: case _3: case _4: case _5: case _6:
#define _MAKE_CASED_7(_1, _2, _3, _4, _5, _6, _7) case _1: case _2: case _3: case _4: case _5: case _6: case _7:
#define _MAKE_CASED_8(_1, _2, _3, _4, _5, _6, _7, _8) case _1: case _2: case _3: case _4: case _5: case _6: case _7: case _8:
#define _MAKE_CASED_9(_1, _2, _3, _4, _5, _6, _7, _8, _9) case _1: case _2: case _3: case _4: case _5: case _6: case _7: case _8: case _9: 
#define _MAKE_CASED_10(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10) case _1: case _2: case _3: case _4: case _5: case _6: case _7: case _8: case _9: case _10:

#define _MAKE_CASED_(N) _MAKE_CASED_##N
#define _MAKE_CASED(N) _MAKE_CASED_##N

#define case(...) _N_CASES(_ARG_COUNT(__VA_ARGS__), __VA_ARGS__)
#define then(...) __VA_ARGS__; break;
#define default(...) default: __VA_ARGS__; break;
#define match switch
#define _N_CASES(N, ...) _MAKE_CASED(N)(__VA_ARGS__)

#endif
// match (type) {
//     case(1) then({
//         // do something
//     })
//     case(2, 3, 4) then({

//     })
//     default({
//         // I don't know
//     })
// }

// turns into:
// switch (type) {
//     case 1: {
//         // do something
//         break;
//     }
//     case 2: case 3: case 4: {
//         break;
//     }
//     default: {
//     // I don't know
//     }
// }