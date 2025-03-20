#include "hashmap.c"


typedef struct Test {
    int x1;
    int x2;
} Test;

int main() {
    
    HashMap *map = HashMap(Test, true);

    HashMap_put(map, StringRef("hi"), (Test){.x1 = 2, .x2 = 3});

    Test *my_thing = HashMap_get(map, StringRef("hi"));

    HashMap_print(map);

    printf("x1: %d, x2: %d \n", my_thing->x1, my_thing->x2);

    HashMap_free(map);

    return 0;
}