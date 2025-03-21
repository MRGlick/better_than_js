#include "malloc_trace.c"
#include "hashmap.c"

typedef struct Test {
    int x1;
    int x2;
} Test;



int main() {
    
    HashMap *map = HashMap(Test);

    HashMap_put(map, StringRef("hi"), &(Test){.x1 = 2, .x2 = 3});
    HashMap_put(map, StringRef("hi"), &(Test){.x1 = 3, .x2 = 3});
    HashMap_put(map, StringRef("hi2"), &(Test){.x1 = 4, .x2 = 3});
    HashMap_put(map, StringRef("hi2"), &(Test){.x1 = 5, .x2 = 3});
    HashMap_put(map, StringRef("hi"), &(Test){.x1 = 62, .x2 = 3});
    
    Test *my_thing = HashMap_get(map, StringRef("hi"));

    HashMap_print(map);

    printf("x1: %d, x2: %d \n", my_thing->x1, my_thing->x2);

    HashMap_free(map);

    HashMap *int_map = HashMap(int);

    int x = 2;

    HashMap_put(int_map, StringRef("blud"), 2 + 3 / x);
    

    HashMap_print(int_map);

    HashMap_free(int_map);


    printf("didn't crash! HURRAY! \n");

    printf("mallocs: %d frees: %d \n", malloc_counter, free_counter);

    return 0;
}