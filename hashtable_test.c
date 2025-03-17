#include "hashmap.c"


int main() {
    HashMap *map = HashMap_new(sizeof(int), 1, false);
    HashMap_put(map, StringRef("hi"), (void *)3);
    HashMap_put(map, StringRef("hi2"), (void *)30);
    HashMap_put(map, StringRef("hi3"), (void *)40);
    HashMap_put(map, StringRef("hi4"), (void *)50);
    HashMap_put(map, StringRef("hi5"), (void *)60);


    int hi = (int)HashMap_get(map, StringRef("hi"));
    int hi2 = (int)HashMap_get(map, StringRef("hi2"));

    printf("hi: %d, hi2: %d \n", hi, hi2);

    HashMap_print(map);


    printf("END \n");

    return 0;
}