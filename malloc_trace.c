#define ACTIVE



#ifdef ACTIVE

#include <stdlib.h>

int malloc_counter = 0;
int free_counter = 0;

#define malloc(size) tracked_malloc(size)
#define free(ptr) tracked_free(ptr)

void *tracked_malloc(int size) {
    malloc_counter++;
    #undef malloc
    void *res = malloc(size);
    #define malloc(size) tracked_malloc(size)
    return res;
}

void tracked_free(void *ptr) {
    free_counter++;
    #undef free
    free(ptr);
    #define free(ptr) tracked_free(ptr)
}


#endif