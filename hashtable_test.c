#include "hashtable.c"


int main() {

    printf("hashmap test: \n");

    HashMap map = HashMap(int, false);

    HM_put(map, StringRef("hello"), 3);
    HM_put(map, StringRef("hello2"), 5);

    int val = (int)HM_get(map, StringRef("hello"));
    int val2 = (int)HM_get(map, StringRef("hello2"));

    printf("val: %d, val2: %d \n", val, val2);

    HM_delete(map);

    return 0;
}