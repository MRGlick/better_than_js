// im sick of this shit im just rewriting this

#include "mystring.c"
#include "array.c"
#include <stdbool.h>

typedef struct HashNode {
    String key;
    void *value;
    struct HashNode *next;
    bool empty;
} HashNode;

typedef struct HashMap {
    String *keys;
    HashNode *values;
    int capacity;
    int value_size;
    bool copy;
} HashMap;

int hash(String key, int capacity) {

    // Thanks Dan

    unsigned long hash = 5381;
    int c;

    for (int i = 0; i < key.len; i++) {
        c = key.data[i];
        hash = ((hash << 5) + hash) + c; // hash * 33 + c
    }

    return hash % capacity;
}

HashMap *HashMap_new(int value_size, int capacity, bool copy);

void HashMap_put(HashMap *map, String key, void *value);

void *HashMap_get(HashMap *map, String key);

void HashMap_free(HashMap *map);

void HashMap_print(HashMap *map);

HashMap *HashMap_new(int value_size, int capacity, bool copy) {
    HashMap *map = malloc(sizeof(HashMap)); 
    
    *map = (HashMap){
        .capacity = capacity,
        .copy = copy,
        .value_size = value_size
    };

    map->keys = array(String, 20);

    map->values = malloc(sizeof(HashNode) * capacity);
    for (int i = 0; i < capacity; i++) {
        map->values[i].next = NULL;
        map->values[i].key = String_null;
        map->values[i].value = NULL;
        map->values[i].empty = true; // doing everything explicitly for debugging's sake
    }

    return map;
}

void *copy_value(void *value, int size) {
    void *new = malloc(size);
    memcpy(new, value, size);

    return new;
}

void print_hash_node(HashNode *node) {
    printf("[Node addr: %p, ", node);
    printf("K: \"%s\", V: <%p>]", node->key.data, node->value);
    if (node->next != NULL) {
        printf(" -> ");
        print_hash_node(node->next);
    } else {
        printf("\n");
    }
}


void HashMap_put(HashMap *map, String key, void *value) {

    bool found = false;
    for (int i = 0; i < array_length(map->keys); i++) {
        if (String_equal(map->keys[i], key)) {
            found = true;
            break;
        }
    }

    if (!found) {
        array_append(map->keys, key);
    }

    int hash_value = hash(key, map->capacity);
    HashNode *current = &map->values[hash_value];
    
    // debug
    // current->empty = false;
    // current->key = key;
    // current->value = map->copy ? copy_value(value, map->value_size) : value;
    // current->next = NULL;
    // printf("setting key to %s \n", key.data);
    // print_hash_node(current);
    // return;

    if (current->empty) {
        current->empty = false;
        current->key = key;
        current->value = map->copy ? copy_value(value, map->value_size) : value;
        return;
    }

    HashNode *prev = current;

    while (current != NULL) {
        if (String_equal(current->key, key)) {
            break;
        }
        prev = current;
        current = current->next;
    }

    if (current != NULL) {
        if (map->copy) {
            free(current->value);
            current->value = copy_value(value, map->value_size);
        } else {
            current->value = value;
        }
        return;
    }

    HashNode *new_node = malloc(sizeof(HashNode));
    new_node->empty = false;
    new_node->key = key;
    new_node->next = NULL;
    new_node->value = map->copy ? copy_value(value, map->value_size) : value;
    prev->next = new_node;
    print_hash_node(&map->values[hash_value]);
}

void *HashMap_get(HashMap *map, String key) {
    int hash_value = hash(key, map->capacity);

    HashNode *current = &map->values[hash_value];

    if (current->empty) goto nokey;
    

    while (current != NULL) {
        if (String_equal(current->key, key)) {
            return current->value;
        }
        current = current->next;
    }

    nokey: printf("Key doesn't exist in HashMap! \n");

    return NULL;

}




void HashMap_print(HashMap *map) {
    printf("{");
    int len = array_length(map->keys);
    for (int i = 0; i < len; i++) {
        printf("\n  \"%s\": <%p>", map->keys[i].data, HashMap_get(map, map->keys[i]));
        if (i + 1 < len) printf(","); // pretty formatting
    }
    if (len > 0) printf("\n"); // pretty formatting
    printf("} \n");
}