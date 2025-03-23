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
    bool primitive_values;
} HashMap;

#define if_primitive_value(true_case, false_case, ...) (_Generic((__VA_ARGS__), \
    int: true_case, long: true_case, float: true_case, double: true_case, bool: true_case, char: true_case, default: false_case))

#define if_primitive_type(true_case, false_case, ...) (if_primitive_value(true_case, false_case, (__VA_ARGS__){0}))

#define HashMap(type) HashMap_new(sizeof(type), 100, (if_primitive_type(1, 0, type)))

// ... = primitive value OR ptr to struct
#define HashMap_put(map, key, ...) do { \
    if (if_primitive_value(1, 0, (__VA_ARGS__)) != map->primitive_values) printf("HashMap type mismatch! Line: %d \n Primitive: %d, map: %d \n", __LINE__, if_primitive_value(1, 0, (__VA_ARGS__)), map->primitive_values); \
    else _HashMap_put(map, key, (void *)(__VA_ARGS__)); \
} while (0)

#define HashMap_free(map) do {_HashMap_free(map); map = NULL;} while (0)

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

HashMap *HashMap_new(int value_size, int capacity, bool prim);

void _HashMap_put(HashMap *map, String key, void *value);

void *HashMap_get(HashMap *map, String key);

void *HashMap_get_safe(HashMap *map, String key, void *default_val);

void _HashMap_free(HashMap *map);

void HashMap_print(HashMap *map);

bool HashMap_contains(HashMap *map, String key);

HashMap *HashMap_copy(HashMap *map);

HashMap *HashMap_new(int value_size, int capacity, bool prim) {
    HashMap *map = malloc(sizeof(HashMap)); 
    
    *map = (HashMap){
        .capacity = capacity,
        .primitive_values = prim,
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

// 'key' is YOUR responsibilty! HashMap_free() won't free it.
void _HashMap_put(HashMap *map, String key, void *value) {

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
    // current->value = map->primitive_values ? copy_value(value, map->value_size) : value;
    // current->next = NULL;
    // printf("setting key to %s \n", key.data);
    // print_hash_node(current);
    // return;

    if (current->empty) {
        current->empty = false;
        current->key = key;
        current->value = !map->primitive_values ? copy_value(value, map->value_size) : value;
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
        if (!map->primitive_values) {
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
    new_node->value = !map->primitive_values ? copy_value(value, map->value_size) : value;
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
    printf("map: \n");
    HashMap_print(map);

    return NULL;

}

void HashNode_free(HashNode *node) {
    if (node == NULL) return;

    HashNode_free(node->next);
    free(node);
}

void _HashMap_free(HashMap *map) {

    if (map == NULL) {
        printf("Tried to free NULL HashMap! \n");
        return;
    }

    if (!map->primitive_values) {
        int len = array_length(map->keys);
        for (int i = 0; i < len; i++) {
            free(HashMap_get(map, map->keys[i]));
        }
    }

    for (int i = 0; i < map->capacity; i++) {
        HashNode_free(map->values[i].next);
    }

    free(map->values);

    array_free(map->keys);

    free(map);

}

void HashMap_print(HashMap *map) {
    printf("{");
    int len = array_length(map->keys);
    for (int i = 0; i < len; i++) {
        printf("\"%s\": <%p>", map->keys[i].data, HashMap_get(map, map->keys[i]));
        if (i + 1 < len) printf(", "); // pretty formatting
    }
    //if (len > 0) printf("\n"); // pretty formatting
    printf("} \n");
}

bool HashMap_contains(HashMap *map, String key) {
    int len = array_length(map->keys);
    for (int i = 0; i < len; i++) {
        if (String_equal(map->keys[i], key)) {
            return true;
        }
    }

    return false;
}

HashMap *HashMap_copy(HashMap *map) {
    HashMap *new = HashMap_new(map->value_size, map->capacity, map->primitive_values);


    int len = array_length(map->keys);
    for (int i = 0; i < len; i++) {
        void *value = map->primitive_values? HashMap_get(map, map->keys[i]) : copy_value(HashMap_get(map, map->keys[i]), map->value_size);

        _HashMap_put(new, map->keys[i], value); // THE ONLY EXCEPTION TO THE MACRO
    }


    return new;
}

void *HashMap_get_safe(HashMap *map, String key, void *default_val) {
    int hash_value = hash(key, map->capacity);

    HashNode *current = &map->values[hash_value];

    if (current->empty) return default_val;
    

    while (current != NULL) {
        if (String_equal(current->key, key)) {
            return current->value;
        }
        current = current->next;
    }

    return default_val;
}


