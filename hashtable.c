#ifndef HASH_TABLE_C
#define HASH_TABLE_C

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>   
#include "array.c"
#include "mystring.c"


typedef struct HashNode {
    String key;
    void *val;
    struct HashNode *next;
    bool is_empty;
} HashNode;

typedef struct HashMap {
    String *keys;
    HashNode *values;
    int value_size;
    int capacity;
    bool copy_values;
} HashMap;

// copy values - wether or not to copy the values put into the hashmap using HM_put(). Useful if the valtype is a string or another type of heap allocated structure.
#define HashMap(valtype, copy_values) _HM_new(sizeof(valtype), copy_values)

#define HM_read(valtype, val) (*(valtype *)val)

#define HM_put(map, key, val) _HM_put(map, key, (void *)val)


int hash(String key, int arr_size);

void HashNode_free(HashNode *node);

void _HashNode_free_helper(HashNode *node);

void print_hash_node(HashNode *node);


bool _HM_is_hashnode_empty(HashNode node) {
    return node.is_empty;
}

HashMap _HM_new(int value_size, bool copy_values) {
    HashMap table = {0};
    table.value_size = value_size;
    table.capacity = 1;
    table.keys = _create_array(sizeof(String), 2);
    table.values = malloc(sizeof(HashNode) * table.capacity);
    for (int i = 0; i < table.capacity; i++) {
        table.values[i].is_empty = true;
    }
    table.copy_values = copy_values;

    return table;
}

void _HM_put(HashMap table, String key, void *value) {

    bool has_key = false;
    for (int i = 0; i < array_length(table.keys); i++) {
        if (String_equal(table.keys[i], key)) {
            has_key = true;
            break;
        }
    }

    printf("has key: %s \n", has_key ? "true" : "false");

    if (!has_key) {
        ArrayHeader *debug_header = array_header(table.keys);
        printf("table.keys: len=%d, capacity=%d, item_size=%d, padding=%d \n", debug_header->length, debug_header->size, debug_header->item_size, debug_header->padding);
        array_append(table.keys, key);
    }

    int hash_value = hash(key, table.capacity);

    HashNode *node = &table.values[hash_value];

    // if its the first time we encounter the node
    if (node->is_empty) {
        printf("First hashnode is empty. \n");
        node->next = NULL;
        node->key = key;
        node->is_empty = false;
        if (table.copy_values) {
            void *new_val = malloc(table.value_size);
            memcpy(new_val, value, table.value_size);
            node->val = new_val; // responsibilties
        } else {
            node->val = value;
        }
        return;
    }

    if (String_equal(node->key, key)) {
        if (table.copy_values) {
            void *new_val = malloc(table.value_size);
            memcpy(new_val, value, table.value_size);
            free(node->val);
            node->val = new_val; // responsibilties
        } else {
            node->val = value;
        }
        return;
    }

    HashNode *current = node;

    bool found = false;

    while (current->next != NULL && !found) {
        if (String_equal(current->key, key)) {
            found = true;
        } else {
            current = current->next;
        }
    }

    if (String_equal(current->key, key)) {
        found = true;
    }

    printf("found: %s \n", found? "true" : "false");
    printf("current->next == NULL: %s \n", current->next == NULL ? "true" : "false");

    if (found) {
        if (table.copy_values) {
            void *new_val = malloc(table.value_size);
            memcpy(new_val, value, table.value_size);
            free(current->val);
            current->val = new_val; // responsibilties
        } else {
            current->val = value;
        }
        return;
    }

    HashNode *new_node = malloc(sizeof(HashNode));
    new_node->is_empty = false;
    new_node->next = NULL;
    new_node->key = key;
    if (table.copy_values) {
        void *new_val = malloc(table.value_size);
        memcpy(new_val, value, table.value_size);
        new_node->val = new_val; // responsibilties
    } else {
        new_node->val = value;
    }
    current->next = new_node;

    ArrayHeader *debug_header = array_header(table.keys);
    printf("table.keys: len=%d, capacity=%d, item_size=%d, padding=%d \n", debug_header->length, debug_header->size, debug_header->item_size, debug_header->padding);

    printf("Inserted key '%s' with value <%p> \n", key.data, value);
}

void print_hash_node(HashNode *node) {
    printf("[Node addr: %p, ", node);
    printf("K: \"%s\", V: <%p>]", node->key.data, node->val);
    if (node->next != NULL) {
        printf(" -> ");
        print_hash_node(node->next);
    } else {
        printf("\n");
    }
}

void *HM_get(HashMap table, String key) {

    int hash_value = hash(key, table.capacity);
    HashNode *current = &table.values[hash_value];

    // print_hash_node(current);

    if (_HM_is_hashnode_empty(*current)) {
        printf("Key doesn't exist in hashmap! \n");
        return NULL;
    }

    while (current != NULL && !String_equal(current->key, key)) {
        current = current->next;
    }

    if (current == NULL) {
        printf("Key doesn't exist in hashmap! \n");
        return NULL;
    }
    return current->val;
}

bool HM_has_key(HashMap table, String key) {
    int hash_value = hash(key, table.capacity);
    HashNode *current = &table.values[hash_value];
    return !_HM_is_hashnode_empty(*current);
}


void HM_delete(HashMap table) {

    if (table.copy_values) {
        for (int i = 0; i < array_length(table.keys); i++) {
            void *val = HM_get(table, table.keys[i]);
            free(val);
        }
    }

    for (int i = 0; i < array_length(table.keys); i++) {
        HashNode_free(&table.values[hash(table.keys[i], table.capacity)]);
    }

    printf("freed hashnodes \n");


    array_free(table.keys);
    array_free(table.values);

    printf("freed arrays \n");
}


int hash(String key, int arr_size) {

    // Thanks Dan

    unsigned long hash = 5381;
    int c;

    for (int i = 0; i < key.len; i++) {
        c = key.data[i];
        hash = ((hash << 5) + hash) + c; // hash * 33 + c
    }

    return hash % arr_size;
}

String *HM_get_keys(HashMap map) {
    String *keys = array(String, 5);

    for (int i = 0; i < array_length(map.keys); i++) {
        if (!String_isnull(map.keys[i])) {
            array_append(keys, map.keys[i]);
        }
    }

    return keys;
}

void HM_print(HashMap map) {
    printf("{");
    String *keys = HM_get_keys(map);
    int len = array_length(keys);
    for (int i = 0; i < len; i++) {
        printf("\n\t'%s' : %p", keys[i].data, HM_get(map, keys[i]));
    }
    if (len > 0) {
        printf("\n");
    }
    printf("} \n");
    array_free(keys);
}

// DOESN'T COPY COMPLEX VALUES!
HashMap HM_copy(HashMap map) {
    HashMap res = _HM_new(map.value_size, map.copy_values);

    String *keys = HM_get_keys(map);

    for (int i = 0; i < array_length(keys); i++) {
        HM_put(res, keys[i], HM_get(map, keys[i]));
    }

    array_free(keys);

    return res;
}

void HashNode_free(HashNode *node) {
    _HashNode_free_helper(node->next);
}

void _HashNode_free_helper(HashNode *node) {
    if (node == NULL) return;
    _HashNode_free_helper(node->next);
    free(node);
}


#endif