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

bool _HM_is_hashnode_empty(HashNode node) {

    if (String_isnull(node.key)) return true;

    return false;
}

HashMap _HM_new(int value_size, bool copy_values) {
    HashMap table = {0};
    table.value_size = value_size;
    table.capacity = 100;
    table.keys = array(String, table.capacity);
    table.values = calloc(sizeof(HashNode), table.capacity);
    table.copy_values = copy_values;
    for (int i = 0; i < table.capacity; i++) {
        table.keys[i] = String_null;
    }

    return table;
}

void _HM_put(HashMap table, String key, void *value) {

    bool found = false;
    for (int i = 0; i < array_length(table.keys); i++) {
        if (String_equal(table.keys[i], key)) {
            found = true;
            break;
        }
    }
    if (!found) {
        array_append(table.keys, key);
    }

    int hash_value = hash(key, table.capacity);
    HashNode *available = &table.values[hash_value];
    HashNode *last = available;
    while (available->val != NULL) {
        available = available->next;
        if (!_HM_is_hashnode_empty(*available)) last = available;
    }

    if (last != available) {
        last->next = available;
    }

    available->key = key;

    if (!table.copy_values) {
        available->val = value;
    } else {
        void *new_val = malloc(table.value_size);
        memcpy(new_val, value, table.value_size);

        available->val = new_val;
    }
}

void print_hash_node(HashNode *node) {
    printf("Node: %p \n", node);
    printf("\tKey: '%s' \n", node->key.data);
    printf("\tValue: %p \n", node->val);
    printf("\tNext node address: %p \n", node->next);
}

void *HM_get(HashMap table, String key) {

    int hash_value = hash(key, table.capacity);
    HashNode *current = &table.values[hash_value];

    // print_hash_node(current);

    if (_HM_is_hashnode_empty(*current)) {
        printf("Key doesn't exist in hashmap! \n");
        return NULL;
    }

    while (!String_equal(current->key, key)) {
        current = current->next;
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
        if (!table.keys[i].ref) {
            String_delete(&table.keys[i]);
        }
    }


    array_free(table.keys);
    array_free(table.values);
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

#endif