#ifndef HASH_TABLE_C
#define HASH_TABLE_C

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>   
#include "array.c"


typedef struct HashNode {
    const void *key;
    int key_size;
    void *val;
    struct HashNode *next;
} HashNode;

typedef struct HashMap {

    void **keys;
    int key_size;
    int value_size;

    bool copy_values;

    int capacity;

    HashNode *values;

} HashMap;

#define HashMap(keytype, valtype, copy_values) _HM_new(sizeof(keytype), sizeof(valtype), copy_values)

#define HM_read(valtype, val) (*(valtype *)val)

int hash(char *val, int val_size, int arr_size);

bool _HM_is_hashnode_empty(HashNode node) {

    if (node.key == NULL || node.key_size == 0 || node.val == NULL) return true;

    return false;
}

HashMap _HM_new(int key_size, int value_size, bool copy_values) {
    HashMap table = {0};
    table.key_size = key_size;
    table.value_size = value_size;
    table.capacity = 100;
    table.keys = array(void *, table.capacity);
    table.values = calloc(sizeof(HashNode), table.capacity);
    table.copy_values = copy_values;
    for (int i = 0; i < table.capacity; i++) {
        table.keys[i] = NULL;
    }

    return table;
}

void HM_put(HashMap *table, void *key, void *value) {

    bool found = false;
    for (int i = 0; i < array_length(table->keys); i++) {
        if (table->keys[i] == key) {
            found = true;
            break;
        }
    }
    if (!found) {
        array_append(table->keys, key);
    }

    int hash_value = hash(key, table->key_size, table->capacity);
    HashNode *available = &table->values[hash_value];
    HashNode *last = available;
    while (available->val != NULL) {
        available = available->next;
        if (available->val != NULL) last = available;
    }

    if (last != available) {
        last->next = available;
    }

    available->key = key;
    available->key_size = table->key_size;

    if (!table->copy_values) {
        available->val = value;
    } else {
        void *new_val = malloc(table->value_size);
        memcpy(new_val, value, table->value_size);

        available->val = new_val;
    }


}

void print_hash_node(HashNode *node) {
    printf("Node: %p \n", node);
    printf("\tKey address: %p \n", node->key);
    printf("\tValue address: %p \n", node->val);
    printf("\tKey size: %d \n", node->key_size);
    printf("\tNext node address: %p \n", node->next);
}

void *HM_get(HashMap table, void *key) {

    int hash_value = hash(key, table.key_size, table.capacity);
    HashNode *current = &table.values[hash_value];

    print_hash_node(current);

    if (_HM_is_hashnode_empty(*current)) return NULL;

    while (strncmp(current->key, key, table.key_size) != 0) {
        current = current->next;
    }
    return current->val;
}


void HM_delete(HashMap table) {

    if (table.copy_values) {
        for (int i = 0; i < array_length(table.keys); i++) {
            void *val = HM_get(table, table.keys[i]);
            free(val);
        }
    }



    array_free(table.keys);
    array_free(table.values);
}


int hash(char *val, int val_size, int arr_size) {

    // Thanks Dan

    unsigned long hash = 5381;
    int c;

    for (int i = 0; i < val_size; i++) {
        c = val[i];
        hash = ((hash << 5) + hash) + c; // hash * 33 + c
    }

    return hash % arr_size;
}

void test_hash() {

    int comb_len;
    printf("Enter combination length: ");
    scanf("%d", &comb_len);

    char *combination = malloc(comb_len);
    for (int i = 0; i < comb_len; i++) {
        combination[i] = 'a';
    }
    int arr_size = 20;

    printf("Enter test hashmap size: ");

    scanf("%d", &arr_size);

    int *counter_arr = calloc(sizeof(int), arr_size);

    while (combination[comb_len - 1] < 'z') {
        counter_arr[hash(combination, comb_len, arr_size)]++;

        combination[0]++;
        for (int i = 0; i < comb_len - 1; i++) {
            if (combination[i] > 'z') {
                combination[i] = 'a';
                combination[i + 1]++;
            }
        }
    }

    for (int i = 0; i < arr_size; i++) {
        if (counter_arr[i] > 0) {
            printf("I: %d, Collisions found: %d \n", i, counter_arr[i]);
        }
    }

    free(counter_arr);
    free(combination);
}

















#endif