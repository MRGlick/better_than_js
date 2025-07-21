#include <stdlib.h>

typedef struct LLNode {
    void *val;
    struct LLNode *next;
} LLNode;

typedef struct LinkedList {
    LLNode *head;
    LLNode *tail;
} LinkedList;

#define LL_free(list) do { \
    _LL_free(list); \
    list = NULL; \
} while (0)

LinkedList *LL_new() {
    LinkedList *list = malloc(sizeof(LinkedList));
    list->head = NULL;
    list->tail = NULL;

    return list;
}

void LL_prepend(LinkedList *list, LLNode *node) {
    if (list->head == NULL) {
        list->head = node;
        list->tail = node;
        return;
    }
    node->next = list->head;
    list->head = node;
}

void LL_append(LinkedList *list, LLNode *node) {
    node->next = NULL;
    if (list->head == NULL) {
        list->head = node;
        list->tail = node;
        return;
    }
    list->tail->next = node;
    list->tail = node;
}

void *LL_pop_head(LinkedList *list) {
    void *retval = list->head->val;
    LLNode *h = list->head;
    list->head = list->head->next;
    free(h);

    return retval;
}

LLNode *LLNode_new(void *val) {
    LLNode *node = malloc(sizeof(LLNode));
    node->val = val;
    node->next = NULL;
    return node;
}

void _LL_free(LinkedList *list) {
    LLNode *node = list->head;
    while (node != NULL) {
        LLNode *current = node;
        node = node->next;
        free(current);
    }
}

