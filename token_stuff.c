
#include "mystring.c"
#include <stdlib.h>
#include "utils.c"
#include "globals.h"


static inline int get_vartype_size(VarType t) {
    switch (t) {
        case T_INT:
            return 4;
            break;
        case T_STRING:
        case T_FLOAT:
        case T_STRUCT:
        case T_NULL_REF:
            return 8;
            break;
        case T_BOOL:
            return 1;
            break;
        default:
            print_err("Can't get vartype size of type [%d]!", (int)t);
            return -1;
            break;
    }
}


TokenNode *TokenNode_create(Token tk) {
    TokenNode *tk_node = malloc(sizeof(TokenNode));
    tk_node->token = tk;
    tk_node->next = NULL;

    return tk_node;
}

#define free_list(tk_node) do { \
    _free_list(tk_node); \
    tk_node = NULL; \
} while (0)

void _free_list(TokenNode *tk_node) {

    if (tk_node == NULL) {
        print_err("Tried to free a null token list!");
        return;
    }

    TokenNode *node = tk_node;
    while (node != NULL) {
        TokenNode *next = node->next;
        free(node);
        node = next;
    }
}

#define list_prepend(tk_node, other) do { \
    if (other == NULL) { \
        print_err("Can't prepend null to a list!"); \
        break; \
    } \
    other->next = tk_node; \
    tk_node = other; \
} while (0)

void list_append(TokenNode *tk_node, TokenNode *other) {

    if (tk_node == NULL) {
        print_err("Tried to append to a null token list!");
        return;
    }

    TokenNode *node = tk_node;
    while (node->next != NULL) {
        node = node->next;
    }

    node->next = other;
}