
#include "mystring.c"
#include <stdlib.h>
#include "utils.c"
#include "globals.c"
#include "match.h"

static inline int get_typekind_size(u8 t) {
    match (t) {
        case (TYPE_int) 
            return 4;
        case (TYPE_float, TYPE_str, TYPE_struct, TYPE_array, TYPE_null_ref) 
            return 8;
        case (TYPE_bool, TYPE_char) 
            return 1;
        default ()
            print_err("Tried to get size of unknown type! Type: %s \n", type_kind_names[t]);
            return -1;
    }
}

static inline int get_vartype_size(Type *t) {
    return get_typekind_size(t->kind);
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