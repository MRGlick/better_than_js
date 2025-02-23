
#include "mystring.c"
#include <stdlib.h>
#include "utils.c"

typedef enum TokenType {
    KEYWORD,
    NAME, // Either a variable or a function (which is maybe also a variable)
    STRING_LITERAL,
    INTEGER,
    FLOAT,
    EXPRESSION,
    SYMBOL,
    UNRESOLVED,
    INVALID,
    SCOPE
} TokenType;

struct TokenNode;

typedef struct Token {
    TokenType type;
    union {
        String text;
        struct TokenNode *unresolved_tokens;
        double double_val;
        int int_val;
        char symbol;
    };
} Token;

typedef struct TokenNode {
    Token token;
    struct TokenNode *next;
} TokenNode;

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