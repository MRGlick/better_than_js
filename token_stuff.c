
#include "mystring.c"
#include <stdlib.h>
#include "utils.c"


#define thingamabob(...)

#define TOKEN_TYPES \
    X(NULL_TOKEN) \
    X(KEYWORD) \
    X(NAME) \
    X(VARIABLE) \
    X(STRING_LITERAL) \
    X(INTEGER) \
    X(FLOAT) \
    X(OP_EQ) \
    X(OP_NOTEQ) \
    X(OP_GREATER) \
    X(OP_GREATEREQ) \
    X(OP_LESS) \
    X(OP_LESSEQ) \
    X(OP_ASSIGN) \
    X(OP_AND) \
    X(OP_OR) \
    X(OP_NOT) \
    X(OP_ADD) \
    X(OP_SUB) \
    X(OP_MUL) \
    X(OP_DIV) \
    X(OP_MOD) \
    X(LPAREN) \
    X(RPAREN) \
    X(LCURLY) \
    X(RCURLY) \
    X(TYPE) \
    X(STMT_END) \
    X(COMMA) \
    X(UNRESOLVED) \
    X(INVALID) \
    X(BLOCK) \
    X(BOOL) \
    X(IF_STMT) \
    X(IF_ELSE_STMT) \
    X(WHILE_STMT) \
    X(DECL_STMT) \
    X(ASSIGN_STMT) \
    X(DECL_ASSIGN_STMT) \
    X(PRINT_STMT) \
    X(STMT_SEQ) \
    X(VAL_SEQ) \
    X(TOKEN_TYPE_COUNT) \

#define VAR_TYPES \
    X(T_INT) \
    X(T_FLOAT) \
    X(T_STRING) \
    X(T_BOOL) \
    X(T_STRUCT)

typedef enum VarType {
    #define X(a) a, 
    VAR_TYPES
    #undef X
} VarType;

char *var_type_names[] = {
    "int",
    "float",
    "string",
    "bool",
    "struct"
};

// Generate the enum
typedef enum TokenType {
    #define X(name) name,
    TOKEN_TYPES
    #undef X
} TokenType;

// Generate the string table
const char* token_type_names[] = {
    #define X(name) #name,
    TOKEN_TYPES
    #undef X
};
struct TokenNode;

typedef struct Token {
    TokenType type;
    union {
        String text;
        struct TokenNode *unresolved_tokens;
        double double_val;
        int int_val;
        char symbol;
        bool bool_val;
        VarType var_type; 
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