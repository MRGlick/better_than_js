
#pragma once

#include "inttypes.h"
#include <stdio.h>
#include "mystring.c"
#include "types.c"

const char SYMBOLS[] = {
    ' ', ',', ';', '(', ')', '{', '}', '+', '-', '/', '*', '=', '>', '<', '!', '&', '|', '.'
};

char *KEYWORDS[] = {
    "if",
    "while",
    "for", // TODO
    "print",
    "write",
    "else",
    "input",
    "return",
    "defer",
    "struct",
    "new",
    "delete"
};

// dont ask
#define MAYBE 2

#define TOKEN_TYPES \
    X(NULL_TOKEN) \
    X(KEYWORD) \
    X(NAME) \
    X(VARIABLE) \
    X(STRING_LITERAL) \
    X(INTEGER) \
    X(FLOAT) \
    X(NULL_REF) \
    X(OP_UNARY_MINUS) \
    X(BOOLOPS_START) \
    X(OP_NOT) \
    X(BINOPS_START) \
    X(OP_EQ) \
    X(OP_NOTEQ) \
    X(OP_GREATER) \
    X(OP_GREATEREQ) \
    X(OP_LESS) \
    X(OP_LESSEQ) \
    X(OP_AND) \
    X(OP_OR) \
    X(BOOLOPS_END) \
    X(ARITHOPS_START) \
    X(OP_ADD) \
    X(OP_SUB) \
    X(OP_MUL) \
    X(OP_DIV) \
    X(OP_MOD) \
    X(ARITHOPS_END) \
    X(BINOPS_END) \
    X(OP_ASSIGN) \
    X(ASSIGN_SEQ) \
    X(OP_NEW) \
    X(MODIFY_TOKENS_START) \
    X(OP_ASSIGN_ADD) \
    X(OP_ASSIGN_SUB) \
    X(OP_ASSIGN_MUL) \
    X(OP_ASSIGN_DIV) \
    X(OP_ASSIGN_MOD) \
    X(MODIFY_TOKENS_END) \
    X(LPAREN) \
    X(RPAREN) \
    X(LCURLY) \
    X(RCURLY) \
    X(TYPE) \
    X(STMT_END) \
    X(COMMA) \
    X(ATTR_ACCESS) \
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
    X(WRITE_STMT) \
    X(INPUT_STMT) \
    X(FUNC_DECL_STMT) \
    X(FUNC_ARGS_SEQ) \
    X(FUNC_ARG) \
    X(FUNC_CALL) \
    X(RETURN_STMT) \
    X(STMT_SEQ) \
    X(VAL_SEQ) \
    X(DECL_SEQ) \
    X(DEFER_STMT) \
    X(STRUCT_DECL_STMT) \
    X(DELETE_STMT) \
    X(TOKEN_TYPE_COUNT)

//              term i made up VVV
// sorted by bin-op conversion precedence (least to most)
typedef struct Val {
    union {
        void *any_val;
        char *s_val;
        double f_val;
        int i_val;
        bool b_val;
    };
    u8 type;
} Val;

typedef struct Inst {
    u8 type;
    Val arg1;
    Val arg2; 
} Inst;

typedef enum VHType {
    VH_FUNC,
    VH_FUNCS,
    VH_VAR,
    VH_STRUCT
} VHType;

typedef struct VarHeader {
    VHType type;
    String name;
    union {
        struct { // VH_VAR
            Type *var_type;
            i32 var_pos;
        };
        struct { // VH_FUNC
            Type *func_return_type;
            i32 func_pos;
            struct VarHeader *func_args;
        };
        struct { // VH_FUNCS
            struct VarHeader *funcs;
        };
        struct { // VH_STRUCT
            struct VarHeader *struct_members;
            i32 struct_size;
            i32 struct_metadata_idx;
        };
    };
}VarHeader;

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
    Type *var_type; 
    union {
        String text;
        double double_val;
        int int_val;
        char symbol;
    };
} Token;

typedef struct TokenNode {
    Token token;
    struct TokenNode *next;
} TokenNode;

const char *vh_type_names[] = {
    "variable",
    "function",
    "struct"
};

typedef struct ASTNode {
    Token token;
    struct ASTNode *children;
    Type *expected_return_type;
    bool complete;
} ASTNode;
