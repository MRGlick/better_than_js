
#pragma once

#include "inttypes.h"
#include <stdio.h>
#include "mystring.c"
#include "types.c"

const char SYMBOLS[] = {
    ' ', ',', ';', '(', ')', '{', '}', '+', '-', '/', '*', '%', '=', '>', '<', '!', '&', '|', '.', '[', ']', '\''
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


#define calculate_array_offset(idx, elem_size) (sizeof(ObjectHeader) + sizeof(int) + idx * elem_size)


#define TOKEN_TYPES \
    X(NULL_TOKEN) \
    X(KEYWORD) \
    X(NAME) \
    X(VARIABLE) \
    X(STRING_LITERAL) \
    X(CHAR) \
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
    X(ARRAY) \
    X(LBRACKET) \
    X(RBRACKET) \
    X(ARRAY_LITERAL) \
    X(ARRAY_INITIALIZER) \
    X(DIMENSION_SEQ) \
    X(ARRAY_SUBSCRIPT) \
    X(OP_CONVERT_TYPE) \
    X(TOKEN_TYPE_COUNT) 

//              term i made up VVV
// sorted by bin-op conversion precedence (least to most)
typedef struct Val {
    union {
        void *as_ptr;
        char *as_str;
        char as_char;
        double as_double;
        int as_int;
        bool as_bool;
    };
    u8 type;
} Val;

Val Val_int(int n) {return (Val){.type = TYPE_int, .as_int = n};}
Val Val_char(char c) {return (Val){.type = TYPE_char, .as_char = c};}
Val Val_double(double d) {return (Val){.type = TYPE_float, .as_double = d};}
Val Val_bool(bool b) {return (Val){.type = TYPE_bool, .as_bool = b};}
Val Val_str(char *str) {return (Val){.type = TYPE_str, .as_str = str};}
const Val Val_null = {0};


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

const char *token_type_to_pretty_str(TokenType type) {
    switch (type) {
        case NULL_TOKEN:          return "end of file";
        case KEYWORD:             return "keyword";
        case NAME:                return "name";
        case VARIABLE:            return "var";
        case STRING_LITERAL:      return "string literal";
        case CHAR:        return "char";
        case INTEGER:             return "integer";
        case FLOAT:               return "float";
        case NULL_REF:            return "null";
        case OP_UNARY_MINUS:      return "-";
        case BOOLOPS_START:       return "";
        case OP_NOT:              return "!";
        case BINOPS_START:        return "";
        case OP_EQ:               return "==";
        case OP_NOTEQ:            return "!=";
        case OP_GREATER:          return ">";
        case OP_GREATEREQ:        return ">=";
        case OP_LESS:             return "<";
        case OP_LESSEQ:           return "<=";
        case OP_AND:              return "&&";
        case OP_OR:               return "||";
        case BOOLOPS_END:         return "";
        case ARITHOPS_START:      return "";
        case OP_ADD:              return "+";
        case OP_SUB:              return "-";
        case OP_MUL:              return "*";
        case OP_DIV:              return "/";
        case OP_MOD:              return "%";
        case ARITHOPS_END:        return "";
        case BINOPS_END:          return "";
        case OP_ASSIGN:           return "=";
        case ASSIGN_SEQ:          return "=";
        case OP_NEW:              return "new";
        case MODIFY_TOKENS_START: return "";
        case OP_ASSIGN_ADD:       return "+=";
        case OP_ASSIGN_SUB:       return "-=";
        case OP_ASSIGN_MUL:       return "*=";
        case OP_ASSIGN_DIV:       return "/=";
        case OP_ASSIGN_MOD:       return "%=";
        case MODIFY_TOKENS_END:   return "";
        case LPAREN:              return "(";
        case RPAREN:              return ")";
        case LCURLY:              return "{";
        case RCURLY:              return "}";
        case LBRACKET:            return "[";
        case RBRACKET:            return "]";
        case TYPE:                return "type";
        case STMT_END:            return ";";
        case COMMA:               return ",";
        case ATTR_ACCESS:         return ".";
        case UNRESOLVED:          return "unresolved";
        case INVALID:             return "invalid";
        case BLOCK:               return "block";
        case BOOL:                return "true/false";
        case IF_STMT:             return "if";
        case IF_ELSE_STMT:        return "if-else";
        case WHILE_STMT:          return "while";
        case DECL_STMT:           return "decl";
        case ASSIGN_STMT:         return "assign";
        case DECL_ASSIGN_STMT:    return "decl-assign";
        case PRINT_STMT:          return "print";
        case WRITE_STMT:          return "write";
        case INPUT_STMT:          return "input";
        case FUNC_DECL_STMT:      return "func-decl";
        case FUNC_ARGS_SEQ:       return "args";
        case FUNC_ARG:            return "arg";
        case FUNC_CALL:           return "call";
        case RETURN_STMT:         return "return";
        case STMT_SEQ:            return "stmts";
        case VAL_SEQ:             return "vals";
        case DECL_SEQ:            return "decls";
        case DEFER_STMT:          return "defer";
        case STRUCT_DECL_STMT:    return "struct";
        case DELETE_STMT:         return "delete";
        case TOKEN_TYPE_COUNT:    return "count";
        case ARRAY:               return "array";
        case ARRAY_SUBSCRIPT:     return "array subscript";
        case OP_CONVERT_TYPE:     return "type conversion";
    }

    return "<unknown>";
}



struct TokenNode;

typedef struct Token {
    TokenType type;
    Type *var_type; 
    union {
        String text;
        double as_double;
        int as_int;
        char as_char;
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
