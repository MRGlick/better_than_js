
// #FLAGS
#define DEBUG_PRINTS_ACTIVE

#define STAGE_LEXER 0
#define STAGE_TOKENIZER 1
#define STAGE_PARSER 2
#define STAGE_IR_GEN 3
#define STAGE_RUN_CODE 4

#define COMPILATION_STAGE STAGE_RUN_CODE

#define PREPROCESS_AST 1

#define LEXER_PRINT 0
#define TOKENIZER_PRINT 0
#define PARSER_PRINT 1
#define IR_PRINT 1


// #define TRACK_ALLOCS
// #define GUARD_DOUBLE_FREE

// #FLAGS END


#pragma GCC diagnostic ignored "-Wunused-but-set-variable"

#include <stdio.h>
#include "utils.c"
#include "mystring.c"
#include "array.c"
#include <stdbool.h>
#include "token_stuff.c"
#include "hashmap.c"
#include "inttypes.h"
#include <inttypes.h>
#include "linked_list.c"
#include "runtime.c"
#include "globals.c"
#include "errors.c"
#include "move.c"
#include "debug.h"
#include "map_macro.h"

VarHeader create_var_header(String name, Type *var_type, int var_pos) {
    return (VarHeader){.type = VH_VAR, .var_type = var_type, .var_pos = var_pos, .name = name};
}

VarHeader create_func_header(String name, Type *return_type, int pos, VarHeader *args) {
    return (VarHeader){
        .name = name, 
        .type = VH_FUNC, 
        .func_return_type = return_type, 
        .func_pos = pos, 
        .func_args = args
    };
}

VarHeader create_struct_header(String name, VarHeader *struct_members, int size, int meta_idx) {
    return (VarHeader){.name = name, .type = VH_STRUCT, .struct_members = struct_members, .struct_size = size,
                        .struct_metadata_idx = meta_idx};
}

void print_token(Token token, int level);

bool is_char_alpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

bool is_char_num(char c) {
    return (c >= '0' && c <= '9');
}

Type *type_from_str(String str) {
    int len = sizeof(type_kind_names) / sizeof(char *);

    for (int i = 0; i < len; i++) {
        if (String_equal(str, StringRef(type_kind_names[i]))) {
            return &_const_types[i];
        }
    }

    return NULL;
}

bool is_int(String part) {

    if (part.len == 0) return false;

    for (int i = 0; i < part.len; i++) {
        if (!is_char_num(part.data[i]) && part.data[i] != '.') return false;
    }

    return true;
}

bool is_name(String part) {

    if (part.len == 0) return false;

    if (!is_char_alpha(part.data[0]) && part.data[0] != '_') return false;

    for (int i = 1; i < part.len; i++) {
        char c = part.data[i];
        if (!is_char_alpha(c) && !is_char_num(c) && c != '_') return false;
    }

    return true;
}

bool is_symbol(String part) {
    if (part.len != 1) return false;

    for (size_t i = 0; i < sizeof(SYMBOLS); i++) {
        if (part.data[0] == SYMBOLS[i]) return true;
    }

    return false;
}

bool is_keyword(String part) {
    if (part.len == 0) return false;

    int len = sizeof(KEYWORDS) / sizeof(char *); 

    for (int i = 0; i < len; i++) {
        if (String_equal(part, StringRef(KEYWORDS[i]))) {
            return true;
        }
    }

    return false;
}

bool str_is_type(String part) {
    if (part.len == 0) return false;

    for (int i = named_types_start(); i < named_types_end(); i++) {
        if (String_equal(part, StringRef(type_kind_names[i]))) {
            return true;
        }
    }

    return false;
}

// ASSUMES A VALID NUMBER!
double parse_double(String number) {
    bool frac = false;
    double d_res = 0;
    double divisor = 10;
    for (int i = 0; i < number.len; i++) {

        if (number.data[i] == '.') {
            frac = true;
            continue;
        }
        if (frac) {
            d_res += (number.data[i] - '0') / divisor;
            divisor *= 10;
        } else {
            d_res *= 10;
            d_res += number.data[i] - '0';
        }
    }

    return d_res;
}

int parse_int(String number) {
    int res = 0;
    for (int i = 0; i < number.len; i++) {
        res *= 10;
        res += number.data[i] - '0';
    }

    return res;
}

typedef struct Lexeme {
    String text;
    int line;
} Lexeme;

static inline Lexeme Lexeme_new(String text, int line) {
    return (Lexeme){.text = text, .line = line};
}



Token *tokenize(Lexeme *Ls) {
    Token *tokens = array(Token, 10);

    int len = array_length(Ls);

    for (int i = 0; i < len; i++) {
        if (String_equal(Ls[i].text, StringRef("true"))) {
            Token tk = {.type = BOOL, .as_int = true, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("false"))) {
            Token tk = {.type = BOOL, .as_int = false, .line = Ls[i].line, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("maybe")) || String_equal(Ls[i].text, StringRef("mabye"))) {
            Token tk = {.type = BOOL, .as_int = MAYBE, .line = Ls[i].line, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("null"))) {
            Type *t = &_const_types[TYPE_struct];
            Token tk = {.type = NULL_REF, .var_type = t, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }

        if (String_equal(Ls[i].text, StringRef("."))) { // check this early because it conflicts with float
            Token tk = {.type = ATTR_ACCESS, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (is_keyword(Ls[i].text)) {
            Token tk = {.type = KEYWORD, .text = Ls[i].text, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (str_is_type(Ls[i].text)) {
            Token tk = {.type = TYPE, .var_type = type_from_str(Ls[i].text), .line = Ls[i].line};
            array_append(tokens, tk);

            continue;
        }

        if (is_name(Ls[i].text)) {
            Token tk = {.type = NAME, .text = Ls[i].text, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }

        if (is_int(Ls[i].text)) {

            if (i + 1 < len && Ls[i + 1].text.data[0] == '.') {
                if (i + 2 < len && is_int(Ls[i + 2].text)) {
                    String full_str = String_concatf(String_concat(Ls[i].text, StringRef(".")), String_copy(Ls[i + 2].text));
                    Token tk = {.type = FLOAT, .as_double = parse_double(full_str), .line = Ls[i].line};
                    array_append(tokens, tk);
                    String_delete(&full_str);
                    i += 2;
                } else {// in cases like '32.' we dont have to use Ls[i + 2].text because all we care about is Ls[i].text ('32' in the example)
                    Token tk = {.type = FLOAT, .as_double = parse_double(Ls[i].text), .line = Ls[i].line};
                    array_append(tokens, tk);
                    i++;
                }
            } else {
                Token tk = {.type = INTEGER, .as_int = parse_int(Ls[i].text), .line = Ls[i].line};
                array_append(tokens, tk);
            }
            continue;


        }

        if (String_equal(Ls[i].text, StringRef("="))) {
            Token tk = {.type = OP_ASSIGN, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("+="))) {
            Token tk = {.type = OP_ASSIGN_ADD, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("-="))) {
            Token tk = {.type = OP_ASSIGN_SUB, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("*="))) {
            Token tk = {.type = OP_ASSIGN_MUL, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("/="))) {
            Token tk = {.type = OP_ASSIGN_DIV, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("%="))) {
            Token tk = {.type = OP_ASSIGN_MOD, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("=="))) {
            Token tk = {.type = OP_EQ, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("!"))) {
            Token tk = {.type = OP_NOT, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("!="))) {
            Token tk = {.type = OP_NOTEQ, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef(">="))) {
            Token tk = {.type = OP_GREATEREQ, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef(">"))) {
            Token tk = {.type = OP_GREATER, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("<="))) {
            Token tk = {.type = OP_LESSEQ, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("<"))) {
            Token tk = {.type = OP_LESS, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("&&"))) {
            Token tk = {.type = OP_AND, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("||"))) {
            Token tk = {.type = OP_OR, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("+"))) {
            Token tk = {.type = OP_ADD, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("-"))) {
            Token tk = {.type = OP_SUB, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("/"))) {
            Token tk = {.type = OP_DIV, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("*"))) {
            Token tk = {.type = OP_MUL, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("("))) {
            Token tk = {.type = LPAREN, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef(")"))) {
            Token tk = {.type = RPAREN, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("{"))) {
            Token tk = {.type = LCURLY, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("}"))) {
            Token tk = {.type = RCURLY, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("["))) {
            Token tk = {.type = LBRACKET, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("]"))) {
            Token tk = {.type = RBRACKET, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef(","))) {
            Token tk = {.type = COMMA, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef(";"))) {
            Token tk = {.type = STMT_END, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("%"))) {
            Token tk = {.type = OP_MOD, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(Ls[i].text, StringRef("->"))) {
            Token tk = {.type = ARROW, .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        // only need to check the first because at this point it's guranteed to be a valid literal
        // also this leaks but who gives a shit
        if (Ls[i].text.data[0] == '"') { 
            Token tk = {.type = STRING_LITERAL, .text = String_cslice(Ls[i].text, 1, Ls[i].text.len - 1), .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        if (Ls[i].text.data[0] == '\'') { 
            if (Ls[i].text.len > 3) {
                print_err("Char literal can only be one character long!");
            }
            Token tk = {.type = CHAR, .as_char = Ls[i].text.data[1], .line = Ls[i].line};
            array_append(tokens, tk);
            continue;
        }
        
        

        Token invalid_token = {.type = INVALID};
        array_append(tokens, invalid_token);
        print_err("Invalid token!");
        printf("Token: %s \n", Ls[i].text.data);

    }

    return tokens;

}

void free_tokens(Token *tokens) {
    array_free(tokens);
}

ASTNode ASTNode_new(Token tk, bool complete) {
    ASTNode node = {0};
    node.children = array(ASTNode, 2);
    node.token = tk;
    node.complete = complete;
    node.expected_return_type = make_type(TYPE_void);
    
    return node;
}

#define ASTNode_add_child(parent, child) array_append(parent.children, child)

#define ASTNode_insert_child(parent, child, idx) array_insert(parent.children, child, idx)

ASTNode ASTNode_copy(ASTNode *ast) {
    ASTNode copy = *ast;
    copy.children = array(ASTNode, 2);
    for (int i = 0; i < array_length(ast->children); i++) {
        ASTNode_add_child(copy, ASTNode_copy(&ast->children[i]));
    }

    return copy;
}

void ast_replace_expected_return_type(ASTNode *ast, Type *new_type) {
    free_type(ast->expected_return_type);
    ast->expected_return_type = new_type;
}

#define free_ast(ast) do { \
    if (!(ast).children) print_err("Tried to free AST with null children! "); \
    _free_ast(ast); \
    ast = (ASTNode){0}; \
} while (0)

int _free_ast(ASTNode ast) {
    
    if (!ast.children) {
        print_err("Tried to free an AST with null children!");
        return -1;
    }

    for (int i = 0; i < array_length(ast.children); i++) {
        free_ast(ast.children[i]);
    }

    array_free(ast.children);

    if (ast.token.type == TYPE) {
        free_type(ast.token.var_type);
    }

    return 0;
}

typedef struct ParseResult {
    bool success;
    int endpos;
    ASTNode node;
} ParseResult;

#define create_parse_result(s, n, e) ({ASTNode abcdef = n; (ParseResult){.success = s, .node = abcdef, .endpos = e};})

ParseResult _parse_failed_successfully = {0};

// done this way to avoid warnings...
#define PARSE_FAILED_SUCCESSFULLY(idx) ({_parse_failed_successfully.success = true; _parse_failed_successfully.endpos = idx; _parse_failed_successfully;})
#define PARSE_FAILED ((ParseResult){0})
#define PARSE_FAILED_DEBUG ({printf("Parse failed on line %d! \n", __LINE__); PARSE_FAILED;})
int parse_idx = 0;
int parse_anchor = 0;
Token *parse_tokens = NULL;

void set_parse_tokens(Token *tokens) {
    parse_tokens = tokens;
    parse_idx = 0;
}

Token get_token(int idx) {
    if (!parse_tokens) {
        print_err("Tried to match tokens, but there aren't any!");
        return null(Token);
    }
    if (idx >= array_length(parse_tokens)) {
        //print_err("Tried to match token, but reached end of tokens!");
        return null(Token);
    }
    return parse_tokens[idx];
}

Token *get_token_ref(int idx) {
    if (!parse_tokens) {
        print_err("Tried to match tokens, but there aren't any!");
        return NULL;
    }
    if (idx >= array_length(parse_tokens)) {
        //print_err("Tried to match token, but reached end of tokens!");
        return NULL;
    }
    return &parse_tokens[idx];
}

#define check_keyword(token, keyword_literal) \
    (token.type == KEYWORD && String_equal(token.text, StringRef(keyword_literal)))

#define PARSE_ERR_SIZE 1024

char parse_err[PARSE_ERR_SIZE] = {0};
int parse_err_most_tokens = 0;

#define MARK_UNUSED(var) ((void)var)

#define START_PARSE ASTNode *__ast_free_list = array(ASTNode, 2); bool print_fails = false; MARK_UNUSED(print_fails); 
#define START_PARSE_DEBUG ASTNode *__ast_free_list = array(ASTNode, 2); bool print_fails = true; MARK_UNUSED(print_fails);

#define FINISH_PARSE(ast) \
    do { \
        array_free(__ast_free_list); \
        return create_parse_result(true, ast, idx); \
    } while (0)

#define set_parse_error_if_deepest(...) \
    if (idx >= parse_err_most_tokens) {  /*if there are multiple deepests, get the last one*/ \
        parse_err_most_tokens = idx; \
        memset(parse_err, 0, PARSE_ERR_SIZE); \
        sprintf(parse_err, __VA_ARGS__); \
    }

#define CANT_FAIL 0
#define CAN_FAIL 1

#define _destroy_free_list() \
    { \
        for (int i = 0; i < array_length(__ast_free_list); i++) \
            free_ast(__ast_free_list[i]); \
        array_free(__ast_free_list); \
    }

#define return_correct_parse_res(...) return (__VA_ARGS__ + 0) \
            ? PARSE_FAILED_SUCCESSFULLY(idx) \
            : (print_fails ? PARSE_FAILED_DEBUG : PARSE_FAILED) \

#define MATCH_PARSE(varname, expr, expected, ...) \
    ParseResult varname = expr; \
    if (!varname.success) { \
        _destroy_free_list(); \
        char msg[200] = {0}; \
        sprintf(msg, "Parse failed! Expected "expected"! on line %d", get_token(idx - 1).line); \
        set_parse_error_if_deepest(msg); \
        return_correct_parse_res(__VA_ARGS__); \
    } \
    if (!is_null_ast(varname.node)) \
        array_append(__ast_free_list, varname.node); \
    idx = varname.endpos;

#define TRY_MATCH_PARSE(varname, expr) \
    ParseResult varname = expr; \
    if (varname.success) { \
        array_append(__ast_free_list, varname.node); \
        idx = varname.endpos; \
    } else { \
        if (print_fails) printf("Parse failed safely on line %d \n", __LINE__); \
    }

#define MATCH_TOKEN_WITH_TYPE(t, ...) \
    { \
        TokenType t__ = t; \
        if (get_token(idx).type != t__) { \
            _destroy_free_list(); \
            set_parse_error_if_deepest( \
                "Parse failed! Expected '%s' after '%s', got '%s' instead! on line %d", \
                token_type_to_pretty_str(t__), \
                token_type_to_pretty_str(get_token(idx - 1).type), \
                token_type_to_pretty_str(get_token(idx).type), \
                get_token(idx).line \
            ); \
            return_correct_parse_res(__VA_ARGS__); \
        } \
        idx++; \
    }

#define MATCH_TOKEN_WITH_KEYWORD(kw, ...) \
    if (!check_keyword(get_token(idx), kw)) { \
        _destroy_free_list(); \
        set_parse_error_if_deepest("Parse failed! Expected '%s', got '%s'! On line %d", kw, token_type_to_pretty_str(get_token(idx).type), get_token(idx).line); \
        return_correct_parse_res(__VA_ARGS__); \
    } \
    idx++;

#define MATCH_TOKEN(truthy_cond, ...) \
    if (!(truthy_cond)) { \
        _destroy_free_list(); \
        set_parse_error_if_deepest("Parse failed! Expected condition '"#truthy_cond"', got '%s'! on line %d", token_type_to_pretty_str(get_token(idx).type), get_token(idx).line); \
        return_correct_parse_res(__VA_ARGS__); \
    } \
    idx++;

#define MATCH_RETURN_IF_PARSED(expr) \
    { \
        ParseResult __res = expr; \
        if (__res.success) return __res; \
    }


ASTNode wrap_with_block(ASTNode node) {
    ASTNode block = ASTNode_new((Token){.type = BLOCK}, true);
    ASTNode_add_child(block, node);

    return block;
}



// #PARSE START

ParseResult parse_expr(int idx);

ParseResult parse_base_rule(int idx);

ParseResult parse_attr_rule(int idx);

ParseResult parse_add_rule(int idx);

ParseResult parse_block(int idx);

ParseResult parse_print_stmt(int idx);

ParseResult parse_while_stmt(int idx);

ParseResult parse_vardecl_stmt(int idx);

ParseResult parse_assign_stmt(int idx);

ParseResult parse_vardecl_assign_stmt(int idx);

ParseResult parse_input_stmt(int idx);

ParseResult parse_func_decl_stmt(int idx);

ParseResult parse_return_stmt(int idx);

ParseResult parse_modify_stmt(int idx);

ParseResult parse_defer_stmt(int idx);

ParseResult parse_struct_decl(int idx);

ParseResult parse_postfix(int idx);

ParseResult parse_postfix_seq(int idx);

ParseResult parse_primary(int idx);

ParseResult parse_unary_rule(int idx);

ParseResult parse_new_rule(int idx);

ParseResult parse_delete_stmt(int idx);

ParseResult parse_for_stmt(int idx);

ParseResult parse_array_literal(int idx);

ParseResult parse_array_initializer(int idx);

ParseResult parse_lambda(int idx);

void print_ast(ASTNode node, int level);



ParseResult parse_value(int idx) {
    
    START_PARSE {
        int token_idx = idx;
        MATCH_TOKEN(
            get_token(idx).type == BOOL
            || get_token(idx).type == STRING_LITERAL
            || get_token(idx).type == INTEGER
            || get_token(idx).type == FLOAT
            || get_token(idx).type == NAME
            || get_token(idx).type == NULL_REF
            || get_token(idx).type == CHAR
        );
        FINISH_PARSE(ASTNode_new(get_token(token_idx), true));
    }
}

#define is_null_ast(ast) (!(ast).children)

ParseResult parse_base_rule(int idx) {
    START_PARSE {
        MATCH_RETURN_IF_PARSED(parse_value(idx));
        MATCH_RETURN_IF_PARSED(parse_new_rule(idx));
        MATCH_RETURN_IF_PARSED(parse_array_literal(idx));
        MATCH_RETURN_IF_PARSED(parse_array_initializer(idx));
        MATCH_RETURN_IF_PARSED(parse_lambda(idx));
        MATCH_TOKEN_WITH_TYPE(LPAREN);
        MATCH_PARSE(expr_res, parse_expr(idx), "expression");
        MATCH_TOKEN_WITH_TYPE(RPAREN);
        expr_res.node.complete = true; // dont remember why i did this, dont have the balls to remove
        FINISH_PARSE(expr_res.node);
    }
}

ParseResult parse_attr_rule_h(int idx) {

    START_PARSE {
        int op_idx = idx;
        MATCH_TOKEN_WITH_TYPE(ATTR_ACCESS, CAN_FAIL);
        int name_idx = idx;
        MATCH_TOKEN_WITH_TYPE(NAME);
        MATCH_PARSE(attr_h_res, parse_attr_rule_h(idx), "NEVER");

        ASTNode node = ASTNode_new(get_token(op_idx), false);
    
        ASTNode_add_child(node, ASTNode_new(get_token(name_idx), true));

        if (is_null_ast(attr_h_res.node)) FINISH_PARSE(node);
    
        ASTNode leaf = attr_h_res.node;
        while (!leaf.children[0].complete) {
            leaf = leaf.children[0];
        }
        array_insert(leaf.children, node, 0);

        attr_h_res.node.complete = true;

        FINISH_PARSE(attr_h_res.node);
    }
}

ParseResult parse_attr_rule(int idx) {
    
    START_PARSE {
        MATCH_PARSE(base_rule_res, parse_base_rule(idx), "identifier");
        MATCH_PARSE(attr_h_res, parse_attr_rule_h(idx), "NEVER");

        if (is_null_ast(attr_h_res.node)) FINISH_PARSE(base_rule_res.node);

        ASTNode leaf = attr_h_res.node;
        while (!leaf.children[0].complete) {
            leaf = leaf.children[0];
        }
        array_insert(leaf.children, base_rule_res.node, 0);
        attr_h_res.node.complete = true;
        FINISH_PARSE(attr_h_res.node);
    }
}



ParseResult parse_mul_rule_h(int idx) {

    START_PARSE {
        int op_idx = idx;
        MATCH_TOKEN(
            get_token(idx).type == OP_MUL 
            || get_token(idx).type == OP_DIV
            || get_token(idx).type == OP_MOD
        , CAN_FAIL);

        MATCH_PARSE(unary_res, parse_unary_rule(idx), "operand");
        MATCH_PARSE(mul_rule_h_res, parse_mul_rule_h(idx), "NEVER");

        ASTNode node = ASTNode_new(get_token(op_idx), false);
        
        ASTNode_add_child(node, unary_res.node);

        if (is_null_ast(mul_rule_h_res.node)) FINISH_PARSE(node);

        ASTNode leaf = mul_rule_h_res.node;
        while (!leaf.children[0].complete) {
            leaf = leaf.children[0];
        }
        array_insert(leaf.children, node, 0);
        node.complete = true;
        FINISH_PARSE(mul_rule_h_res.node);
    }
}

ParseResult parse_mul_rule(int idx) {

    START_PARSE {
        MATCH_PARSE(unary_res, parse_unary_rule(idx), "operand");

        MATCH_PARSE(mul_rule_h_res, parse_mul_rule_h(idx), "NEVER");

        if (is_null_ast(mul_rule_h_res.node)) FINISH_PARSE(unary_res.node);

        ASTNode leaf = mul_rule_h_res.node;
        while (!leaf.children[0].complete) {
            leaf = leaf.children[0];
        }
        array_insert(leaf.children, unary_res.node, 0);

        mul_rule_h_res.node.complete = true;
        FINISH_PARSE(mul_rule_h_res.node);
    }
}

ParseResult parse_add_rule_h(int idx) {
    
    START_PARSE {
        int op_idx = idx;
        MATCH_TOKEN(
            get_token(idx).type == OP_ADD
            || get_token(idx).type == OP_SUB
            , CAN_FAIL
        );

        MATCH_PARSE(mul_rule_res, parse_mul_rule(idx), "operand");
        MATCH_PARSE(add_rule_h_res, parse_add_rule_h(idx), "NEVER");

        ASTNode node = ASTNode_new(get_token(op_idx), false);
        
        ASTNode_add_child(node, mul_rule_res.node);

        if (is_null_ast(add_rule_h_res.node)) FINISH_PARSE(node);

        ASTNode leaf = add_rule_h_res.node;
        while (!leaf.children[0].complete) {
            leaf = leaf.children[0];
        }
        array_insert(leaf.children, node, 0);
        
        add_rule_h_res.node.complete = true;

        FINISH_PARSE(add_rule_h_res.node);
    }
}

// 1 + (1 + 1)

ParseResult parse_add_rule(int idx) {

    START_PARSE {
        MATCH_PARSE(mul_rule_res, parse_mul_rule(idx), "operand");

        MATCH_PARSE(add_rule_h_res, parse_add_rule_h(idx), "NEVER");

        if (is_null_ast(add_rule_h_res.node)) FINISH_PARSE(mul_rule_res.node);

        ASTNode leaf = add_rule_h_res.node;
        while (!leaf.children[0].complete) {
            leaf = leaf.children[0];
        }
        array_insert(leaf.children, mul_rule_res.node, 0);
        add_rule_h_res.node.complete = true;
        FINISH_PARSE(add_rule_h_res.node);
    }
}

ParseResult parse_rel_rule_h(int idx) {

    START_PARSE {
        int op_idx = idx;
        MATCH_TOKEN (
            get_token(idx).type == OP_EQ 
            || get_token(idx).type == OP_NOTEQ 
            || get_token(idx).type == OP_GREATER 
            || get_token(idx).type == OP_GREATEREQ
            || get_token(idx).type == OP_LESS
            || get_token(idx).type == OP_LESSEQ
            , CAN_FAIL
        );

        MATCH_PARSE(add_rule_res, parse_add_rule(idx), "operand");

        MATCH_PARSE(rel_rule_h_res, parse_rel_rule_h(idx), "NEVER");

        ASTNode node = ASTNode_new(get_token(op_idx), false);
        
        ASTNode_add_child(node, add_rule_res.node);        

        if (is_null_ast(rel_rule_h_res.node)) FINISH_PARSE(node);

        ASTNode leaf = rel_rule_h_res.node;
        while (!leaf.children[0].complete) {
            leaf = leaf.children[0];
        }
        array_insert(leaf.children, node, 0);
        
        rel_rule_h_res.node.complete = true;

        FINISH_PARSE(rel_rule_h_res.node);
    
    }
}

ParseResult parse_rel_rule(int idx) {

    START_PARSE {
        MATCH_PARSE(add_rule_res, parse_add_rule(idx), "operand");

        MATCH_PARSE(rel_rule_h_res, parse_rel_rule_h(idx), "NEVER");

        if (is_null_ast(rel_rule_h_res.node)) FINISH_PARSE(add_rule_res.node);

        ASTNode leaf = rel_rule_h_res.node;
        while (!leaf.children[0].complete) {
            leaf = leaf.children[0];
        }
        array_insert(leaf.children, add_rule_res.node, 0);
        rel_rule_h_res.node.complete = true;
        FINISH_PARSE(rel_rule_h_res.node);
    }
}

ParseResult parse_and_rule_h(int idx) {

    START_PARSE {
        int op_idx = idx;
        MATCH_TOKEN_WITH_TYPE(OP_AND, CAN_FAIL);

        MATCH_PARSE(rel_rule_res, parse_rel_rule(idx), "operand");

        MATCH_PARSE(and_rule_h_res, parse_and_rule_h(idx), "NEVER");

        ASTNode node = ASTNode_new(get_token(op_idx), false);
        
        ASTNode_add_child(node, rel_rule_res.node);

        

        if (is_null_ast(and_rule_h_res.node)) FINISH_PARSE(node);
        ASTNode leaf = and_rule_h_res.node;
        while (!leaf.children[0].complete) {
            leaf = leaf.children[0];
        }
        array_insert(leaf.children, node, 0);
        

        and_rule_h_res.node.complete = true;

        FINISH_PARSE(and_rule_h_res.node);
    }
}

ParseResult parse_and_rule(int idx) {

    START_PARSE {
        MATCH_PARSE(rel_rule_res, parse_rel_rule(idx), "operand");

        MATCH_PARSE(and_rule_h_res, parse_and_rule_h(idx), "NEVER");

        if (is_null_ast(and_rule_h_res.node)) FINISH_PARSE(rel_rule_res.node);

        ASTNode leaf = and_rule_h_res.node;
        while (!leaf.children[0].complete) {
            leaf = leaf.children[0];
        }
        array_insert(leaf.children, rel_rule_res.node, 0);
        and_rule_h_res.node.complete = true;
        FINISH_PARSE(and_rule_h_res.node);
    }
}

ParseResult parse_expr_h(int idx) {

    START_PARSE {
        int op_idx = idx;
        MATCH_TOKEN_WITH_TYPE(OP_OR, CAN_FAIL);

        MATCH_PARSE(and_rule_res, parse_and_rule(idx), "operand");

        MATCH_PARSE(expr_h_res, parse_expr_h(idx), "NEVER");

        ASTNode node = ASTNode_new(get_token(op_idx), false);
        
        ASTNode_add_child(node, and_rule_res.node);

        if (is_null_ast(expr_h_res.node)) FINISH_PARSE(node);
        ASTNode leaf = expr_h_res.node;
        while (!leaf.children[0].complete) {
            leaf = leaf.children[0];
        }
        array_insert(leaf.children, node, 0);
        
        expr_h_res.node.complete = true;

        FINISH_PARSE(expr_h_res.node);
    }
}

ParseResult parse_expr(int idx) {

    START_PARSE {
        MATCH_PARSE(and_rule_res, parse_and_rule(idx), "operand");

        MATCH_PARSE(expr_h_res, parse_expr_h(idx), "NEVER");

        if (is_null_ast(expr_h_res.node)) FINISH_PARSE(and_rule_res.node);

        ASTNode leaf = expr_h_res.node;
        while (!leaf.children[0].complete) {
            leaf = leaf.children[0];
        }
        array_insert(leaf.children, and_rule_res.node, 0);
        expr_h_res.node.complete = true;
        FINISH_PARSE(expr_h_res.node);
    }
}



ParseResult parse_stmt(int idx);

ParseResult parse_if_stmt(int idx) {

    START_PARSE {

        MATCH_TOKEN_WITH_KEYWORD("if");
        
        MATCH_TOKEN_WITH_TYPE(LPAREN);
        
        MATCH_PARSE(expr_res, parse_expr(idx), "boolean expression");
        
        MATCH_TOKEN_WITH_TYPE(RPAREN);
        
        MATCH_PARSE(stmt_res, parse_stmt(idx), "statement after if");
        
        if (check_keyword(get_token(idx), "else")) {
            
            MATCH_TOKEN_WITH_KEYWORD("else");

            MATCH_PARSE(stmt2_res, parse_stmt(idx), "statement after else");

            ASTNode node = ASTNode_new((Token){.type = IF_ELSE_STMT}, true);

            ASTNode_add_child(node, expr_res.node);
            if (stmt_res.node.token.type != BLOCK) {
                stmt_res.node = wrap_with_block(stmt_res.node);
            }
            
            if (stmt2_res.node.token.type != BLOCK) {
                stmt2_res.node = wrap_with_block(stmt2_res.node);
            }
            

            ASTNode_add_child(node, stmt_res.node);
            ASTNode_add_child(node, stmt2_res.node);

            FINISH_PARSE(node);

        } else {
            ASTNode node = ASTNode_new((Token){.type = IF_STMT}, true);
            ASTNode_add_child(node, expr_res.node);

            if (stmt_res.node.token.type != BLOCK) {
                stmt_res.node = wrap_with_block(stmt_res.node);
            }
            
            ASTNode_add_child(node, stmt_res.node);

            FINISH_PARSE(node);
        }
    }
}

ParseResult parse_expr_stmt(int idx) {
    START_PARSE {
        MATCH_PARSE(expr_res, parse_expr(idx), "expression");
        MATCH_TOKEN_WITH_TYPE(STMT_END);

        FINISH_PARSE(expr_res.node);
    }
}

// #PARSE STMT

ParseResult parse_stmt(int idx) {

    START_PARSE {
        MATCH_RETURN_IF_PARSED(parse_block(idx));
        MATCH_RETURN_IF_PARSED(parse_if_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_while_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_print_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_modify_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_vardecl_assign_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_vardecl_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_func_decl_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_input_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_return_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_defer_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_struct_decl(idx));
        MATCH_RETURN_IF_PARSED(parse_delete_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_for_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_assign_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_expr_stmt(idx));

        MATCH_TOKEN(false); // to exit, since non of the other ones exit on fail
    }
}


ParseResult parse_stmt_seq(int idx);

ParseResult parse_program() {
    memset(parse_err, 0, PARSE_ERR_SIZE);
    parse_err_most_tokens = 0;
    return parse_stmt_seq(0);
}

ParseResult parse_stmt_seq(int idx) {
    
    START_PARSE {
        MATCH_PARSE(stmt_res, parse_stmt(idx), "statement");
        
        ASTNode node = ASTNode_new((Token){.type = STMT_SEQ}, true);
        
        ASTNode_add_child(node, stmt_res.node);
        
        while (true) {
            TRY_MATCH_PARSE(res, parse_stmt(idx));
            
            if (!res.success) break;

            ASTNode_add_child(node, res.node);
        }
        
        FINISH_PARSE(node);
        
    }
}


ParseResult parse_block(int idx) {
    START_PARSE {
        
        MATCH_TOKEN_WITH_TYPE(LCURLY);
        
        TRY_MATCH_PARSE(stmt_seq_res, parse_stmt_seq(idx));

        MATCH_TOKEN_WITH_TYPE(RCURLY);
        
        if (stmt_seq_res.success) {
            stmt_seq_res.node.token.type = BLOCK;
            FINISH_PARSE(stmt_seq_res.node);
        } else {
            FINISH_PARSE(ASTNode_new((Token){.type = BLOCK}, true));
        }

    }
}

ParseResult parse_val_seq(int idx) {

    START_PARSE {
        MATCH_PARSE(expr_res, parse_expr(idx), "expression");

        ASTNode node = ASTNode_new((Token){.type = VAL_SEQ}, true);

        ASTNode_add_child(node, expr_res.node);

        while (true) {
            if (get_token(idx).type != COMMA) break;
            MATCH_TOKEN_WITH_TYPE(COMMA);

            TRY_MATCH_PARSE(res, parse_expr(idx));
            if (!res.success) break;

            ASTNode_add_child(node, res.node);
        }

        FINISH_PARSE(node);
    }
}

ParseResult parse_print_stmt(int idx) {
    START_PARSE {
        bool newline = false;
        MATCH_TOKEN(
            (newline = check_keyword(get_token(idx), "print")) 
            || check_keyword(get_token(idx), "write")
        );
    
        MATCH_PARSE(val_seq, parse_val_seq(idx), "value sequence");

        MATCH_TOKEN_WITH_TYPE(STMT_END);
        
        val_seq.node.token.type = newline ? PRINT_STMT : WRITE_STMT;

        FINISH_PARSE(val_seq.node);
    }
}

ParseResult parse_while_stmt(int idx) {

    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("while");
        MATCH_TOKEN_WITH_TYPE(LPAREN);
        MATCH_PARSE(expr_res, parse_expr(idx), "boolean expression");
        MATCH_TOKEN_WITH_TYPE(RPAREN);
        MATCH_PARSE(stmt_res, parse_stmt(idx), "statement after while");

        ASTNode node = ASTNode_new((Token){.type =  WHILE_STMT}, true);

        ASTNode_add_child(node, expr_res.node);

        if (stmt_res.node.token.type != BLOCK) {
            stmt_res.node = wrap_with_block(stmt_res.node);
        }

        ASTNode_add_child(node, stmt_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_array_type_postfix(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(LBRACKET);
        MATCH_TOKEN_WITH_TYPE(RBRACKET);

        FINISH_PARSE(ASTNode_new((Token){.type = TYPE, .var_type = make_array_type(NULL)}, false));
    }
}

ParseResult parse_type(int idx, bool *is_struct_out);

ParseResult parse_type_seq(int idx) {
    START_PARSE {
        MATCH_PARSE(type_res, parse_type(idx, NULL), "type in type sequence");

        ASTNode node = ASTNode_new((Token){.type = TYPE_SEQ}, true);

        ASTNode_add_child(node, type_res.node);

        while (true) {

            if (get_token(idx).type != COMMA) break;
            MATCH_TOKEN_WITH_TYPE(COMMA);

            TRY_MATCH_PARSE(res, parse_type(idx, NULL));
            if (!res.success) break;

            ASTNode_add_child(node, res.node);
        }


        FINISH_PARSE(node);
    }
}

ParseResult parse_func_type_postfix(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(LPAREN);
        TRY_MATCH_PARSE(type_seq, parse_type_seq(idx));
        MATCH_TOKEN_WITH_TYPE(RPAREN);

        ASTNode node = ASTNode_new(
            (Token){.type = TYPE, .var_type = make_func_type(NULL, array(Type *, 0))}, 
            false
        );

        if (type_seq.success) {
            ASTNode_add_child(node, type_seq.node);
        } else {
            ASTNode_add_child(node, ASTNode_new((Token){.type = TYPE_SEQ}, true));
        }

        FINISH_PARSE(node);
    }
}

ParseResult parse_type_postfix(int idx) {
    START_PARSE {
        // .. different types go here
        MATCH_RETURN_IF_PARSED(parse_array_type_postfix(idx));
        MATCH_RETURN_IF_PARSED(parse_func_type_postfix(idx));

        MATCH_TOKEN(false);
    }
}

ParseResult parse_type_postfix_seq(int idx) {
    START_PARSE {
        MATCH_PARSE(postfix_res, parse_type_postfix(idx), "postfix");

        ASTNode node = postfix_res.node;

        while (true) {
            TRY_MATCH_PARSE(res, parse_type_postfix(idx));
            if (!res.success) break;

            ASTNode_add_child(res.node, node);
            node = res.node;
        }


        FINISH_PARSE(node);
    }
}

Type *make_type_from_tree(ASTNode *ast) {

    assert(ast->token.type == TYPE || ast->token.type == NAME);
    
    if (ast->token.type == NAME) {
        return make_struct_type(ast->token.text);
    }

    match (ast->token.var_type->kind) {
        case (TYPE_array) 
            return make_array_type(make_type_from_tree(&ast->children[0]));

        case (TYPE_func) {
            Type *return_type = make_type_from_tree(&ast->children[0]);
            
            ASTNode *args_node = &ast->children[1];
            int len = array_length(args_node->children);
            Type **arg_types = array(Type *, len);
            for (int i = 0; i < len; i++) {
                array_append(arg_types, copy_type(args_node->children[i].token.var_type));
            }

            return make_func_type(move(return_type), move(arg_types));
        }

        default () 
            return copy_type(ast->token.var_type);
    }
}

ParseResult parse_type(int idx, bool *is_struct_out) {
    
    START_PARSE {
        bool is_struct = false;
        int type_idx = idx;
        MATCH_TOKEN(
            get_token(idx).type == TYPE
            || (is_struct = get_token(idx).type == NAME)
        );

        TRY_MATCH_PARSE(postfix_seq_res, parse_type_postfix_seq(idx));

        Type *base_type = is_struct 
                        ? make_struct_type(get_token(type_idx).text) 
                        : copy_type(get_token(type_idx).var_type);

        Type *t = NULL;

        if (postfix_seq_res.success) {
        
            ASTNode leaf = postfix_seq_res.node;
            while (array_length(leaf.children) != 0 && !leaf.children[0].complete) {
                leaf.children[0].complete = true;
                leaf = leaf.children[0];
            }

            ASTNode_insert_child(leaf, ASTNode_new((Token){.type = TYPE, .var_type = move(base_type)}, true), 0);

            t = make_type_from_tree(&postfix_seq_res.node);

            free_ast(postfix_seq_res.node);

        } else {
            t = move(base_type);
        }

        ASTNode node = ASTNode_new(
            (Token) {
                .type = TYPE,
                .var_type = t
            },
            true
        );


        if (is_struct_out != NULL) *is_struct_out = is_struct;

        FINISH_PARSE(node);
    }
}

ParseResult parse_vardecl_stmt(int idx) {
    START_PARSE {
        MATCH_PARSE(type_res, parse_type(idx, NULL), "type");

        int name_idx = idx;
        MATCH_TOKEN_WITH_TYPE(NAME);

        MATCH_TOKEN_WITH_TYPE(STMT_END);

        ASTNode node = ASTNode_new((Token){.type = DECL_STMT}, true);

        ASTNode_add_child(node, type_res.node);

        ASTNode_add_child(node, ASTNode_new(get_token(name_idx), true));

        FINISH_PARSE(node);
    }
}

ParseResult parse_assign_stmt(int idx) {
    
    START_PARSE {
        MATCH_PARSE(primary_res, parse_primary(idx), "variable");

        MATCH_TOKEN_WITH_TYPE(OP_ASSIGN);

        MATCH_PARSE(expr_res, parse_expr(idx), "expression");

        MATCH_TOKEN_WITH_TYPE(STMT_END);

        ASTNode node = ASTNode_new((Token){.type = ASSIGN_STMT}, true);
    
        ASTNode_add_child(node, primary_res.node);
        
        ASTNode_add_child(node, expr_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_vardecl_assign_stmt(int idx) {

    START_PARSE {
        MATCH_PARSE(type_res, parse_type(idx, NULL), "type");

        int name_idx = idx;
        MATCH_TOKEN_WITH_TYPE(NAME);

        MATCH_TOKEN_WITH_TYPE(OP_ASSIGN);

        MATCH_PARSE(expr_res, parse_expr(idx), "expression");

        MATCH_TOKEN_WITH_TYPE(STMT_END);

        ASTNode node = ASTNode_new((Token){.type = DECL_ASSIGN_STMT}, true);
    
        ASTNode_add_child(node, type_res.node);
        
        ASTNode_add_child(node, ASTNode_new(get_token(name_idx), true));
        
        ASTNode_add_child(node, expr_res.node);
        
        FINISH_PARSE(node);
    }
}

ParseResult parse_input_stmt(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("input");

        MATCH_PARSE(primary_res, parse_primary(idx), "variable");

        MATCH_TOKEN_WITH_TYPE(STMT_END);

        ASTNode node = ASTNode_new((Token){.type = INPUT_STMT}, true);

        ASTNode_add_child(node, primary_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_func_arg(int idx) {
    START_PARSE {
        MATCH_PARSE(type_res, parse_type(idx, NULL), "type");

        int name_idx = idx;
        MATCH_TOKEN_WITH_TYPE(NAME);

        ASTNode node = ASTNode_new((Token){.type = FUNC_ARG}, true);
        ASTNode_add_child(node, type_res.node);
        ASTNode_add_child(node, ASTNode_new(get_token(name_idx), true));

        FINISH_PARSE(node);
    }
}

ParseResult parse_func_args_seq(int idx) {
    START_PARSE {
        MATCH_PARSE(func_arg_res, parse_func_arg(idx), "function argument", CAN_FAIL);

        ASTNode node = ASTNode_new((Token){.type = FUNC_ARGS_SEQ}, true);

        ASTNode_add_child(node, func_arg_res.node);

        while (true) {

            if (get_token(idx).type != COMMA) break;

            MATCH_TOKEN_WITH_TYPE(COMMA);


            TRY_MATCH_PARSE(res, parse_func_arg(idx));

            if (!res.success) break;

            ASTNode_add_child(node, res.node);
        }

        FINISH_PARSE(node);

    }
}

ParseResult parse_func_decl_stmt(int idx) {
    START_PARSE {
        MATCH_PARSE(type_res, parse_type(idx, NULL), "type");

        int name_idx = idx;
        MATCH_TOKEN_WITH_TYPE(NAME);

        MATCH_TOKEN_WITH_TYPE(LPAREN);

        MATCH_PARSE(func_args_res, parse_func_args_seq(idx), "function arguments");

        MATCH_TOKEN_WITH_TYPE(RPAREN);

        MATCH_PARSE(stmt_res, parse_stmt(idx), "statement after function declaration");

        // turn one liners into blocks so i can gurantee every function has a block (to make dealing with argument lifetimes easier)
        if (stmt_res.node.token.type != BLOCK) {
            stmt_res.node = wrap_with_block(stmt_res.node);
        }

        ASTNode node = ASTNode_new((Token){.type = FUNC_DECL_STMT}, true);

        ASTNode_add_child(node, type_res.node);

        ASTNode_add_child(node, ASTNode_new(get_token(name_idx), true));

        if (!is_null_ast(func_args_res.node)) {
            ASTNode_add_child(node, func_args_res.node);
        } else {
            // empty placeholder node
            ASTNode_add_child(node, ASTNode_new((Token){.type = FUNC_ARGS_SEQ}, true));
        }

        ASTNode_add_child(node, stmt_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_return_stmt(int idx) {

    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("return");

        TRY_MATCH_PARSE(expr_res, parse_expr(idx));
        if (expr_res.success) {
            MATCH_TOKEN_WITH_TYPE(STMT_END);

            ASTNode node = ASTNode_new((Token){.type = RETURN_STMT}, true);

            ASTNode_add_child(node, expr_res.node);

            FINISH_PARSE(node);
        }

        MATCH_TOKEN_WITH_TYPE(STMT_END);

        FINISH_PARSE(ASTNode_new((Token){.type = RETURN_STMT}, true));
    }
}

ParseResult parse_modify_stmt(int idx) {
    START_PARSE {
        MATCH_PARSE(primary_res, parse_primary(idx), "variable");
        
        int op_idx = idx;
        MATCH_TOKEN(in_range(get_token(idx).type, MODIFY_TOKENS_START, MODIFY_TOKENS_END));
        
        MATCH_PARSE(expr_res, parse_expr(idx), "expression");
        
        MATCH_TOKEN_WITH_TYPE(STMT_END);
        
        
        
        ASTNode node = ASTNode_new((Token){.type = ASSIGN_STMT}, true);
        
        
        ASTNode_add_child(node, primary_res.node);
        TokenType op_type = NULL_TOKEN;
        
        match (get_token(op_idx).type) {
            case (OP_ASSIGN_ADD) 
                op_type = OP_ADD;
            
            case (OP_ASSIGN_SUB) 
                op_type = OP_SUB;
            
            case (OP_ASSIGN_MUL) 
                op_type = OP_MUL;
            
            case (OP_ASSIGN_DIV) 
                op_type = OP_DIV;
            
            case (OP_ASSIGN_MOD) 
                op_type = OP_MOD;
            
            default () print_err("LITERALLY CAN'T HAPPEN. KYS.");
        }
        
        ASTNode op_node = ASTNode_new((Token){.type = op_type}, true);
        
        ASTNode primary_res_node_copy = ASTNode_copy(&primary_res.node);
        ASTNode_add_child(op_node, primary_res_node_copy); // because we already added that node to the tree
        ASTNode_add_child(op_node, expr_res.node);
        ASTNode_add_child(node, op_node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_defer_stmt(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("defer");

        MATCH_PARSE(stmt_res, parse_stmt(idx), "statement after defer");

        ASTNode node = ASTNode_new((Token){.type = DEFER_STMT}, true);
        ASTNode_add_child(node, stmt_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult _parse_vardecl_and_or_assign_decl(int idx) {
    START_PARSE {
        MATCH_RETURN_IF_PARSED(parse_vardecl_stmt(idx));
        MATCH_RETURN_IF_PARSED(parse_vardecl_assign_stmt(idx));

        MATCH_TOKEN(false);
    }
}

ParseResult _parse_decl_seq(int idx) {
    START_PARSE {
        MATCH_PARSE(decl_res, _parse_vardecl_and_or_assign_decl(idx), "declaration");

        ASTNode node = ASTNode_new((Token){.type = DECL_SEQ}, true);

        ASTNode_add_child(node, decl_res.node);

        while (true) {
            TRY_MATCH_PARSE(res, _parse_vardecl_and_or_assign_decl(idx));
            if (!res.success) break;

            ASTNode_add_child(node, res.node);
        }

        FINISH_PARSE(node);
    }
}

ParseResult parse_struct_decl(int idx) {

    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("struct");

        int name_idx = idx;
        MATCH_TOKEN_WITH_TYPE(NAME);

        MATCH_TOKEN_WITH_TYPE(LCURLY);

        MATCH_PARSE(decl_seq_res, _parse_decl_seq(idx), "declaration sequence");

        MATCH_TOKEN_WITH_TYPE(RCURLY);

        ASTNode node = ASTNode_new((Token){.type = STRUCT_DECL_STMT}, true);

        ASTNode_add_child(node, ASTNode_new(get_token(name_idx), true));
        ASTNode_add_child(node, decl_seq_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_attr_postfix(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(ATTR_ACCESS);

        int name_idx = idx;
        MATCH_TOKEN_WITH_TYPE(NAME);
        
        ASTNode node = ASTNode_new((Token){.type = ATTR_ACCESS}, false);
        ASTNode_add_child(node, ASTNode_new(get_token(name_idx), true));
        FINISH_PARSE(node);
    }
}

ParseResult parse_func_call_postfix(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(LPAREN);

        TRY_MATCH_PARSE(val_seq_res, parse_val_seq(idx));

        if (val_seq_res.success) {
            MATCH_TOKEN_WITH_TYPE(RPAREN);

            ASTNode node = ASTNode_new((Token){.type = FUNC_CALL}, false);

            ASTNode_add_child(node, val_seq_res.node);
            FINISH_PARSE(node);
        }

        MATCH_TOKEN_WITH_TYPE(RPAREN);

        ASTNode node = ASTNode_new((Token){.type = FUNC_CALL}, false);

        ASTNode_add_child(node, ASTNode_new((Token){.type = VAL_SEQ}, true));

        FINISH_PARSE(node);
    }
}

ParseResult parse_subscript_postfix(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(LBRACKET);
        MATCH_PARSE(expr_res, parse_expr(idx), "expression in subscript");
        MATCH_TOKEN_WITH_TYPE(RBRACKET);

        ASTNode node = ASTNode_new((Token){.type = ARRAY_SUBSCRIPT}, false);

        ASTNode_add_child(node, expr_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_postfix(int idx) {

    START_PARSE {
        MATCH_RETURN_IF_PARSED(parse_attr_postfix(idx));
        MATCH_RETURN_IF_PARSED(parse_func_call_postfix(idx));
        MATCH_RETURN_IF_PARSED(parse_subscript_postfix(idx));


        FINISH_PARSE(null(ASTNode));
    }
}
// .b.c.d
ParseResult parse_postfix_seq(int idx) {
    START_PARSE {
        MATCH_PARSE(postfix_res, parse_postfix(idx), "postfix operator");

        if (is_null_ast(postfix_res.node)) FINISH_PARSE(null(ASTNode));

        while (true) {
            MATCH_PARSE(res, parse_postfix(idx), "NEVER"); // we know this can't fail, so no need for TRY_MATCH_PARSE
            if (is_null_ast(res.node)) break;

            array_insert(res.node.children, postfix_res.node, 0);

            postfix_res.node = res.node;
        }

        FINISH_PARSE(postfix_res.node);
    }
}

void _make_ast_complete(ASTNode *ast) {
    ast->complete = true;
    for (int i = 0; i < array_length(ast->children); i++) {
        _make_ast_complete(&ast->children[i]);
    }
}

ParseResult parse_primary(int idx) {

    START_PARSE {
        MATCH_PARSE(base_rule_res, parse_base_rule(idx), "base");

        MATCH_PARSE(postfix_seq_res, parse_postfix_seq(idx), "postfix sequence");

        if (is_null_ast(postfix_seq_res.node)) FINISH_PARSE(base_rule_res.node);

        ASTNode node = postfix_seq_res.node;
    
        ASTNode *leaf_parent = &node;
        while (!leaf_parent->children[0].complete) {
            leaf_parent = &leaf_parent->children[0];
        }

        _make_ast_complete(&node);

        array_insert(leaf_parent->children, base_rule_res.node, 0);
        
        FINISH_PARSE(node);
    }
}

ParseResult parse_unary_rule(int idx);

ParseResult parse_unary_minus(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(OP_SUB);
        MATCH_PARSE(unary_res, parse_unary_rule(idx), "value after unary minus");
        ASTNode node = ASTNode_new((Token){.type = OP_UNARY_MINUS}, true);
        ASTNode_add_child(node, unary_res.node);
        FINISH_PARSE(node);
    }
}

ParseResult parse_unary_not(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(OP_NOT);
        MATCH_PARSE(unary_res, parse_unary_rule(idx), "value after unary not");
        ASTNode node = ASTNode_new((Token){.type = OP_NOT}, true);
        ASTNode_add_child(node, unary_res.node);
        FINISH_PARSE(node);
    }
}

ParseResult parse_type_conversion_prefix(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(LPAREN);
        MATCH_PARSE(type_res, parse_type(idx, NULL), "type");
        MATCH_TOKEN_WITH_TYPE(RPAREN);
        MATCH_PARSE(unary_res, parse_unary_rule(idx), "value after type conversion prefix");
        ASTNode node = ASTNode_new((Token){.type = OP_CONVERT_TYPE}, true);
        ASTNode_add_child(node, type_res.node);
        ASTNode_add_child(node, unary_res.node);
        FINISH_PARSE(node);
        
    }
}

ParseResult parse_unary_rule(int idx) {
    
    START_PARSE {

        MATCH_RETURN_IF_PARSED(parse_primary(idx));
        
        MATCH_RETURN_IF_PARSED(parse_type_conversion_prefix(idx));

        MATCH_RETURN_IF_PARSED(parse_unary_minus(idx));
        
        MATCH_RETURN_IF_PARSED(parse_unary_not(idx));


        MATCH_TOKEN(false);
    }
}

ParseResult _parse_assign(int idx) {
    START_PARSE {
        int name_idx = idx;
        MATCH_TOKEN_WITH_TYPE(NAME);

        MATCH_TOKEN_WITH_TYPE(OP_ASSIGN);

        MATCH_PARSE(expr_res, parse_expr(idx), "expression");

        ASTNode node = ASTNode_new((Token){.type = OP_ASSIGN}, true);
        ASTNode_add_child(node, ASTNode_new(get_token(name_idx), true));
        ASTNode_add_child(node, expr_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_assign_seq(int idx) {
    START_PARSE {
        MATCH_PARSE(assign_res, _parse_assign(idx), "assignment");

        ASTNode node = ASTNode_new((Token){.type = ASSIGN_SEQ}, true);

        ASTNode_add_child(node, assign_res.node);

        while (true) {

            if (get_token(idx).type != COMMA) break;

            MATCH_TOKEN_WITH_TYPE(COMMA);


            TRY_MATCH_PARSE(res, _parse_assign(idx));
            if (!res.success) break;

            ASTNode_add_child(node, res.node);
        }

        FINISH_PARSE(node);
    }
}

ParseResult parse_new_rule(int idx) {

    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("new");

        bool is_struct;

        int name_idx = idx;
        MATCH_PARSE(type_res, parse_type(idx, &is_struct), "type");

        if (!is_struct) {
            print_err("Can't use 'new' keyword on a primitive type! if you want it on the heap, wrap it in a struct");
            MATCH_TOKEN(false);
        }

        MATCH_TOKEN_WITH_TYPE(LPAREN);

        TRY_MATCH_PARSE(assign_seq_res, parse_assign_seq(idx));

        if (assign_seq_res.success) {
            MATCH_TOKEN_WITH_TYPE(RPAREN);

            ASTNode node = ASTNode_new((Token){.type = OP_NEW}, true);
            ASTNode_add_child(node, ASTNode_new(get_token(name_idx), true));
            ASTNode_add_child(node, assign_seq_res.node);

            FINISH_PARSE(node);
        }

        MATCH_TOKEN_WITH_TYPE(RPAREN);

        ASTNode node = ASTNode_new((Token){.type = OP_NEW}, true);
        ASTNode_add_child(node, ASTNode_new(get_token(name_idx), true));
        ASTNode_add_child(node, ASTNode_new((Token){.type = ASSIGN_SEQ}, true));

        FINISH_PARSE(node);
    }
}

ParseResult parse_delete_stmt(int idx) {

    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("delete");
        MATCH_PARSE(primary_res, parse_primary(idx), "variable");
        MATCH_TOKEN_WITH_TYPE(STMT_END);

        ASTNode node = ASTNode_new((Token){.type = DELETE_STMT}, true);
        ASTNode_add_child(node, primary_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_for_stmt(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("for");
        MATCH_TOKEN_WITH_TYPE(LPAREN);
        MATCH_PARSE(init_stmt, parse_stmt(idx), "first statement in for");
        MATCH_PARSE(cond_expr, parse_expr(idx), "boolean expression");
        MATCH_TOKEN_WITH_TYPE(STMT_END);
        MATCH_PARSE(update_stmt, parse_stmt(idx), "last statement in for");
        MATCH_TOKEN_WITH_TYPE(RPAREN);
        MATCH_PARSE(code_stmt, parse_stmt(idx), "statement after for");
        ASTNode node = ASTNode_new((Token){.type = BLOCK}, true);
        ASTNode_add_child(node, init_stmt.node);

        ASTNode while_node = ASTNode_new((Token){.type = WHILE_STMT}, true);
        ASTNode_add_child(while_node, cond_expr.node);

        if (code_stmt.node.token.type != BLOCK) {
            code_stmt.node = wrap_with_block(code_stmt.node);
        }

        ASTNode_add_child(code_stmt.node, update_stmt.node);

        ASTNode_add_child(while_node, code_stmt.node);

        ASTNode_add_child(node, while_node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_array_literal(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(LBRACKET);
        TRY_MATCH_PARSE(val_seq_res, parse_val_seq(idx));
        MATCH_TOKEN_WITH_TYPE(RBRACKET);
        
        ASTNode node = ASTNode_new((Token){.type = ARRAY_LITERAL}, true);
        if (val_seq_res.success) {
            ASTNode_add_child(node, val_seq_res.node);
        } else {
            ASTNode_add_child(node, ASTNode_new((Token){.type = VAL_SEQ}, true));
        }

        FINISH_PARSE(node);
    }
}

ParseResult parse_dimension_seq(int idx) {
    START_PARSE {
        MATCH_PARSE(expr_res, parse_expr(idx), "expression in dimension sequence");
        
        ASTNode node = ASTNode_new((Token){.type = DIMENSION_SEQ}, true);

        ASTNode_add_child(node, expr_res.node);

        while (true) {

            if (get_token(idx).type != STMT_END) break;

            MATCH_TOKEN_WITH_TYPE(STMT_END);

            TRY_MATCH_PARSE(res, parse_expr(idx));

            if (!res.success) break;

            ASTNode_add_child(node, res.node);
        }

        FINISH_PARSE(node);

    }
}


ParseResult parse_array_initializer(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(LBRACKET);
        MATCH_PARSE(dim_seq_res, parse_dimension_seq(idx), "sequence of dimensions");
        MATCH_TOKEN_WITH_TYPE(RBRACKET);

        ASTNode node = ASTNode_new((Token){.type = ARRAY_INITIALIZER}, false);
        ASTNode_add_child(node, dim_seq_res.node);


        FINISH_PARSE(node);
    }
}

ParseResult parse_name_seq(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(NAME);
        ASTNode node = ASTNode_new((Token){.type = NAME_SEQ}, true);

        ASTNode_add_child(node, ASTNode_new(get_token(idx - 1), true));

        while (true) {

            if (get_token(idx).type != COMMA) break;
            MATCH_TOKEN_WITH_TYPE(COMMA);

            if (get_token(idx).type != NAME) break;
            MATCH_TOKEN_WITH_TYPE(NAME);

            ASTNode_add_child(node, ASTNode_new(get_token(idx - 1), true));
        }

        FINISH_PARSE(node);
    }

}

ParseResult parse_lambda(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(LPAREN);
        TRY_MATCH_PARSE(name_seq, parse_name_seq(idx));
        MATCH_TOKEN_WITH_TYPE(RPAREN);
        MATCH_TOKEN_WITH_TYPE(ARROW);
        TRY_MATCH_PARSE(expr_res, parse_expr(idx));
        
        ASTNode node = ASTNode_new((Token){.type = LAMBDA}, true);

        if (name_seq.success) {
            ASTNode_add_child(node, name_seq.node);
        } else {
            ASTNode_add_child(node, ASTNode_new((Token){.type = NAME_SEQ}, true));
        }

        if (expr_res.success) {
            ASTNode return_stmt = ASTNode_new((Token){.type = RETURN_STMT}, true);
            ASTNode_add_child(return_stmt, expr_res.node);
            ASTNode block = ASTNode_new((Token){.type = BLOCK}, true);
            ASTNode_add_child(block, return_stmt);
            ASTNode_add_child(node, block);
        } else {
            MATCH_PARSE(block_res, parse_block(idx), "a block or an expression");
            ASTNode_add_child(node, block_res.node);
        }

        FINISH_PARSE(node);

    }
}

// #PARSE END


// #RULES
/*
Rules:
<if-stmt> -> if ( <expr> ) <stmt> | if ( <expr> ) <stmt> else <stmt>

<while-stmt> -> while ( <expr> ) <stmt>
<declare-and-assign-stmt> -> <typename> <name> = <expr>;
<declare-stmt> -> <typename> <name>;

<func-decl> -> <typename> <name>(<func-args>) <stmt>

<func-arg> -> <typename> <name>
<func-args> -> <func-arg> <func-args> | , <func-arg> <func-args> | epsilon 

<func-call> -> <attr-rule>(<val-seq>);

<typename> -> one of a list of allowed types
<name> -> sequence of characters which is NOT defined as a variable, doesnt start with [0-9], allowed characters: [a-z][A-Z]_[0-9]
<assign-stmt> -> <variable> = <expr>;

<stmt> -> <if-stmt> | <while-stmt> | ... | <block>
<block> -> { <stmt-seq> }
<stmt-seq> -> <stmt> | <stmt> <stmt-seq>

<val-seq> -> <expr> | <expr>, <val-seq>

<print-stmt> -> print <val-seq>;

<input-stmt> -> input <name>;

<assign_seq> -> <assign_stmt> | <assign_stmt> <assign_seq>
<new_rule> -> new <typename> (<assign_seq>)


<expr> -> <and_rule> <'expr>
<'expr> -> || <and_rule> <'expr> | epsilon
<and_rule> -> <rel_rule> <'and_rule>
<'and_rule> -> && <rel_rule> <'and_rule> | epsilon
<rel_rule> -> <add_rule> <'rel_rule>
<'rel_rule> -> (== | != | > | ...) <add_rule> <'rel_rule> | epsilon
<add_rule> -> <mul_rule> <'add_rule>
<'add_rule> -> + <mul_rule> <'add_rule> | - <mul_rule> <'add_rule> | epsilon
<mul_rule> -> <unary_rule> <'mul_rule>
<'mul_rule> -> * <unary_rule> <'mul_rule> | / <unary_rule> <'mul_rule> | epsilon

<unary_rule> -> <primary> | !<unary_rule> | -<unary_rule>

<primary> -> <base_rule> <postfix-seq>

<base_rule> -> <value> | ( <expr> )

<postfix> -> .<name> | (<val-seq>)
<postfix-seq> -> <postfix> <postfix-seq> | epsilon

<value> -> <bool> | <literal> | <variable> | <int> | <float>
<bool> -> true | false | maybe


()
!
* /
+ -
== != > >= < <=
&&
||

1 * (2 * 3)
*/

// IR:
/*
>int x = (9 + 7.31 * 2.71828) - (3 - 9.0);
>>> RESULT AST <<<
[STMT_SEQ]
|---[DECL_ASSIGN_STMT]
|---|---[TYPE, int]
|---|---[NAME, x]
|---|---[OP_SUB]
|---|---|---[OP_ADD]
|---|---|---|---[INTEGER, 9]
|---|---|---|---[OP_MUL]
|---|---|---|---|---[FLOAT, 7.31]
|---|---|---|---|---[FLOAT, 2.72]
|---|---|---[OP_SUB]
|---|---|---|---[INTEGER, 3]
|---|---|---|---[FLOAT, 9.00]
x: 0

STACK_ALLOC 4
PUSH (0, int) // x pos
PUSH (9, int)
PUSH (7.31, float)
PUSH (2.72, float)
MUL
ADD
PUSH (3, int)
PUSH (9.0, float)
SUB
SUB
STACK_STORE
*/

VarHeader *lookup_var_safe(LinkedList *var_map_list, String name, bool *global);

VarHeader *lookup_var(LinkedList *var_map_list, String name, bool *global);

void add_varheader_to_map_list(LinkedList *var_map_list, VarHeader *vh);

// Types will convert to the highest precedence type in a collision
int get_type_precedence(Type *type) {
    if (type == NULL) return -1;

    match (type->kind) {
        
        case (TYPE_null_ref)
            return 0;
        
        case (TYPE_bool) 
            return 1;

        case (TYPE_char)
            return 2;
        
        case (TYPE_int) 
            return 3;
        
        case (TYPE_float) 
            return 4;
        
        case (TYPE_str) 
            return 5;
        
        case (TYPE_struct) 
            return 5;
        
        case (TYPE_array)
            return 5;
        
        default () return -1;
    }
}

VarHeader *find_attr_in_struct(VarHeader *struct_vh, String attr_name) {
    
    if (struct_vh->type != VH_STRUCT) {
        print_err("Tried to find attribute in non struct! (a %s)", vh_type_names[struct_vh->type]);
    }
    
    for (int i = 0; i < array_length(struct_vh->struct_members); i++) {
        if (String_equal(struct_vh->struct_members[i].name, attr_name)) return &struct_vh->struct_members[i];
    }

    print_err("Couldn't find attribute '%s' in struct '%s'!", attr_name.data, struct_vh->name.data);
    return NULL;
}

VarHeader *get_args_from_func_decl_ast(ASTNode *ast) {
    VarHeader *args = array(VarHeader, 2);

    ASTNode *func_args = ast;

    int offset = 0;

    for (int i = 0; i < array_length(func_args->children); i++) {
        ASTNode *child = &func_args->children[i];
        Type *type = child->children[0].token.var_type;
        String var_name = child->children[1].token.text;

        VarHeader vh = create_var_header(var_name, type, offset);
        offset += get_vartype_size(type);

        array_append(args, vh);
    }

    return args;
}

#define RETURN_PATHS_OK 1
#define UNREACHABLE_CODE 0
#define MISSING_RETURN_PATHS -1
#define RETURN_FROM_VOID_FUNCTION -2

bool is_valid_type_conversion(Type *from, Type *to);

// TODO: A BLOCK CAN BE A BLOCKER!
int _validate_return_paths(ASTNode *ast, Type *return_type) {

    if (ast->token.type != BLOCK) {
        print_err("I don't know what to do here! look into this further. node type: '%s'", token_type_names[ast->token.type]);
        return false;
    }
    for (int i = 0; i < array_length(ast->children); i++) {
        ASTNode *child = &ast->children[i];
        if (child->token.type == WHILE_STMT) continue;
        if (child->token.type == RETURN_STMT && is_valid_type_conversion(child->children[0].expected_return_type, return_type)) {
            if (i < array_length(ast->children) - 1) return UNREACHABLE_CODE;
            return RETURN_PATHS_OK;
        }
        if (child->token.type == IF_ELSE_STMT) {

            int r_if = _validate_return_paths(&child->children[1], return_type);
            int r_else = _validate_return_paths(&child->children[2], return_type);

            int m = min(r_if, r_else);
            if (m > MISSING_RETURN_PATHS) {
                if (i < array_length(ast->children) - 1) return UNREACHABLE_CODE;
                return m;
            }
        }
        if (child->token.type == BLOCK) {
            int r = _validate_return_paths(child, return_type);
            if (r > MISSING_RETURN_PATHS) return r;
        }
    }

    return MISSING_RETURN_PATHS;
}

bool has_returns_with_values(ASTNode *ast) {
    for (int i = 0; i < array_length(ast->children); i++) {
        ASTNode *child = &ast->children[i];
        if (child->token.type == RETURN_STMT && array_length(child->children) > 0) return true;
        if (has_returns_with_values(child)) return true;
    }

    return false;
}

int validate_return_paths(ASTNode *ast) {
    if (ast->token.type != FUNC_DECL_STMT) {
        print_err("Invalid call to validate_return_paths()!");
        return false;
    }
    Type *target_type = ast->children[0].token.var_type;

    if (target_type->kind == TYPE_void) return has_returns_with_values(ast) ? RETURN_FROM_VOID_FUNCTION : RETURN_PATHS_OK;

    return _validate_return_paths(&ast->children[3], target_type);
}
bool _func_vh_args_equal(VarHeader *args1, VarHeader *args2) {
    if (array_length(args1) != array_length(args2)) return false;

    for (int i = 0; i < array_length(args1); i++) {
        if (args1[i].var_type != args2[i].var_type) return false;
    }

    return true;
}

void try_set_temp_array_subtype(ASTNode *node, Type *subtype) {
    if (node->token.type != ARRAY_LITERAL && node->token.type != ARRAY_INITIALIZER) return;

    ASTNode_insert_child(
        (*node), 
        ASTNode_new((Token){.type = TYPE, .var_type = copy_type(subtype)}, true),
        0
    );
    node->complete = true;
}

void try_set_lambda_type(ASTNode *node, Type *target_type) {

    assert(target_type->kind == TYPE_func);

    if (node->token.type != LAMBDA) return;

    int expected_arg_count = array_length(target_type->func_data.arg_types);
    int arg_count = array_length(node->children[0].children);

    if (expected_arg_count != arg_count) return_err(
        "Lambda args don't match its type's args! Expected %d arguments, found %d",
        expected_arg_count,
        arg_count
    );

    node->expected_return_type = copy_type(target_type);
}

void apply_type_inference(ASTNode *node, Type *target_type) {
    match(target_type->kind) {
        case (TYPE_array) {
            try_set_temp_array_subtype(node, target_type->array_data.type);
        }
        case (TYPE_func) {
            try_set_lambda_type(node, target_type);
        }
    }
}



// #INTRINSICS

#define INTRINSICS \
    X(none_) \
    X(clear_terminal_lines) \
    X(rand) \
    X(sleep) \
    X(end_)
    
    
#define intr_start() (INTR_none_ + 1)

#define intr_end() (INTR_end_)

#define intr_count() (INTR_end_ - 1)

typedef enum {
    #define X(a) INTR_##a,
    INTRINSICS
    #undef X
} IntrKind;

char *intrinsic_names[] = {
    #define X(a) #a,
    INTRINSICS
    #undef X
};

Type *intrinsic_func_types[intr_count()] = {0};

void init_intrinsic_func_types() {
    intrinsic_func_types[INTR_clear_terminal_lines] = make_func_type(
        make_type(TYPE_void),
        array_from_literal(Type *, {
            make_type(TYPE_int)
        })
    );
    intrinsic_func_types[INTR_rand] = make_func_type(
        make_type(TYPE_int),
        array(Type *, 0)
    );
    intrinsic_func_types[INTR_sleep] = make_func_type(
        make_type(TYPE_void),
        array_from_literal(Type *, {
            make_type(TYPE_int)
        })
    );

    for (int i = intr_start(); i < intr_end(); i++) {
        if (!intrinsic_func_types[i]) {
            print_err("Type not initialized for intrinsic '%s'!", intrinsic_names[i]);
            exit(EXIT_FAILURE);
        }
    }

}

void free_intrinsic_func_types() {
    for (int i = intr_start(); i < intr_end(); i++) {
        free_type(intrinsic_func_types[i]);
    }
}

VarHeader *get_intrinsic_varheaders() {
    VarHeader *res = array(VarHeader, intr_count());

    for (int i = intr_start(); i < intr_end(); i++) {
        VarHeader vh = create_var_header(
            StringRef(intrinsic_names[i]),
            intrinsic_func_types[i],
            -i
        );

        array_append(res, vh);
    }

    return res;
}

int get_intrinsic_by_name(String name) {

    int len = sizeof(intrinsic_names) / sizeof(char *);

    for (int i = 0; i < len; i++)
        if (String_equal(name, StringRef(intrinsic_names[i])))
            return i;

    return INTR_none_;
}

Type *get_func_type_from_ast(ASTNode *node) {

    if (node->token.type != FUNC_DECL_STMT) {
        print_err("Tried to get func type from a non-function declaration!");
        return NULL;
    }

    ASTNode *type_node = &node->children[0];
    ASTNode *func_args = &node->children[2];

    Type **arg_types = array(Type *, 3);
    for (int i = 0; i < array_length(func_args->children); i++) {
        array_append(arg_types, copy_type(func_args->children[i].children[0].token.var_type));
    }

    Type *return_type = copy_type(type_node->token.var_type);

    return make_func_type(move(return_type), move(arg_types));
}

// anything this function doesn't touch is meant to return void
void typeify_tree(ASTNode *node, LinkedList *var_map_list) {

    static ASTNode *current_func = NULL;


    match (node->token.type) {
        case(BLOCK) {

            LL_prepend(var_map_list, LLNode_new(HashMap(VarHeader)));

            for (int i = 0; i < array_length(node->children); i++) {
                typeify_tree(&node->children[i], var_map_list);
            }

            HashMap *vm = LL_pop_head(var_map_list);
            HashMap_free(vm);
        }
        case(DECL_STMT, DECL_ASSIGN_STMT) {

            String var_name = node->children[1].token.text;
            Type *type = node->children[0].token.var_type;
            ASTNode *expr_node = &node->children[2];
            
            VarHeader vh = create_var_header(var_name, type, -1);
            add_varheader_to_map_list(var_map_list, &vh);

            if (node->token.type == DECL_ASSIGN_STMT) {
                apply_type_inference(expr_node, type);
            }

            for (int i = 0; i < array_length(node->children); i++) {
                typeify_tree(&node->children[i], var_map_list);
            }
        }
        case (ASSIGN_STMT) {
            ASTNode *left_side = &node->children[0];
            ASTNode *right_side = &node->children[1];
            typeify_tree(left_side, var_map_list);

            apply_type_inference(right_side, left_side->expected_return_type);
            typeify_tree(right_side, var_map_list);
        }
        case(INTEGER) {
            node->expected_return_type = make_type(TYPE_int);
        }
        case (CHAR) {
            node->expected_return_type = make_type(TYPE_char);
        }
        case(FLOAT) {
            node->expected_return_type = make_type(TYPE_float);
        }
        case(BOOL) {
            node->expected_return_type = make_type(TYPE_bool);
        }
        case(STRING_LITERAL) {
            node->expected_return_type = make_type(TYPE_str);
        }
        case(NULL_REF) {
            node->expected_return_type = make_type(TYPE_null_ref);
        }
        case(NAME) {
            VarHeader *vh = lookup_var(var_map_list, node->token.text, NULL);
            if (!vh) 
                return_err("Identifier '%s' Doesn't exist within the current scope!", node->token.text.data);
            
            node->expected_return_type = copy_type(vh->var_type);
        }
        case(FUNC_DECL_STMT) {
            ASTNode func_args = node->children[2];

            String func_name = node->children[1].token.text;

            if (get_intrinsic_by_name(func_name) != INTR_none_) return_err(
                "Can't declare function '%s', as that is an intrinsic function!",
                func_name.data
            );

            if (lookup_var_safe(var_map_list, func_name, NULL)) return_err(
                "Tried to redefine function '%s'!",
                func_name
            );

            Type *t = get_func_type_from_ast(node);

            VarHeader vh = create_var_header(
                func_name,
                move(t),
                -1
            );

            add_varheader_to_map_list(var_map_list, &vh);

            LL_prepend(var_map_list, LLNode_new(HashMap(VarHeader)));
            
            int len = array_length(func_args.children);
            for (int i = 0; i < len; i++) {
                
                StringRef var_name = func_args.children[i].children[1].token.text;
                
                Type *type = func_args.children[i].children[0].token.var_type;
                
                VarHeader vh = create_var_header(var_name, type, -1);

                add_varheader_to_map_list(var_map_list, &vh);
            }


            ASTNode *func_scope = &node->children[3];

            ASTNode *prev_func = current_func;
            current_func = node;

            // this ensures we don't copy the hashmap twice because of the scope
            for (int i = 0; i < array_length(func_scope->children); i++) {
                typeify_tree(&func_scope->children[i], var_map_list);
            }

            current_func = prev_func;

            HashMap *vm = LL_pop_head(var_map_list);
            HashMap_free(vm);

            int result = validate_return_paths(node);
            
            match (result) {
                case(UNREACHABLE_CODE) 
                    return_err(
                        "Return might cause unreachable code in function '%s()'!", 
                        func_name.data
                    );
                
                case(MISSING_RETURN_PATHS) 
                    return_err(
                        "Not all return paths return type '%s' in function '%s()'!", 
                        type_kind_names[node->children[0].token.var_type->kind], 
                        func_name.data
                    );    
                
                case(RETURN_FROM_VOID_FUNCTION) 
                    return_err(
                        "Tried to return a value from '%s()', which returns void!", 
                        func_name.data
                    );
            }

        }
        case (STRUCT_DECL_STMT) {
            String name = node->children[0].token.text;
            ASTNode members = node->children[1];
    
            VarHeader *arr = array(VarHeader, 2);
    
            int offset = sizeof(ObjectHeader);
    
            for (int i = 0; i < array_length(members.children); i++) {
                
                String var_name = members.children[i].children[1].token.text;
                Type *var_type = members.children[i].children[0].token.var_type;
                
                VarHeader vh = create_var_header(var_name, var_type, offset);
    
                array_append(arr, vh);
                offset += get_vartype_size(var_type);
            }
    
            VarHeader vh = create_struct_header(name, arr, offset, -1);
    
            add_varheader_to_map_list(var_map_list, &vh);
        }
        case (ATTR_ACCESS) {
            
            ASTNode *left_side = &node->children[0];
            ASTNode *attr_node = &node->children[1];

            if (left_side->token.type == ARRAY_LITERAL) return_err(
                "Cannot access attribute of ambiguous array literal!"
            );
            
            typeify_tree(left_side, var_map_list);
            Type *left_type = left_side->expected_return_type;

            if (left_type->kind == TYPE_array) {
                // assume it's the length attribute, verify at IR stage
                node->expected_return_type = make_type(TYPE_int);
                return;
            }
            if (left_type->kind == TYPE_str) {
                // same shit, might change in the future so im keeping them seperate
                node->expected_return_type = make_type(TYPE_int);
                return;
            }



            if (left_type->kind != TYPE_struct) return_err(
                "Tried accessing attribute in primitive type '%s'!", 
                type_get_name(left_type).data
            );
            
            
            String attr_name = attr_node->token.text;

            String struct_name = type_get_name(left_type);

            VarHeader *struct_header = lookup_var_safe(var_map_list, struct_name, NULL);

            if (!struct_header) return_err(
                "Tried accessing attribute of struct '%s' which is not defined!", 
                struct_name.data
            );
            

            VarHeader *attr_header = find_attr_in_struct(struct_header, attr_name);

            node->expected_return_type = attr_header->var_type;
        }

        case (OP_NEW) {
            String struct_name = node->children[0].token.text;
            VarHeader *struct_vh = lookup_var_safe(var_map_list, struct_name, NULL);
            if (!struct_vh) return_err(
                "can't create new '%s', as that struct is not defined!", 
                struct_name.data
            );
            

            node->expected_return_type = make_struct_type(struct_name);


            ASTNode *assign_seq = &node->children[1];
            for (int i = 0; i < array_length(assign_seq->children); i++) {
                ASTNode *member = &assign_seq->children[i].children[0];
                ASTNode *expr = &assign_seq->children[i].children[1];

                VarHeader *member_vh = find_attr_in_struct(struct_vh, member->token.text);

                member->expected_return_type = copy_type(member_vh->var_type);

                apply_type_inference(expr, member->expected_return_type);
                
                typeify_tree(expr, var_map_list);
            }
        }

        case (OP_UNARY_MINUS) {
            typeify_tree(&node->children[0], var_map_list);
            node->expected_return_type = node->children[0].expected_return_type;    
        }

        case (FUNC_CALL) {

            typeify_tree(&node->children[0], var_map_list);

            ASTNode *func_node = &node->children[0];

            if (func_node->expected_return_type->kind != TYPE_func) return_err(
                "Tried to call a non-function! Expected func type, got '%s'",
                type_get_name(func_node->expected_return_type).data
            );
            
            Type *func_type = func_node->expected_return_type;
            
            ASTNode *args_ast = &node->children[1];
    
            int arg_count = array_length(args_ast->children);

            if (arg_count != array_length(func_type->func_data.arg_types)) return_err(
                "Passed invalid number of arguments to function! Expected %d arguments, found %d",
                array_length(func_type->func_data.arg_types),
                arg_count
            );

            for (int i = 0; i < arg_count; i++) {
                ASTNode *child = &args_ast->children[i];
                apply_type_inference(child, func_type->func_data.arg_types[i]);
                typeify_tree(child, var_map_list);

                if (!is_valid_type_conversion(child->expected_return_type, func_type->func_data.arg_types[i]))
                    return_err(
                        "Passed invalid argument to function! Expected arg %d to be of type '%s', but found type '%s'",
                        i,
                        type_get_name(func_type->func_data.arg_types[i]).data,
                        type_get_name(child->expected_return_type).data
                    );
            }

            node->expected_return_type = copy_type(func_type->func_data.return_type);
        }

        case (ARRAY_LITERAL) {

            assert(array_length(node->children) == 2);

            ASTNode *type_node = &node->children[0];
            node->expected_return_type = make_array_type(copy_type(type_node->token.var_type));

            ASTNode *values_node = &node->children[1];

            for (int i = 0; i < array_length(values_node->children); i++) {
                ASTNode *child = &values_node->children[i];
                apply_type_inference(child, type_node->token.var_type);
                typeify_tree(child, var_map_list);
            }


        }
        case (ARRAY_INITIALIZER) {
            ASTNode *dims = &node->children[1];
            Type *t = make_array_type(copy_type(node->children[0].token.var_type));
            for (int i = 0; i < array_length(dims->children); i++) {
                typeify_tree(&dims->children[i], var_map_list);
                if (dims->children[i].expected_return_type->kind != TYPE_int)
                    return_err("Dimension value must be of type int!");
            }

            node->expected_return_type = move(t);

        }
        case (ARRAY_SUBSCRIPT) {

            if (node->children[0].token.type == ARRAY_LITERAL) return_err(
                "Cannot index into ambiguous array literal! Try casting it to a specific type first. "
            );
            

            typeify_tree(&node->children[0], var_map_list);
            typeify_tree(&node->children[1], var_map_list);

            ASTNode *left = &node->children[0];
            ASTNode *subscript_expr = &node->children[1];

            if (left->expected_return_type->kind == TYPE_str) {
                node->expected_return_type = make_type(TYPE_char);
                return;
            }

            if (left->expected_return_type->kind != TYPE_array)
                return_err("Can't subscript a value of type '%s'! maybe later with traits..", type_get_name(left->expected_return_type).data);
            
            if (subscript_expr->expected_return_type->kind != TYPE_int) 
                return_err(
                    "Array subscript expression must be of type 'int'! found '%s' instead!",
                    type_get_name(subscript_expr->expected_return_type).data
                );
            
            node->expected_return_type = copy_type(left->expected_return_type->array_data.type);            
            
        }

        case (RETURN_STMT) {
            if (!current_func) return_err(
                "Tried to return outside of a function!"
            );

            if (array_length(node->children) == 0) return;

            Type *return_type;

            if (current_func->token.type == FUNC_DECL_STMT) {
                return_type = current_func->children[0].token.var_type;
            } else { // lambda
                return_type = current_func->expected_return_type->func_data.return_type;
            }

            apply_type_inference(&node->children[0], return_type);            
            
            typeify_tree(&node->children[0], var_map_list);
        }
        case (OP_CONVERT_TYPE) {
            
            node->expected_return_type = copy_type(node->children[0].token.var_type);

            apply_type_inference(&node->children[1], node->expected_return_type);

            typeify_tree(&node->children[1], var_map_list);
            
        
        }

        case (LAMBDA) {

            if (node->expected_return_type->kind == TYPE_void) return_err(
                "Ambiguous use of lambda expression! Try converting it to a type first."
            );
            

            ASTNode *prev_func = current_func;
            current_func = node;

            LL_prepend(var_map_list, LLNode_new(HashMap(VarHeader)));

            ASTNode *block = &node->children[1];
            ASTNode *args = &node->children[0];

            for (int i = 0; i < array_length(args->children); i++) {
                ASTNode *arg = &args->children[i];
                Type *t = node->expected_return_type->func_data.arg_types[i];

                VarHeader vh = create_var_header(arg->token.text, t, -1);

                add_varheader_to_map_list(var_map_list, &vh);
            }


            // Since lambdas have to be pure to keep them static and avoid the need for closures, they have a declaration scope seperate from the rest

            LinkedList *limited_list = LL_new();
            LL_prepend(limited_list, LLNode_new(HashMap_copy(var_map_list->head->val)));
            limited_list->head->next = NULL;
            limited_list->tail = limited_list->head;

            for (int i = 0; i < array_length(block->children); i++) {
                ASTNode *child = &block->children[i];

                typeify_tree(child, limited_list);

            }

            _HashMap_free(LL_pop_head(limited_list));

            LL_free(limited_list);


            _HashMap_free(LL_pop_head(var_map_list));

            current_func = prev_func;
        }

        default() {
            if (in_range(node->token.type, ARITHOPS_START, ARITHOPS_END)) {
                Type *highest_precedence_type = NULL;
                int len = array_length(node->children);
                for (int i = 0; i < len; i++) {
                    typeify_tree(&node->children[i], var_map_list);
                    if (get_type_precedence(node->children[i].expected_return_type) > get_type_precedence(highest_precedence_type)) {
                        highest_precedence_type = node->children[i].expected_return_type;
                    }
                }
                node->expected_return_type = move(highest_precedence_type); // #MAYBENULL
            } else if (in_range(node->token.type, BOOLOPS_START, BOOLOPS_END)) {
        
                node->expected_return_type = make_type(TYPE_bool);
        
                int len = array_length(node->children);
                for (int i = 0; i < len; i++) {
                    typeify_tree(&node->children[i], var_map_list);
                }
            } else {
                int len = array_length(node->children);
                for (int i = 0; i < len; i++) {
                    typeify_tree(&node->children[i], var_map_list);
                }
            } 
        }
    }
}


// very simple defer, TODO: make it return-aware (and breaks/continues should work when i add them)
void move_defers_to_end(ASTNode *node) {
    ASTNode *defers = array(ASTNode, 2);

    //                 changes during loop VVV
    for (int i = 0; i < array_length(node->children); i++) {
        if (node->children[i].token.type == DEFER_STMT) {
            array_append(defers, node->children[i]);
            array_remove(node->children, i);
            i--; // because we deleted an item from the array while iterating over it (what could possibly go wrong)
        }
    }
    
    int defer_count = array_length(defers);
    for (int i = 0; i < defer_count; i++) {
        // we dont actually care about the defer, only about it's statement
        ASTNode_add_child((*node), defers[i].children[0]); 
    }
    
    // guranteed to be the same
    // child_count = array_length(node->children);
    int child_count = array_length(node->children);
    
    for (int i = 0; i < child_count; i++) {
        move_defers_to_end(&node->children[i]);
    }


    array_free(defers);
}

void node_clear_children(ASTNode *node) {
    for (int i = array_length(node->children) - 1; i >= 0; i--) {
        free_ast(node->children[i]);
        array_remove(node->children, i);
    }

}

bool _is_foldable(ASTNode *node) {
    if (
        node->token.type != INTEGER
        && node->token.type != FLOAT 
        && !in_range(node->token.type, ARITHOPS_START, ARITHOPS_END)
        && node->token.type != OP_UNARY_MINUS
    ) return false;

    int len = array_length(node->children);
    for (int i = 0; i < len; i++) {
        if (!_is_foldable(&node->children[i])) return false;
    }

    return true;
}

void _fold(ASTNode *node) {

    if (node->token.type == INTEGER || node->token.type == FLOAT) return;

    int len = array_length(node->children);
    for (int i = 0; i < len; i++) {
        _fold(&node->children[i]);
    }

    #define BINOP_LOGIC(op) if (node->children[0].token.type == INTEGER && node->children[1].token.type == INTEGER) { \
        node->token.type = INTEGER; \
        node->token.as_int = node->children[0].token.as_int op node->children[1].token.as_int; \
        node_clear_children(node); \
    } else { \
        double num1, num2; \
        if (node->children[0].token.type == INTEGER) num1 = (double)node->children[0].token.as_int; \
        else num1 = node->children[0].token.as_double; \
        if (node->children[1].token.type == INTEGER) num2 = (double)node->children[1].token.as_int; \
        else num2 = node->children[1].token.as_double; \
        node->token.type = FLOAT; \
        node->token.as_double = num1 op num2; \
        node_clear_children(node); \
    }

    #define BINOP_MOD_LOGIC() if (node->children[0].token.type == INTEGER && node->children[1].token.type == INTEGER) { \
        node->token.type = INTEGER; \
        node->token.as_int = node->children[0].token.as_int % node->children[1].token.as_int; \
        node_clear_children(node); \
    } else { \
        double num1, num2; \
        if (node->children[0].token.type == INTEGER) num1 = (double)node->children[0].token.as_int; \
        else num1 = node->children[0].token.as_double; \
        if (node->children[1].token.type == INTEGER) num2 = (double)node->children[1].token.as_int; \
        else num2 = node->children[1].token.as_double; \
        node->token.type = FLOAT; \
        node->token.as_double = fmod(num1, num2); \
        node_clear_children(node); \
    }

    switch (node->token.type) {
        case (OP_ADD)
            BINOP_LOGIC(+)
            break;
        case (OP_SUB)
            BINOP_LOGIC(-)
            break;
        case (OP_DIV)
            BINOP_LOGIC(/)
            break;
        case (OP_MUL)
            BINOP_LOGIC(*)
            break;
        case (OP_MOD)
            BINOP_MOD_LOGIC()
            break;
        case (OP_UNARY_MINUS)
            if (node->expected_return_type->kind == TYPE_int) {
                node->token.type = INTEGER;
                node->token.as_int = node->children[0].token.as_int;
                node->token.as_int *= -1;
                node_clear_children(node);
            } else {
                node->token.type = FLOAT;
                node->token.as_double = node->children[0].token.as_double;
                node->token.as_double *= -1;
                node_clear_children(node);
            }
            break;
        default:
            print_err("Unsupported fold operation! Tried folding '%s' token!", token_type_names[node->token.type]);
            break;
    }

    #undef BINOP_LOGIC
    #undef BINOP_MOD_LOGIC
}

void apply_constant_folding(ASTNode *node) {
    if (_is_foldable(node)) {
        _fold(node);
        return;
    }

    int len = array_length(node->children);
    for (int i = 0; i < len; i++) {
        apply_constant_folding(&node->children[i]);
    }
}

typedef struct {
    ASTNode *parent;
    int child_idx;
} LambdaRef;

void _get_lambdas_in_tree(ASTNode *node, LambdaRef **refs) {
    for (int i = 0; i < array_length(node->children); i++) {
        ASTNode *child = &node->children[i];
        if (child->token.type == LAMBDA) {
            LambdaRef ref = {.child_idx = i, .parent = node};
            array_append(*refs, ref);
        } else {
            _get_lambdas_in_tree(child, refs);
        }
    }
}

ASTNode make_func_decl_from_lambda(ASTNode *lambda, String func_name) {
    ASTNode node = ASTNode_new((Token){.type = FUNC_DECL_STMT}, true);

    Type *return_type = lambda->expected_return_type->func_data.return_type;
    Type **arg_types = lambda->expected_return_type->func_data.arg_types;

    ASTNode_add_child(node, ASTNode_new((Token){.type = TYPE, .var_type = copy_type(move(return_type))}, true));

    ASTNode_add_child(node, ASTNode_new((Token){.type = NAME, .text = func_name}, true));

    ASTNode_add_child(node, ASTNode_new((Token){.type = FUNC_ARGS_SEQ}, true));

    ASTNode func_args_seq = node.children[2];

    int len = array_length(lambda->children[0].children);
    assert(len == array_length(arg_types));

    for (int i = 0; i < len; i++) {
        ASTNode func_arg = ASTNode_new((Token){.type = FUNC_ARG}, true);

        ASTNode_add_child(func_arg, ASTNode_new((Token){.type = TYPE, .var_type = copy_type(arg_types[i])}, true));

        ASTNode name = ASTNode_new((Token){.type = NAME, .text = lambda->children[0].children[i].token.text}, true);
        ASTNode_add_child(func_arg, name);

        ASTNode_add_child(func_args_seq, func_arg);
    }

    ASTNode_add_child(node, ASTNode_copy(&lambda->children[1]));

    return node;
}

void convert_lambdas_to_anonymous_funcs(ASTNode *node) {
    // 1. find all lambdas recursively, store them in a list
    // 2. for each lambda store its parent and index (so it can be removed)
    // 3. iterate over lambdas, add a function for each one, give it a name, 
    //    go to the lambda mention and replace it with a reference to the anonymous func

    LambdaRef *refs = array(LambdaRef, 10);

    ASTNode *func_decls = array(ASTNode, 10);

    _get_lambdas_in_tree(node, &refs);

    for (int i = 0; i < array_length(refs); i++) {
        String name = String_concatf(String("$"), String_from_int(i));

        LambdaRef ref = refs[i];
        ASTNode lambda = ref.parent->children[ref.child_idx];
        ASTNode func_decl_ast = make_func_decl_from_lambda(&lambda, name);

        // insert the func decls later so we don't mess with the indices
        array_append(func_decls, func_decl_ast);

        ASTNode ident = ASTNode_new((Token){.type = NAME, .text = name}, true);
        ident.expected_return_type = copy_type(lambda.expected_return_type);

        ref.parent->children[ref.child_idx] = ident;

        free_ast(lambda);
    }

    // do it in reverse so the func decls are ordered
    for (int i = array_length(func_decls) - 1; i >= 0; i--) {
        ASTNode_insert_child((*node), func_decls[i], 0);
    }

}


void preprocess_ast(ASTNode *ast) {

    LinkedList *vm_list = LL_new();
    LL_prepend(vm_list, LLNode_new(HashMap(VarHeader)));

    VarHeader *intrinsics = get_intrinsic_varheaders();

    for (int i = 0; i < intr_count(); i++) {
        add_varheader_to_map_list(vm_list, &intrinsics[i]);
    }

    array_free(intrinsics);

    typeify_tree(ast, vm_list);


    HashMap_free(vm_list->head->val);
    LL_pop_head(vm_list);
    LL_free(vm_list);

    convert_lambdas_to_anonymous_funcs(ast);

    move_defers_to_end(ast);
    apply_constant_folding(ast);
    // and anything else i might wanna do
}


// #INST
#define INSTRUCTIONS \
X(I_INVALID) \
X(I_PUSH) \
X(I_PUSH_MAYBE) \
X(I_PUSH_RAND) \
X(I_READ) \
X(I_READ_GLOBAL) \
X(I_ADD) \
X(I_ADD_FLOAT) \
X(I_SUB) \
X(I_SUB_FLOAT) \
X(I_MUL) \
X(I_MUL_FLOAT) \
X(I_DIV) \
X(I_DIV_FLOAT) \
X(I_MOD) \
X(I_MOD_FLOAT) \
X(I_GREATER) \
X(I_GREATER_FLOAT) \
X(I_GREATER_EQUAL) \
X(I_GREATER_EQUAL_FLOAT) \
X(I_LESS) \
X(I_LESS_FLOAT) \
X(I_LESS_EQUAL) \
X(I_LESS_EQUAL_FLOAT) \
X(I_EQUAL) \
X(I_EQUAL_FLOAT) \
X(I_EQUAL_BOOL) \
X(I_EQUAL_STR) \
X(I_EQUAL_REF) \
X(I_NOT_EQUAL) \
X(I_NOT_EQUAL_FLOAT) \
X(I_NOT_EQUAL_BOOL) \
X(I_NOT_EQUAL_STR) \
X(I_NOT_EQUAL_REF) \
X(I_AND) \
X(I_OR) \
X(I_NOT) \
X(I_LABEL) \
X(I_JUMP) \
X(I_JUMP_IF) \
X(I_JUMP_NOT) \
X(I_CONVERT_START) \
X(I_CONVERT_BOOL_INT) \
X(I_CONVERT_BOOL_FLOAT) \
X(I_CONVERT_BOOL_STR) \
X(I_CONVERT_INT_FLOAT) \
X(I_CONVERT_INT_STR) \
X(I_CONVERT_INT_BOOL) \
X(I_CONVERT_INT_CHAR) \
X(I_CONVERT_CHAR_INT) \
X(I_CONVERT_FLOAT_STR) \
X(I_CONVERT_FLOAT_INT) \
X(I_CONVERT_FLOAT_BOOL) \
X(I_CONVERT_STR_BOOL) \
X(I_CONVERT_STR_INT) \
X(I_CONVERT_STR_FLOAT) \
X(I_CONVERT_END) \
X(I_PRINT_START) \
X(I_PRINT_INT) \
X(I_PRINT_FLOAT) \
X(I_PRINT_CHAR) \
X(I_PRINT_STR) \
X(I_PRINT_CHAR_ARRAY) /*same as print_str, but since they have different offsets ill make different instructions*/ \
X(I_PRINT_BOOL) \
X(I_PRINT_NEWLINE) \
X(I_PRINT_END) \
X(I_INPUT) \
X(I_STACK_STORE) \
X(I_STACK_STORE_GLOBAL) \
X(I_HEAP_STORE) \
X(I_STORE_STACK_PTR) \
X(I_SET_STACK_PTR) \
X(I_STACK_PTR_ADD) \
X(I_RETURN) \
X(I_CALL) \
X(I_READ_ATTR) \
X(I_GET_ATTR_ADDR) \
X(I_ALLOC) \
X(I_ALLOC_DYNAMIC) \
X(I_FREE) \
X(I_DUP) \
X(I_INC_REFCOUNT) \
X(I_DEC_REFCOUNT) \
X(I_DEC_REFCOUNT_ARRAY) \
X(I_INIT_OBJ_HEADER) \
X(I_INIT_ARRAY_HEADER) \
X(I_TUCK) \
X(I_POP_BOTTOM) \
X(I_NOP) \
X(I_STACK_STORE_FUNC_ADDR) \
X(I_ARRAY_SUBSCRIPT) \
X(I_ARRAY_SUBSCRIPT_ADDR) \
X(I_STR_SUBSCRIPT) \
X(I_INIT_N_DIM_ARRAY) \
X(I_CLEAR_TERMI_LINES) \
X(I_SLEEP) \
X(INST_COUNT)



// This took 2.5 hours. Worth every second
#define valify(a) , Val a
#define REST(a, ...) __VA_ARGS__

#define _call_create_inst(type, a, b, ...) create_inst(type, a, b)

#define DEF_CREATE_INST(inst, ...) \
    Inst create_inst_##inst(EVAL1(REST SEP (MAP(valify, __VA_ARGS__)))) { \
        return _call_create_inst(inst, __VA_ARGS__, Val_null); \
    } 
#define DEF_CREATE_INST_NOARGS(inst) \
    Inst create_inst_##inst(void) { \
        return _call_create_inst(inst, Val_null, Val_null); \
    } 

typedef enum InstType {
    #define X(i) i, 
    INSTRUCTIONS
    #undef X
} InstType;

char *inst_names[] = {
    #define X(i) #i,
    INSTRUCTIONS
    #undef X
};


Inst create_inst(InstType type, Val arg1, Val arg2) {
    return (Inst){.type = type, .arg1 = arg1, .arg2 = arg2};
}

DEF_CREATE_INST(I_PUSH, size, value)
DEF_CREATE_INST_NOARGS(I_PUSH_MAYBE)
DEF_CREATE_INST_NOARGS(I_PUSH_RAND)
DEF_CREATE_INST(I_READ, size, pos)
DEF_CREATE_INST(I_READ_GLOBAL, size, pos)
DEF_CREATE_INST_NOARGS(I_ADD)
DEF_CREATE_INST_NOARGS(I_ADD_FLOAT)
DEF_CREATE_INST_NOARGS(I_SUB)
DEF_CREATE_INST_NOARGS(I_SUB_FLOAT)
DEF_CREATE_INST_NOARGS(I_MUL)
DEF_CREATE_INST_NOARGS(I_MUL_FLOAT)
DEF_CREATE_INST_NOARGS(I_DIV)
DEF_CREATE_INST_NOARGS(I_DIV_FLOAT)
DEF_CREATE_INST_NOARGS(I_MOD)
DEF_CREATE_INST_NOARGS(I_MOD_FLOAT)
DEF_CREATE_INST_NOARGS(I_GREATER)
DEF_CREATE_INST_NOARGS(I_GREATER_FLOAT)
DEF_CREATE_INST_NOARGS(I_GREATER_EQUAL)
DEF_CREATE_INST_NOARGS(I_GREATER_EQUAL_FLOAT)
DEF_CREATE_INST_NOARGS(I_LESS)
DEF_CREATE_INST_NOARGS(I_LESS_FLOAT)
DEF_CREATE_INST_NOARGS(I_LESS_EQUAL)
DEF_CREATE_INST_NOARGS(I_LESS_EQUAL_FLOAT)
DEF_CREATE_INST_NOARGS(I_EQUAL)
DEF_CREATE_INST_NOARGS(I_EQUAL_FLOAT)
DEF_CREATE_INST_NOARGS(I_EQUAL_BOOL)
DEF_CREATE_INST_NOARGS(I_EQUAL_STR)
DEF_CREATE_INST_NOARGS(I_EQUAL_REF)
DEF_CREATE_INST_NOARGS(I_NOT_EQUAL)
DEF_CREATE_INST_NOARGS(I_NOT_EQUAL_FLOAT)
DEF_CREATE_INST_NOARGS(I_NOT_EQUAL_BOOL)
DEF_CREATE_INST_NOARGS(I_NOT_EQUAL_STR)
DEF_CREATE_INST_NOARGS(I_NOT_EQUAL_REF)
DEF_CREATE_INST_NOARGS(I_AND)
DEF_CREATE_INST_NOARGS(I_OR)
DEF_CREATE_INST_NOARGS(I_NOT)
DEF_CREATE_INST_NOARGS(I_LABEL)
DEF_CREATE_INST(I_JUMP, pos)
DEF_CREATE_INST(I_JUMP_IF, pos)
DEF_CREATE_INST(I_JUMP_NOT, pos)
DEF_CREATE_INST_NOARGS(I_CONVERT_BOOL_INT)
DEF_CREATE_INST_NOARGS(I_CONVERT_BOOL_FLOAT)
DEF_CREATE_INST_NOARGS(I_CONVERT_BOOL_STR)
DEF_CREATE_INST_NOARGS(I_CONVERT_INT_FLOAT)
DEF_CREATE_INST_NOARGS(I_CONVERT_INT_STR)
DEF_CREATE_INST_NOARGS(I_CONVERT_INT_BOOL)
DEF_CREATE_INST_NOARGS(I_CONVERT_INT_CHAR)
DEF_CREATE_INST_NOARGS(I_CONVERT_CHAR_INT)
DEF_CREATE_INST_NOARGS(I_CONVERT_FLOAT_STR)
DEF_CREATE_INST_NOARGS(I_CONVERT_FLOAT_INT)
DEF_CREATE_INST_NOARGS(I_CONVERT_FLOAT_BOOL)
DEF_CREATE_INST_NOARGS(I_CONVERT_STR_BOOL)
DEF_CREATE_INST_NOARGS(I_CONVERT_STR_INT)
DEF_CREATE_INST_NOARGS(I_CONVERT_STR_FLOAT)
DEF_CREATE_INST_NOARGS(I_PRINT_INT)
DEF_CREATE_INST_NOARGS(I_PRINT_FLOAT)
DEF_CREATE_INST_NOARGS(I_PRINT_CHAR)
DEF_CREATE_INST_NOARGS(I_PRINT_STR)
DEF_CREATE_INST_NOARGS(I_PRINT_BOOL)
DEF_CREATE_INST_NOARGS(I_PRINT_NEWLINE)
DEF_CREATE_INST_NOARGS(I_INPUT)
DEF_CREATE_INST(I_STACK_STORE, size, pos)
DEF_CREATE_INST(I_STACK_STORE_GLOBAL, size, pos)
DEF_CREATE_INST(I_HEAP_STORE, size)
DEF_CREATE_INST(I_STACK_PTR_ADD, n)
DEF_CREATE_INST_NOARGS(I_RETURN)
DEF_CREATE_INST_NOARGS(I_CALL)
DEF_CREATE_INST(I_READ_ATTR, size, offset)
DEF_CREATE_INST(I_GET_ATTR_ADDR, offset)
DEF_CREATE_INST(I_ALLOC, bytes)
DEF_CREATE_INST_NOARGS(I_ALLOC_DYNAMIC)
DEF_CREATE_INST_NOARGS(I_DUP)
DEF_CREATE_INST_NOARGS(I_INC_REFCOUNT)
DEF_CREATE_INST_NOARGS(I_DEC_REFCOUNT)
DEF_CREATE_INST_NOARGS(I_DEC_REFCOUNT_ARRAY)
DEF_CREATE_INST(I_INIT_OBJ_HEADER, meta)
DEF_CREATE_INST(I_INIT_ARRAY_HEADER, meta)
DEF_CREATE_INST_NOARGS(I_TUCK)
DEF_CREATE_INST_NOARGS(I_POP_BOTTOM)
DEF_CREATE_INST_NOARGS(I_NOP)
DEF_CREATE_INST(I_ARRAY_SUBSCRIPT, elem_size)
DEF_CREATE_INST(I_ARRAY_SUBSCRIPT_ADDR, elem_size)
DEF_CREATE_INST_NOARGS(I_STR_SUBSCRIPT)
DEF_CREATE_INST(I_INIT_N_DIM_ARRAY, n_dims)
DEF_CREATE_INST_NOARGS(I_CLEAR_TERMI_LINES)
DEF_CREATE_INST_NOARGS(I_SLEEP)
DEF_CREATE_INST(I_STACK_STORE_FUNC_ADDR, addr, pos)

void print_val(Val val) {
    printf("(%s, ", type_kind_names[val.type]);
    switch (val.type) {
        case (TYPE_int)
            printf("%d", val.as_int);
        case (TYPE_char)
            printf("'%c'", val.as_char);
        case (TYPE_float)
            printf("%.2f", val.as_double);
        case (TYPE_bool)
            printf("%s", val.as_bool ? "true" : "false");
        case (TYPE_str)
            printf("\"%s\"", val.as_str);
        case (TYPE_struct)
            if (!val.as_ptr) printf("null");
            else printf("%p", val.as_ptr);
        default ()
            printf("print_val() is unimplemented for type '%s'!", type_kind_names[val.type]);
    }
    printf(")");
}

void print_instruction(Inst inst) {
    printf("[%s", inst_names[inst.type]);
    
    switch (inst.type) {

        case (I_STACK_STORE_FUNC_ADDR) {
            printf(", func_idx: #%d, pos: %d", inst.arg1.as_int, inst.arg2.as_int);
        }
        case (I_PUSH)
            printf(", ");
            print_val(inst.arg2);

        case (I_JUMP_NOT, I_JUMP_IF, I_JUMP)
            printf(", #%d", inst.arg1.as_int);

        case (I_STACK_PTR_ADD)
            printf(", %d", inst.arg1.as_int);

        case (I_READ, I_STACK_STORE)
            printf(", sz: %d, pos: fp + %d", inst.arg1.as_int, inst.arg2.as_int);

        case (I_READ_GLOBAL, I_STACK_STORE_GLOBAL)
            printf(", sz: %d, pos: %d", inst.arg1.as_int, inst.arg2.as_int);

        case (I_READ_ATTR)
            printf(", sz: %d, offset: %d", inst.arg1.as_int, inst.arg2.as_int);

        case (I_GET_ATTR_ADDR)
            printf(", offset: %d", inst.arg1.as_int);

        case (I_HEAP_STORE, I_ALLOC)
            printf(", sz: %d", inst.arg1.as_int);
    }
    printf("] \n");
}

void print_instructions(Inst *arr) {
    printf("Instructions: \n");
    for (int i = 0; i < array_length(arr); i++) {
        printf("%d: ", i);
        print_instruction(arr[i]);
    }
}



InstType _get_cvt_inst_type_for_types(Type *from, Type *to) {
    
    if (types_are_equal(from, to)) return I_NOP;
    
    // avert your eyes
    if (from->kind == TYPE_bool) {
        if (to->kind == TYPE_int) return I_CONVERT_BOOL_INT;
        if (to->kind == TYPE_float) return I_CONVERT_BOOL_FLOAT;
        if (to->kind == TYPE_str) return I_CONVERT_BOOL_STR;
    }
    if (from->kind == TYPE_int) {
        if (to->kind == TYPE_bool) return I_CONVERT_INT_BOOL;
        if (to->kind == TYPE_float) return I_CONVERT_INT_FLOAT;
        if (to->kind == TYPE_str) return I_CONVERT_INT_STR;
        if (to->kind == TYPE_char) return I_CONVERT_INT_CHAR;
    }
    if (from->kind == TYPE_char) {
        if (to->kind == TYPE_int) return I_CONVERT_CHAR_INT;
    }
    if (from->kind == TYPE_float) {
        if (to->kind == TYPE_bool) return I_CONVERT_FLOAT_BOOL;
        if (to->kind == TYPE_int) return I_CONVERT_FLOAT_INT;
        if (to->kind == TYPE_str) return I_CONVERT_FLOAT_STR;
    }
    if (from->kind == TYPE_str) {
        if (to->kind == TYPE_bool) return I_CONVERT_STR_BOOL;
        if (to->kind == TYPE_int) return I_CONVERT_STR_INT;
        if (to->kind == TYPE_float) return I_CONVERT_STR_FLOAT;
    }
    if (from->kind == TYPE_null_ref) {
        if (to->kind == TYPE_struct) return I_NOP;
        if (to->kind == TYPE_array) return I_NOP;
        if (to->kind == TYPE_str) return I_NOP;
    }


    return I_INVALID;
}

bool is_valid_type_conversion(Type *from, Type *to) {
    return _get_cvt_inst_type_for_types(from, to) != I_INVALID;
}

bool generate_cvt_inst_for_types(Type *from, Type *to, Inst **instructions) {
    InstType inst = _get_cvt_inst_type_for_types(from, to);
    if (inst == I_INVALID) return false;
    if (inst == I_NOP) return true;

    array_append(*instructions, create_inst(inst, Val_null, Val_null));
    return true;
}

InstType get_inst_type_for_op(TokenType op, Type *var_type) {

    TypeKind kind = var_type->kind;

    if (op == OP_ADD) {
        if (kind == TYPE_int) return I_ADD;
        if (kind == TYPE_float) return I_ADD_FLOAT;
    }
    if (op == OP_SUB) {
        if (kind == TYPE_int) return I_SUB;
        if (kind == TYPE_float) return I_SUB_FLOAT;
    }
    if (op == OP_MUL) {
        if (kind == TYPE_int) return I_MUL;
        if (kind == TYPE_float) return I_MUL_FLOAT;
    }
    if (op == OP_DIV) {
        if (kind == TYPE_int) return I_DIV;
        if (kind == TYPE_float) return I_DIV_FLOAT;
    }
    if (op == OP_MOD) {
        if (kind == TYPE_int) return I_MOD;
        if (kind == TYPE_float) return I_MOD_FLOAT;
    }
    if (op == OP_GREATER) {
        if (kind == TYPE_int) return I_GREATER;
        if (kind == TYPE_float) return I_GREATER_FLOAT;
    }
    if (op == OP_GREATEREQ) {
        if (kind == TYPE_int) return I_GREATER_EQUAL;
        if (kind == TYPE_float) return I_GREATER_EQUAL_FLOAT;
    }
    if (op == OP_LESS) {
        if (kind == TYPE_int) return I_LESS;
        if (kind == TYPE_float) return I_LESS_FLOAT;
    }
    if (op == OP_LESSEQ) {
        if (kind == TYPE_int) return I_LESS_EQUAL;
        if (kind == TYPE_float) return I_LESS_EQUAL_FLOAT;
    }
    if (op == OP_EQ) {
        if (kind == TYPE_int) return I_EQUAL;
        if (kind == TYPE_float) return I_EQUAL_FLOAT;
        if (kind == TYPE_bool) return I_EQUAL_BOOL;
        if (kind == TYPE_str) return I_EQUAL_STR;
        if (is_reference_typekind(kind)) return I_EQUAL_REF;
    }
    if (op == OP_NOTEQ) {
        if (kind == TYPE_int) return I_NOT_EQUAL;
        if (kind == TYPE_float) return I_NOT_EQUAL_FLOAT;
        if (kind == TYPE_bool) return I_NOT_EQUAL_BOOL;
        if (kind == TYPE_str) return I_NOT_EQUAL_STR;
        if (is_reference_typekind(kind)) return I_NOT_EQUAL_REF;
    }
    if (op == OP_AND && kind == TYPE_bool) {
        return I_AND;
    }
    if (op == OP_OR && kind == TYPE_bool) {
        return I_OR;
    }

    return I_INVALID;
}

InstType get_dec_ref_inst_by_typekind(TypeKind kind) {
    match (kind) {
        case (TYPE_array) return I_DEC_REFCOUNT_ARRAY;
        case (TYPE_struct) return I_DEC_REFCOUNT;
        default() print_err("There is no dec refcount instruction for typekind '%s'!", type_kind_names[kind]);
                  *(int *)0 = 1;
    }
    return I_INVALID;
}



int gi_stack_pos = 0;
int gi_label_idx = 0;
ASTNode *gi_current_function = NULL;


VarHeader *lookup_var_safe(LinkedList *var_map_list, String name, bool *global) {
    if (global) *global = false;
    LLNode *current = var_map_list->head;
    while (current != NULL) {
        VarHeader *vh = HashMap_get_safe(current->val, name, NULL);
        if (vh != NULL) {
            if (!current->next && global) *global = true;
            return vh;
        }
        current = current->next;
    }
    return NULL;
}

VarHeader *lookup_var(LinkedList *var_map_list, String name, bool *global) {
    VarHeader *result = lookup_var_safe(var_map_list, name, global);
    if (!result) 
        print_err("Identifier '%s' doesn't exist within the current scope!", name.data);
    
    return result;
}


void add_varheader_to_map_list(LinkedList *var_map_list, VarHeader *vh) {
    HashMap *map = var_map_list->head->val;
    HashMap_put(map, vh->name, vh);
}


//#INST GEN START

int calc_stack_space_for_scope(ASTNode *ast) {
    if (ast->token.type == DECL_ASSIGN_STMT || ast->token.type == DECL_STMT) return get_vartype_size(ast->children[0].token.var_type);
    if (ast->token.type == FUNC_DECL_STMT) return get_typekind_size(TYPE_func); // because thats a seperate scope

    int sum = 0;
    int len = array_length(ast->children);
    for (int i = 0; i < len; i++) {
        sum += calc_stack_space_for_scope(&ast->children[i]);
    }

    return sum;
}

void generate_instructions_for_node(ASTNode *ast, Inst **instructions, LinkedList *var_map_list);

static inline bool is_reference_typekind(TypeKind kind) {
    return kind == TYPE_struct || kind == TYPE_array;
}

static inline bool is_reference(ASTNode *ast) {

    if (is_null_ast(*ast)) return false;

    return is_reference_typekind(ast->expected_return_type->kind);
}

static inline bool is_temporary_reference(ASTNode *ast) {

    if (is_null_ast(*ast)) return false;


    return ast->token.type == OP_NEW
        || ast->token.type == ARRAY_LITERAL
        || ast->token.type == ARRAY_INITIALIZER
        || (
            (ast->token.type == FUNC_CALL || ast->token.type == OP_CONVERT_TYPE) 
            && is_reference_typekind(ast->expected_return_type->kind));
}

static inline bool is_nontemporary_reference(ASTNode *ast) {
    return is_reference(ast) && !is_temporary_reference(ast);
}

void generate_instructions_for_vardecl(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    String var_name = ast->children[1].token.text;

    Type *type = ast->children[0].token.var_type;
    
    VarHeader vh = create_var_header(var_name, type, gi_stack_pos);

    add_varheader_to_map_list(var_map_list, &vh);
    
    int size = get_vartype_size(type);

    
    if (ast->token.type == DECL_ASSIGN_STMT) {
        generate_instructions_for_node(&ast->children[2], instructions, var_map_list);

        Type *child_return_type = ast->children[2].expected_return_type;
        
        if (!types_are_equal(child_return_type, type)) {
            bool result = generate_cvt_inst_for_types(child_return_type, type, instructions);
            if (!result) return_err(
                    "Invalid conversion on variable declaration! Tried to convert from type '%s' to '%s'", 
                    type_get_name(child_return_type).data, 
                    type_get_name(type).data
                );
            
        }
    } else {
        Val val = Val_null;
        val.type = type->kind;
        array_append(*instructions, create_inst_I_PUSH(Val_int(get_vartype_size(type)), val));
    }

    bool inc_refcounter = (ast->token.type == DECL_ASSIGN_STMT) && is_nontemporary_reference(&ast->children[2]);

    if (inc_refcounter) {
        array_append(*instructions, create_inst_I_DUP());
    }
        
    array_append(*instructions, create_inst_I_STACK_STORE(Val_int(get_vartype_size(type)), Val_int(gi_stack_pos)));
    gi_stack_pos += size;


    if (inc_refcounter) {
        array_append(*instructions, create_inst_I_INC_REFCOUNT());
    }


}

void generate_instructions_for_attr_addr(ASTNode *ast, Inst **instructions, LinkedList *var_map_list);

void generate_instructions_for_array_subscript_addr(ASTNode *ast, Inst **instructions, LinkedList *var_map_list);

// #UPDATE FOR STRUCTS
void generate_instructions_for_assign(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    ASTNode *left_side = &ast->children[0];
    ASTNode *right_side = &ast->children[1];


    // RC

    bool dec_refcount_for_left_side = is_reference(left_side);

    if (dec_refcount_for_left_side) {
        generate_instructions_for_node(left_side, instructions, var_map_list);
        array_append(*instructions, create_inst_I_TUCK()); // decrement always after increment
    }

    bool inc_refcount_for_right_side = is_nontemporary_reference(right_side);

    if (left_side->token.type == NAME) {
        String var_name = left_side->token.text;
            
        bool isglobal;
        
        VarHeader *vh = lookup_var(var_map_list, var_name, &isglobal);

        generate_instructions_for_node(right_side, instructions, var_map_list);

        if (inc_refcount_for_right_side) {
            array_append(*instructions, create_inst_I_DUP());
            array_append(*instructions, create_inst_I_INC_REFCOUNT());
        }
        
        Type *goal_type = vh->var_type;

        if (!types_are_equal(goal_type, right_side->expected_return_type)) {
            bool result = generate_cvt_inst_for_types(right_side->expected_return_type, goal_type, instructions);
            if (!result) return_err(
                "Invalid conversion on assignment! Tried to convert from type '%s' to '%s'", 
                type_get_name(right_side->expected_return_type).data, 
                type_get_name(goal_type).data
            );
        }

        array_append(*instructions, create_inst(isglobal? I_STACK_STORE_GLOBAL : I_STACK_STORE, Val_int(get_vartype_size(vh->var_type)), Val_int(vh->var_pos)));

    } else {
        Type *goal_type = left_side->expected_return_type;

        match (left_side->token.type) {
            case (ATTR_ACCESS)
                generate_instructions_for_attr_addr(left_side, instructions, var_map_list);
            case (ARRAY_SUBSCRIPT)
                generate_instructions_for_array_subscript_addr(left_side, instructions, var_map_list);
        }

        generate_instructions_for_node(right_side, instructions, var_map_list);

        if (inc_refcount_for_right_side) {
            array_append(*instructions, create_inst_I_DUP());
            array_append(*instructions, create_inst_I_INC_REFCOUNT());
        }


        if (!types_are_equal(goal_type, right_side->expected_return_type)) {
            bool result = generate_cvt_inst_for_types(right_side->expected_return_type, goal_type, instructions);
            if (!result) return_err(
                "Invalid conversion on assignment! Tried to convert from type '%s' to '%s'", 
                type_get_name(right_side->expected_return_type).data, 
                type_get_name(goal_type).data
            );
        }

        array_append(*instructions, create_inst_I_HEAP_STORE(Val_int(get_vartype_size(left_side->expected_return_type))));

    }

    if (dec_refcount_for_left_side) {
        array_append(*instructions, create_inst_I_POP_BOTTOM());
        array_append(*instructions, create_inst(get_dec_ref_inst_by_typekind(left_side->expected_return_type->kind), Val_null, Val_null));
    }
    
    
}


void generate_instructions_for_binop(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    int len = array_length(ast->children);

    bool is_bool_op = in_range(ast->token.type, BOOLOPS_START, BOOLOPS_END);
    bool is_pure_bool_op = ast->token.type == OP_AND || ast->token.type == OP_OR;

    Type *highest_prec_type = ast->children[0].expected_return_type;

    if (is_bool_op) {
        for (int i = 1; i < len; i++) {
            if (get_type_precedence(ast->children[i].expected_return_type) > get_type_precedence(highest_prec_type)) {
                highest_prec_type = ast->children[i].expected_return_type;
            }
        }
    }

    Type *goal_type = is_pure_bool_op ? &_const_types[TYPE_bool] : (is_bool_op ? highest_prec_type : ast->expected_return_type);

    // any binary operation on chars is the same as ints
    if (goal_type->kind == TYPE_char) goal_type = &_const_types[TYPE_int];

    for (int i = 0; i < len; i++) {

        ASTNode *child = &ast->children[i];

        generate_instructions_for_node(child, instructions, var_map_list);

        if (!types_are_equal(goal_type, child->expected_return_type)) {
            bool result = generate_cvt_inst_for_types(child->expected_return_type, goal_type, instructions);
            if (!result) return_err(
                    "Invalid conversion in binary operation! Tried to convert between '%s' and '%s'!",
                    type_get_name(child->expected_return_type).data,
                    type_get_name(goal_type).data
                );
            
        }
    }
    InstType inst_type = get_inst_type_for_op(ast->token.type, goal_type);
    if (inst_type == I_INVALID) {
        print_err("Invalid operator!");
        printf("Tried to get operator '%s' between '%s' type operands! \n", token_type_names[ast->token.type], type_get_name(goal_type).data);
    } else {
        array_append(*instructions, create_inst(inst_type, Val_null, Val_null));
    }

}

InstType get_print_inst_for_type(Type *type) {
    match (type->kind) {
        case (TYPE_int) 
            return I_PRINT_INT;
        
        case (TYPE_float) 
            return I_PRINT_FLOAT;
        
        case (TYPE_bool) 
            return I_PRINT_BOOL;
        
        case (TYPE_str) 
            return I_PRINT_STR;
        
        case (TYPE_char)
            return I_PRINT_CHAR;

        case (TYPE_array) 
            if (type->array_data.type->kind == TYPE_char) 
                return I_PRINT_CHAR_ARRAY;
            else
                return I_INVALID;
        
        default ()
            return I_INVALID;
    }
}

void generate_instructions_for_print(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    int len = array_length(ast->children);
    for (int i = 0; i < len; i++) {

        generate_instructions_for_node(&ast->children[i], instructions, var_map_list);

        InstType inst_type = get_print_inst_for_type(ast->children[i].expected_return_type);
        if (inst_type == I_INVALID) {
            print_err("Invalid argument for print! (seriously how could you mess this up)");
            printf("argument type: %s \n", type_get_name(ast->children[i].expected_return_type).data);
        } else {
            array_append(*instructions, create_inst(inst_type, Val_null, Val_null));
        }
    }

    if (ast->token.type == PRINT_STMT) array_append(*instructions, create_inst_I_PRINT_NEWLINE());
}

void generate_instructions_for_if(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    int end_label_idx = gi_label_idx++;

    int end_label_true_idx;
    int else_label_true_idx;
    // if (ast->token.type == IF_ELSE_STMT) else_label_idx = gi_label_idx++;

    // condition
    generate_instructions_for_node(&ast->children[0], instructions, var_map_list);

    if (ast->children[0].expected_return_type->kind != TYPE_bool) {
        Type _t_bool = (Type){.kind = TYPE_bool};
        bool result = generate_cvt_inst_for_types(ast->children[0].expected_return_type, &_t_bool, instructions);
        if (!result) {
            return_err(
                "Type '%s' is ambigous! Cannot be used as an if condition. (you did badly.)",
                type_get_name(ast->children[0].expected_return_type).data
            );
        }
    }
    
    int first_jump_idx = array_length(*instructions);
    array_append(*instructions, create_inst_I_JUMP_NOT(Val_int(-1)));

    // if-body
    generate_instructions_for_node(&ast->children[1], instructions, var_map_list);
    
    int if_body_jump_idx = -1;

    if (ast->token.type == IF_ELSE_STMT) {

        if_body_jump_idx = array_length(*instructions);
        array_append(*instructions, create_inst_I_JUMP(Val_int(end_label_idx)));

        else_label_true_idx = array_length(*instructions);
        array_append(*instructions, create_inst_I_LABEL());

        // else-body
        generate_instructions_for_node(&ast->children[2], instructions, var_map_list);
    }

    end_label_true_idx = array_length(*instructions);
    array_append(*instructions, create_inst_I_LABEL());

    if (ast->token.type == IF_STMT) {
        (*instructions)[first_jump_idx].arg1.as_int = end_label_true_idx;
    } else {
        (*instructions)[first_jump_idx].arg1.as_int = else_label_true_idx;
        (*instructions)[if_body_jump_idx].arg1.as_int = end_label_true_idx;
    }

}

void generate_instructions_for_while(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    int start_label_idx = array_length(*instructions);

    array_append(*instructions, create_inst_I_LABEL());

    generate_instructions_for_node(&ast->children[0], instructions, var_map_list);

    if (ast->children[0].expected_return_type->kind != TYPE_bool) {
        bool result = generate_cvt_inst_for_types(ast->children[0].expected_return_type, &_const_types[TYPE_bool], instructions);

        if (!result) return_err(
            "Type '%s' is ambigous! Cannot be used as a while condition. (you did badly.)",
            type_get_name(ast->children[0].expected_return_type).data
        );
    }

    int jump_not_inst_idx = array_length(*instructions);
    array_append(*instructions, create_inst_I_JUMP_NOT(Val_int(-1)));

    // while body
    generate_instructions_for_node(&ast->children[1], instructions, var_map_list);

    array_append(*instructions, create_inst_I_JUMP(Val_int(start_label_idx)));

    int end_label_idx = array_length(*instructions);
    array_append(*instructions, create_inst_I_LABEL());

    (*instructions)[jump_not_inst_idx].arg1.as_int = end_label_idx;


}

void generate_instructions_for_input(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    ASTNode *left_side = &ast->children[0];

    // RC

    bool dec_refcount_for_left_side = is_reference(left_side);

    if (dec_refcount_for_left_side) {
        generate_instructions_for_node(left_side, instructions, var_map_list);
        array_append(*instructions, create_inst_I_TUCK()); // decrement always after increment
    }

    if (left_side->token.type == NAME) {
        String var_name = left_side->token.text;
            
        bool isglobal;
        
        VarHeader *vh = lookup_var(var_map_list, var_name, &isglobal);
        
        Type *goal_type = vh->var_type;

        array_append(*instructions, create_inst_I_INPUT());

        if (goal_type->kind != TYPE_str) {
            bool result = generate_cvt_inst_for_types(&_const_types[TYPE_str], goal_type, instructions);
            if (!result) return_err(
                "Invalid conversion on assignment! Tried to convert from type 'str' to '%s'",  
                type_get_name(goal_type).data
            );
        }

        array_append(*instructions, create_inst(isglobal? I_STACK_STORE_GLOBAL : I_STACK_STORE, Val_int(get_vartype_size(vh->var_type)), Val_int(vh->var_pos)));

    } else {
        Type *goal_type = left_side->expected_return_type;

        match (left_side->token.type) {
            case (ATTR_ACCESS)
                generate_instructions_for_attr_addr(left_side, instructions, var_map_list);
            case (ARRAY_SUBSCRIPT)
                generate_instructions_for_array_subscript_addr(left_side, instructions, var_map_list);
        }

        array_append(*instructions, create_inst_I_INPUT());

        if (goal_type->kind != TYPE_str) {
            bool result = generate_cvt_inst_for_types(&_const_types[TYPE_str], goal_type, instructions);
            if (!result) return_err(
                "Invalid conversion on assignment! Tried to convert from type 'str' to '%s'", 
                type_get_name(goal_type).data
            );
        }

        array_append(*instructions, create_inst_I_HEAP_STORE(Val_int(get_vartype_size(left_side->expected_return_type))));

    }

    if (dec_refcount_for_left_side) {
        array_append(*instructions, create_inst_I_POP_BOTTOM());
        array_append(*instructions, create_inst(get_dec_ref_inst_by_typekind(left_side->expected_return_type->kind), Val_null, Val_null));
    }
}


bool _get_all_vardecls_before_return(ASTNode *node, ASTNode *return_node, VarHeader ***arr_ptr, LinkedList *var_map_list) {
    
    if (node == return_node) return true;

    bool found = return_node == NULL; // if it is null, just act like it's at the very end of the function


    for (int i = array_length(node->children) - 1; i >= 0; i--) {

        ASTNode *child = &node->children[i];

        if (!found) {
            bool result = _get_all_vardecls_before_return(child, return_node, arr_ptr, var_map_list);
            if (result) {
                found = true;
                continue;
            }
        } else {
            match (child->token.type) {
                case (DECL_ASSIGN_STMT, DECL_STMT);
                    String name = child->children[1].token.text;
                    array_append(*arr_ptr, lookup_var(var_map_list, name, NULL));
            }
        }
    }

    return found;

}

VarHeader **get_all_vardecls_before_return(ASTNode *func_node, ASTNode *return_node, LinkedList *var_map_list) {
    VarHeader **arr = array(VarHeader *, 2);

    ASTNode *func_args_node = &func_node->children[2];

    for (int i = 0; i < array_length(func_args_node->children); i++) {
        ASTNode *arg = &func_args_node->children[i];
        String arg_name = arg->children[1].token.text;

        array_append(arr, lookup_var(var_map_list, arg_name, NULL));
    }

    // int:
    //  #          #
    // ##   ###   ###
    //  #   #  #   #
    // ###  #  #   ##

    bool res = _get_all_vardecls_before_return(func_node, return_node, &arr, var_map_list);
    if (!res && return_node != NULL) {
        print_err("Didn't find the return in the function provided! \n");
        return NULL;
    }

    return arr;
}

void generate_instructions_for_func_decl(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    int func_start_idx = array_length(*instructions) + 2; // current is store_func_addr, +1 is jump, +2 is label
    
    array_append(*instructions, create_inst_I_STACK_STORE_FUNC_ADDR(Val_int(func_start_idx), Val_int(gi_stack_pos)));



    array_append(*instructions, create_inst_I_JUMP(Val_int(-1)));
    int jump_inst_idx = array_length(*instructions) - 1;



    array_append(*instructions, create_inst_I_LABEL());

    
    String func_name = ast->children[1].token.text;
    
    
    VarHeader vh = create_var_header(
        func_name,
        get_func_type_from_ast(ast),
        gi_stack_pos
    );
    
    int var_size = get_typekind_size(TYPE_func);
    
    gi_stack_pos += var_size;
    
    add_varheader_to_map_list(var_map_list, &vh);
    
    
    ASTNode var_args = ast->children[2];

    Type *type = ast->children[0].token.var_type;
    
    LL_prepend(var_map_list, LLNode_new(HashMap(VarHeader)));
    int prev_gi_stack_pos = gi_stack_pos;
    ASTNode *gi_prev_function = gi_current_function;
    gi_stack_pos = 0;
    gi_current_function = ast;
    
    int size = 0;
    ASTNode scope = ast->children[3];
    
    for (int i = array_length(var_args.children) - 1; i >= 0; i--) {
        Type *type = var_args.children[i].children[0].token.var_type;
        size += get_vartype_size(type);
    }
    size += calc_stack_space_for_scope(&scope);
    array_append(*instructions, create_inst_I_STACK_PTR_ADD(Val_int(size)));
    
    for (int i = array_length(var_args.children) - 1; i >= 0; i--) {
        String var_name = var_args.children[i].children[1].token.text;
        Type *type = var_args.children[i].children[0].token.var_type;
        array_append(*instructions, create_inst_I_STACK_STORE(Val_int(get_vartype_size(type)), Val_int(gi_stack_pos)));
        VarHeader vh = create_var_header(var_name, type, gi_stack_pos);
        add_varheader_to_map_list(var_map_list, &vh);
        gi_stack_pos += get_vartype_size(type);
        size += get_vartype_size(type);
    }
    
    int len = array_length(scope.children);
    
    for (int i = 0; i < len; i++) {
        generate_instructions_for_node(&scope.children[i], instructions, var_map_list);
    }
    
    // RC
    {
        VarHeader **vardecls = get_all_vardecls_before_return(ast, NULL, var_map_list);
        
        for (int i = 0; i < array_length(vardecls); i++) {
            if (!is_reference_typekind(vardecls[i]->var_type->kind)) continue;
            
            array_append(
                *instructions, 
                create_inst(I_READ, 
                    Val_int(get_vartype_size(vardecls[i]->var_type)), 
                    Val_int(vardecls[i]->var_pos)
                )
            );
            
            array_append(*instructions, create_inst(get_dec_ref_inst_by_typekind(vardecls[i]->var_type->kind), Val_null, Val_null));
        }
        
        array_free(vardecls);
    }
    
    
    
    
    gi_current_function = gi_prev_function;
    gi_stack_pos = prev_gi_stack_pos;
    HashMap_free(var_map_list->head->val);
    LL_pop_head(var_map_list);
    
    if ((*instructions)[array_length(*instructions) - 1].type != I_RETURN)
    array_append(*instructions, create_inst_I_RETURN());
    
    
    (*instructions)[jump_inst_idx].arg1.as_int = array_length(*instructions);
    
    
}


void generate_instructions_for_intrinsic_func_call(IntrKind intr, ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    #define push_arg(idx, exp_type) \
    { \
        generate_instructions_for_node(&ast->children[1].children[idx], instructions, var_map_list); \
            bool res = generate_cvt_inst_for_types( \
                ast->children[1].children[idx].expected_return_type, \
                exp_type, \
                instructions \
            ); \
            assert(res); \
        }


    match (intr) {
        
        case (INTR_clear_terminal_lines) {
            push_arg(0, &_const_types[TYPE_int]);
            array_append(*instructions, create_inst_I_CLEAR_TERMI_LINES());
        }
        
        case (INTR_rand) {
            array_append(*instructions, create_inst_I_PUSH_RAND());
        }

        case (INTR_sleep) {
            push_arg(0, &_const_types[TYPE_float]);
            array_append(*instructions, create_inst_I_SLEEP());
        }
        
        default () {
            return_err(
                "Unimplemented intrinsic: %s, doing nothing \n",
                intrinsic_names[intr]
            );
        }
    }


    #undef push_arg
}


bool try_gen_insts_for_intrinsic(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    String func_name = ast->children[0].token.text;

    IntrKind kind = get_intrinsic_by_name(func_name);

    if (kind == INTR_none_) return false;

    generate_instructions_for_intrinsic_func_call(kind, ast, instructions, var_map_list);

}
// #INTRINSICS END



// #UPDATE FOR STRUCTS
void generate_instructions_for_func_call(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    ASTNode *func_node = &ast->children[0];
    Type *func_type = func_node->expected_return_type;

    ASTNode *args_node = &ast->children[1];

    int len = array_length(args_node->children);

    for (int i = 0; i < len; i++) {
        ASTNode *arg = &args_node->children[i];
        generate_instructions_for_node(arg, instructions, var_map_list);

        // RC
        if (is_nontemporary_reference(arg)) {
            array_append(*instructions, create_inst_I_DUP());
            array_append(*instructions, create_inst_I_INC_REFCOUNT());            
        }


        Type *goal_type = func_type->func_data.arg_types[i];
        if (!types_are_equal(arg->expected_return_type, goal_type)) {
            bool result = generate_cvt_inst_for_types(arg->expected_return_type, goal_type, instructions);
            if (!result) return_err(
                "Function argument #%d expected type '%s' but got '%s'!",
                i,
                type_get_name(goal_type).data,
                type_get_name(arg->expected_return_type).data
            );
        }
    }

    generate_instructions_for_node(func_node, instructions, var_map_list);

    array_append(*instructions, create_inst_I_CALL());

}

void generate_instructions_for_unary_minus(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    generate_instructions_for_node(&ast->children[0], instructions, var_map_list);

    if (ast->expected_return_type->kind == TYPE_int) {
        array_append(*instructions, create_inst_I_PUSH(Val_int(4), Val_int(-1)));
        array_append(*instructions, create_inst_I_MUL());
    } else if (ast->expected_return_type->kind == TYPE_float) {
        array_append(*instructions, create_inst_I_PUSH(Val_int(8), Val_double(-1)));
        array_append(*instructions, create_inst_I_MUL_FLOAT());
    } else {
        print_err("Cannot apply unary minus to value of type '%s'!", type_get_name(ast->expected_return_type).data);
    }
}

void generate_instructions_for_struct_decl(ASTNode *ast, LinkedList *var_map_list) {
    String name = ast->children[0].token.text;
    ASTNode members = ast->children[1];

    VarHeader *arr = array(VarHeader, 2);

    int offset = sizeof(ObjectHeader);

    RefOffset *ref_offsets = array(RefOffset, 2);

    for (int i = 0; i < array_length(members.children); i++) {
        
        String var_name = members.children[i].children[1].token.text;
        Type *var_type = members.children[i].children[0].token.var_type;
        
        if (is_reference_typekind(var_type->kind)) {
            RefOffset ref_offset = (RefOffset){.is_array = var_type->kind == TYPE_array, .offset = offset};
            array_append(ref_offsets, ref_offset);
        }

        VarHeader vh = create_var_header(var_name, var_type, offset);
    
        array_append(arr, vh);
        offset += get_vartype_size(var_type);
    }

    // {
    //     for (int i = 0; i < array_length(ref_offsets); i++) {
    //         printf("%d \n", ref_offsets[i]);
    //     }
    // }

    StructMetadata meta = {
        .offset_count = array_length(ref_offsets),
        .offsets = ref_offsets,
        .size = offset,
        .struct_name = name
    };

    struct_metadata[struct_metadata_ptr++] = meta;

    VarHeader vh = create_struct_header(name, arr, offset, struct_metadata_ptr - 1);

    add_varheader_to_map_list(var_map_list, &vh);

}

void generate_instructions_for_array_attr_access(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    ASTNode *left_side = &ast->children[0];
    ASTNode *attr_node = &ast->children[1];


    if (!String_equal(attr_node->token.text, StringRef("length"))) {
        return_err(
            "Attribute doesn't exist in arrays! (the only attribute is 'length')"
        );
    }
    
    generate_instructions_for_node(left_side, instructions, var_map_list);
    array_append(*instructions, create_inst_I_READ_ATTR(Val_int(sizeof(int)), Val_int(sizeof(ObjectHeader))));


}

void generate_instructions_for_str_attr_access(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    ASTNode *attr_node = &ast->children[1];
    ASTNode *str_node = &ast->children[0];

    if (!String_equal(attr_node->token.text, StringRef("length"))) return_err(
        "Tried to access invalid attribute in str! The only attribute is length."
    );

    generate_instructions_for_node(str_node, instructions, var_map_list);

    array_append(*instructions, create_inst_I_READ_ATTR(Val_int(sizeof(int)), Val_int(-4)));

    
}

void generate_instructions_for_attr_access(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    if (ast->children[0].expected_return_type->kind == TYPE_array) {
        generate_instructions_for_array_attr_access(ast, instructions, var_map_list);
        return;
    }
    if (ast->children[0].expected_return_type->kind == TYPE_str) {
        generate_instructions_for_str_attr_access(ast, instructions, var_map_list);
        return;
    }


    generate_instructions_for_node(&ast->children[0], instructions, var_map_list);

    bool temp_refcount = is_temporary_reference(&ast->children[0]);

    if (temp_refcount) {
        array_append(*instructions, create_inst_I_DUP());
        array_append(*instructions, create_inst_I_TUCK());
    }

    // find member offset
    VarHeader *struct_vh = lookup_var(var_map_list, type_get_name(ast->children[0].expected_return_type), NULL);

    VarHeader *member_vh = find_attr_in_struct(struct_vh, ast->children[1].token.text);

    array_append(*instructions, create_inst_I_READ_ATTR(Val_int(get_vartype_size(member_vh->var_type)), Val_int(member_vh->var_pos)));

    if (temp_refcount) {
        array_append(*instructions, create_inst_I_POP_BOTTOM());
        array_append(*instructions, create_inst_I_DEC_REFCOUNT());
    }
}
void generate_instructions_for_attr_addr(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    generate_instructions_for_node(&ast->children[0], instructions, var_map_list);

    // find member offset
    VarHeader *struct_vh = lookup_var(var_map_list, type_get_name(ast->children[0].expected_return_type), NULL);

    VarHeader *member_vh = find_attr_in_struct(struct_vh, ast->children[1].token.text);

    array_append(*instructions, create_inst_I_GET_ATTR_ADDR(Val_int(member_vh->var_pos)));

}

void generate_instructions_for_new(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    VarHeader *struct_vh = lookup_var(var_map_list, ast->children[0].token.text, NULL);

    array_append(*instructions, create_inst_I_ALLOC(Val_int(struct_vh->struct_size)));

    array_append(*instructions, create_inst_I_DUP()); // use the allocated address
    array_append(*instructions, 
        create_inst(I_INIT_OBJ_HEADER, Val_int(struct_vh->struct_metadata_idx), 
        Val_null)); // use the allocated address

    ASTNode *member_assigns = &ast->children[1];

    for (int i = 0; i < array_length(member_assigns->children); i++) {
        String member_name = member_assigns->children[i].children[0].token.text;
        VarHeader *member_vh = find_attr_in_struct(struct_vh, member_name);

        array_append(*instructions, create_inst_I_DUP()); // use the allocated address
        array_append(*instructions, create_inst_I_GET_ATTR_ADDR(Val_int(member_vh->var_pos)));

        ASTNode *expr = &member_assigns->children[i].children[1];

        generate_instructions_for_node(expr, instructions, var_map_list);

        if (is_nontemporary_reference(expr)) {
            array_append(*instructions, create_inst_I_DUP());
            array_append(*instructions, create_inst_I_INC_REFCOUNT());
        }

        Type *goal_type = member_vh->var_type;
        Type *curr_type = member_assigns->children[i].children[1].expected_return_type;

        if (!types_are_equal(curr_type, goal_type)) {
            bool result = generate_cvt_inst_for_types(curr_type, goal_type, instructions);
            if (!result) return_err("Couldn't convert type '%s' to type '%s' in attribute assignment!", 
                    type_get_name(curr_type).data,
                    type_get_name(goal_type).data
            );
        }

        array_append(*instructions, create_inst_I_HEAP_STORE(Val_int(get_vartype_size(member_vh->var_type))));

    }

}

// #DEPRECATED
void generate_instructions_for_delete(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    ASTNode *thing = &ast->children[0];

    if (thing->expected_return_type->kind != TYPE_struct) {
        print_err("Tried to delete a non-struct! Are you trying to ruin my program on purpose?");
        return;
    }

    if (thing->token.type == ATTR_ACCESS) {
        generate_instructions_for_attr_addr(thing, instructions, var_map_list);
    } else {
        generate_instructions_for_node(thing, instructions, var_map_list);
    }

    print_err("This is deprecated and does nothing! KYS!");
}

void generate_instructions_for_scope_ref_dec(HashMap *scope_map, Inst **instructions, bool global) {
    String *keys = scope_map->keys;

    for (int i = 0; i < array_length(keys); i++) {
        VarHeader *vh = HashMap_get(scope_map, keys[i]);
        if (vh->type != VH_VAR) continue;
        if (!is_reference_typekind(vh->var_type->kind)) continue;

        array_append(*instructions, create_inst(global ? I_READ_GLOBAL : I_READ,
            (Val){.as_int = get_vartype_size(vh->var_type), .type = TYPE_int},
            (Val){.as_int = vh->var_pos, .type = TYPE_int}));
        array_append(*instructions, create_inst(get_dec_ref_inst_by_typekind(vh->var_type->kind), Val_null, Val_null));

    }
}



void generate_instructions_for_return_stmt(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    ASTNode *func = gi_current_function;

    if (!func) return_err("Tried to return outside of a function!");

    bool has_value = array_length(ast->children) != 0;

    if (has_value) {
        generate_instructions_for_node(&ast->children[0], instructions, var_map_list);
    
        if (is_nontemporary_reference(&ast->children[0])) {
            array_append(*instructions, create_inst_I_DUP());
            array_append(*instructions, create_inst_I_INC_REFCOUNT());
        }
    }


    { 


        VarHeader **vardecls = get_all_vardecls_before_return(func, ast, var_map_list);

        int l = array_length(vardecls);

        for (int i = 0; i < l; i++) {
            VarHeader *decl = vardecls[i];

            if (!is_reference_typekind(decl->var_type->kind)) continue;

            array_append(
                *instructions, 
                create_inst(
                    I_READ, 
                    Val_int(get_vartype_size(decl->var_type)), 
                    Val_int(decl->var_pos)
                )
            );

            array_append(*instructions, create_inst(get_dec_ref_inst_by_typekind(decl->var_type->kind), Val_null, Val_null));
        }


        array_free(vardecls);
    }

    if (has_value) {
        Type *target_type = func->children[0].token.var_type;
        
        if (!types_are_equal(target_type, ast->children[0].expected_return_type)) {
            bool res = generate_cvt_inst_for_types(
                ast->children[0].expected_return_type, 
                target_type, 
                instructions
            );
            if (!res) return_err("Type of returned value doesn't match the function signature!");
        }
    }


    array_append(*instructions, create_inst_I_RETURN());
}

// ARRAY STRUCTURE
// RCHeader(4 bytes), length(4 bytes), data(typesize * length bytes)


void generate_instructions_for_array_literal(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    Type *target_subtype = ast->expected_return_type->array_data.type;

    ASTNode *values_node = &ast->children[1];
    int len = array_length(values_node->children);
    int element_size = get_vartype_size(target_subtype);
    
    int size = calculate_array_offset(len, element_size);
    
    array_append(*instructions, create_inst_I_ALLOC(Val_int(size)));
    array_append(*instructions, create_inst_I_DUP());
    array_append(*instructions, create_inst(
        I_INIT_ARRAY_HEADER, Val_int(target_subtype->kind), 
        Val_int(len)));    
    
    for (int i = 0; i < len; i++) {
        
        ASTNode *child = &values_node->children[i];

        int offset = calculate_array_offset(i, element_size);

        array_append(*instructions, create_inst_I_DUP());
        array_append(*instructions, create_inst_I_GET_ATTR_ADDR(Val_int(offset)));
        
        generate_instructions_for_node(child, instructions, var_map_list);

        if (!types_are_equal(child->expected_return_type, target_subtype)) {
            bool res = generate_cvt_inst_for_types(child->expected_return_type, target_subtype, instructions);
            if (!res) return_err(
                "Invalid element type in array literal! Cannot convert from %s to %s!",
                type_get_name(child->expected_return_type).data,
                type_get_name(target_subtype).data
            );
        }

        array_append(*instructions, create_inst_I_HEAP_STORE(Val_int(get_vartype_size(child->expected_return_type))));
    }

}


void generate_instructions_for_array_subscript(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    ASTNode *left = &ast->children[0];

    ASTNode *attr_node = &ast->children[1];

    Type *subtype = left->expected_return_type->array_data.type;

    bool dec_ref = is_temporary_reference(left);

    generate_instructions_for_node(left, instructions, var_map_list);

    if (dec_ref) {
        array_append(*instructions, create_inst_I_DUP());
        array_append(*instructions, create_inst_I_TUCK());
    }

    generate_instructions_for_node(attr_node, instructions, var_map_list);

    array_append(*instructions, create_inst_I_ARRAY_SUBSCRIPT(Val_int(get_vartype_size(subtype))));


    if (dec_ref) {
        array_append(*instructions, create_inst_I_POP_BOTTOM());

        //                                        VVV its always an array for an array subscript duh
        array_append(*instructions, create_inst_I_DEC_REFCOUNT_ARRAY());
    }
}

void generate_instructions_for_str_subscript(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    ASTNode *left = &ast->children[0];
    ASTNode *idx_node = &ast->children[1];

    generate_instructions_for_node(left, instructions, var_map_list);
    generate_instructions_for_node(idx_node, instructions, var_map_list);
    array_append(*instructions, create_inst_I_STR_SUBSCRIPT());
}

void generate_instructions_for_subscript(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    ASTNode *left = &ast->children[0];

    match (left->expected_return_type->kind) {
        case (TYPE_array) generate_instructions_for_array_subscript(ast, instructions, var_map_list);
        case (TYPE_str) generate_instructions_for_str_subscript(ast, instructions, var_map_list);
        default() return_err("Cannot subscript value of type '%s'!", type_get_name(left->expected_return_type));
    }
}

void generate_instructions_for_array_subscript_addr(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    Type *subtype = ast->children[0].expected_return_type->array_data.type;

    generate_instructions_for_node(&ast->children[0], instructions, var_map_list);
    generate_instructions_for_node(&ast->children[1], instructions, var_map_list);

    array_append(*instructions, create_inst_I_ARRAY_SUBSCRIPT_ADDR(Val_int(get_vartype_size(subtype))));
}


void generate_instructions_for_subscript_addr(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    ASTNode *left = &ast->children[0];

    match (left->expected_return_type->kind) {
        case (TYPE_array) generate_instructions_for_array_subscript_addr(ast, instructions, var_map_list);
        case (TYPE_str) return_err("'str' is immmutable, you can't modify it!");
        default() return_err("Cannot subscript value of type '%s'!", type_get_name(left->expected_return_type));
    }
}

int get_dim_count_for_type(Type *t) {

    assert(t != NULL);

    if (t->kind != TYPE_array)
        return 0;

    return 1 + get_dim_count_for_type(t->array_data.type);
}

void generate_instructions_for_array_initializer(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    int dims = get_dim_count_for_type(ast->expected_return_type);

    ASTNode *dims_node = &ast->children[1];

    if (array_length(dims_node->children) != dims) return_err(
        "Bad array initializer! Expected to have %d dimensions, but you specified %d dimensions!",
        dims,
        array_length(dims_node->children)
    );

    for (int i = 0; i < dims; i++) {
        ASTNode *dim = &dims_node->children[i];

        generate_instructions_for_node(dim, instructions, var_map_list);
    }

    Type *final_subtype = ast->expected_return_type;
    while (final_subtype->kind == TYPE_array) final_subtype = final_subtype->array_data.type;

    array_append(*instructions, create_inst(I_INIT_N_DIM_ARRAY
        , Val_int(final_subtype->kind)
        , Val_int(dims)));

}

void generate_instructions_for_type_conversion(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    ASTNode *value = &ast->children[1];

    bool inc_ref = is_nontemporary_reference(value);

    generate_instructions_for_node(value, instructions, var_map_list);

    if (inc_ref) {
        array_append(*instructions, create_inst_I_DUP());
        array_append(*instructions, create_inst_I_INC_REFCOUNT());        
    }

    bool res = generate_cvt_inst_for_types(value->expected_return_type, ast->expected_return_type, instructions);

    if (!res) return_err(
        "Cannot explicitely convert from type '%s' to type '%s'!",
        type_get_name(value->expected_return_type).data,
        type_get_name(ast->expected_return_type).data
    );


}

void generate_instructions_for_node(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    bool handled = true;

    match (ast->token.type) {
        case (DECL_ASSIGN_STMT, DECL_STMT) 
            generate_instructions_for_vardecl(ast, instructions, var_map_list);
        
        case (ASSIGN_STMT) 
            generate_instructions_for_assign(ast, instructions, var_map_list);
        
        case (PRINT_STMT, WRITE_STMT) 
            generate_instructions_for_print(ast, instructions, var_map_list);
        
        case (IF_STMT, IF_ELSE_STMT) 
            generate_instructions_for_if(ast, instructions, var_map_list);
        
        case (WHILE_STMT) 
            generate_instructions_for_while(ast, instructions, var_map_list);
        
        case (INPUT_STMT) 
            generate_instructions_for_input(ast, instructions, var_map_list);
        
        case (FUNC_DECL_STMT) 
            generate_instructions_for_func_decl(ast, instructions, var_map_list);
        
        case (FUNC_CALL) 
            generate_instructions_for_func_call(ast, instructions, var_map_list);
        
        case (OP_UNARY_MINUS) 
            generate_instructions_for_unary_minus(ast, instructions, var_map_list);
        
        case (STRUCT_DECL_STMT) 
            generate_instructions_for_struct_decl(ast, var_map_list);
        
        case (ATTR_ACCESS) 
            generate_instructions_for_attr_access(ast, instructions, var_map_list);
        
        case (OP_NEW) 
            generate_instructions_for_new(ast, instructions, var_map_list);
        
        case (ARRAY_LITERAL)
            generate_instructions_for_array_literal(ast, instructions, var_map_list);
        
        case (ARRAY_INITIALIZER)
            generate_instructions_for_array_initializer(ast, instructions, var_map_list);
        
        case (ARRAY_SUBSCRIPT)
            generate_instructions_for_subscript(ast, instructions, var_map_list);

        case (DELETE_STMT) 
            generate_instructions_for_delete(ast, instructions, var_map_list);
        
        case (RETURN_STMT) 
            generate_instructions_for_return_stmt(ast, instructions, var_map_list);

        case (OP_CONVERT_TYPE)
            generate_instructions_for_type_conversion(ast, instructions, var_map_list);

        
        default ()
            if (in_range(ast->token.type, BINOPS_START, BINOPS_END)) {
                generate_instructions_for_binop(ast, instructions, var_map_list);
            } else {
                handled = false;
            }
    }

    if (handled) return;

    int temp_stack_ptr;

    // pre children operators
    match (ast->token.type) {
        case (STMT_SEQ)  ;
            int size = calc_stack_space_for_scope(ast);
            array_append(*instructions, create_inst_I_STACK_PTR_ADD(Val_int(size)));
        
        case (BLOCK) 
            temp_stack_ptr = gi_stack_pos;
            LL_prepend(var_map_list, LLNode_new(HashMap(VarHeader)));    
    }
        
    for (int i = 0; i < array_length(ast->children); i++) {
        generate_instructions_for_node(&ast->children[i], instructions, var_map_list);
    }


    // post children operators
    match (ast->token.type) {
        case (INTEGER) 
            array_append(*instructions, create_inst(I_PUSH, Val_int(4), 
                                                    Val_int(ast->token.as_int)));
        
        case (CHAR)
            array_append(*instructions, create_inst(I_PUSH, Val_int(1), 
                                                    Val_char(ast->token.as_char)));

        case (FLOAT) 
            array_append(*instructions, create_inst(I_PUSH, Val_int(8), 
                                                    Val_double(ast->token.as_double)));
        
        case (BOOL) 
            if (ast->token.as_int == MAYBE) {
                array_append(*instructions, create_inst_I_PUSH_MAYBE());
            } else {
                array_append(*instructions, create_inst(I_PUSH, Val_int(1), 
                                                        Val_bool(ast->token.as_int)));
            }
        
        case (NULL_REF) 
            array_append(*instructions, create_inst(I_PUSH, Val_int(8), 
                                                    (Val){.type = TYPE_struct, .as_ptr = NULL}));
        
        case (STRING_LITERAL) 
            array_append(*instructions, create_inst(I_PUSH, Val_int(8), 
                                                    Val_str(ast->token.text.data)));
        
        case (OP_ADD) 
            array_append(*instructions, create_inst_I_ADD());
        
        case (OP_SUB) 
            array_append(*instructions, create_inst_I_SUB());
        
        case (OP_MUL) 
            array_append(*instructions, create_inst_I_MUL());
        
        case (OP_DIV) 
            array_append(*instructions, create_inst_I_DIV());
        
        case (OP_MOD) 
            array_append(*instructions, create_inst_I_MOD());
        
        case (OP_GREATER) 
            array_append(*instructions, create_inst_I_GREATER());
        
        case (OP_GREATEREQ) 
            array_append(*instructions, create_inst_I_GREATER_EQUAL());
        
        case (OP_LESS) 
            array_append(*instructions, create_inst_I_LESS());
        
        case (OP_LESSEQ) 
            array_append(*instructions, create_inst_I_LESS_EQUAL());
        
        case (OP_EQ) 
            array_append(*instructions, create_inst_I_EQUAL());
        
        case (OP_NOTEQ) 
            array_append(*instructions, create_inst_I_NOT_EQUAL());
        
        case (OP_NOT) 
            array_append(*instructions, create_inst_I_NOT());
        
        case (NAME)  ;
            bool isglobal;
            VarHeader *vh = lookup_var(var_map_list, ast->token.text, &isglobal);

            if (!vh) return;

            array_append(*instructions, create_inst(isglobal ? I_READ_GLOBAL : I_READ,
                (Val){.as_int = get_vartype_size(vh->var_type), .type = TYPE_int},
                (Val){.as_int = vh->var_pos, .type = TYPE_int}));
        
        case (BLOCK) 

            generate_instructions_for_scope_ref_dec(var_map_list->head->val, instructions, false);
            HashMap_free(var_map_list->head->val);
            LL_pop_head(var_map_list);
            gi_stack_pos = temp_stack_ptr;
        
        case (STMT_SEQ)
            generate_instructions_for_scope_ref_dec(var_map_list->head->val, instructions, true);
        
        default ()
            print_err("Unhandled case in generate_instructions_for_node()!");
            print_token(ast->token, 0);
        
    }
}


Inst *generate_instructions(ASTNode *ast) {

    clear_struct_metadata();

    Inst *res = array(Inst, 20);
    LinkedList *var_map_list = LL_new();
    LL_append(var_map_list, LLNode_new(HashMap(VarHeader)));

    gi_stack_pos = 0;
    gi_label_idx = 0;
    generate_instructions_for_node(ast, &res, var_map_list);

    HashMap_free(var_map_list->head->val);

    LL_free(var_map_list);

    return res;
}

// #INST GEN END




// THIS IS HAPPENING

#define EPSILON 0.001
// 10, 3



void print_double(double a) {

    char str[512] = {0};

    sprintf(str, "%.10f", a);

    int dot_idx = 0;
    while (str[dot_idx] != '.') dot_idx++;

    int idx = dot_idx + 1;
    int final = idx;
    int len = strlen(str);

    int window = 0;

    while (str[idx] != 0 && window < lerp(6, 1, (double)(idx - dot_idx - 1) / (len - dot_idx)) ) {
        if (str[idx] == '0') window++;
        else {
            final = idx;
            window = 0;
        }
        idx++;
    }

    printf("%.*s", final + 1, str);

}




// Use if 'size' is guranteed to be 1, 4, or 8
static inline void my_memcpy(void *dst, const void *src, u8 size) {
    match (size) {
        case (0) {}
        case (1)
            *(u8 *)dst = *(u8 *)src;
        case (4)
            *(u32 *)dst = *(u32 *)src;
        case (8)
            *(u64 *)dst = *(u64 *)src;
        default()
            print_err("You should kill yourself NOW. unsupported size on my_memcpy(): %d", size);
            break;
    }
}


#define append(ptr, size) my_memcpy(temp_stack + temp_stack_ptr++, ptr, size);
#define pop(type) (*(type *)(temp_stack + --temp_stack_ptr))
#define dup() ({temp_stack[temp_stack_ptr] = temp_stack[temp_stack_ptr - 1]; temp_stack_ptr++;})
#define tuck(ptr, size) {memmove(temp_stack + 1, temp_stack, temp_stack_ptr++ * 8); my_memcpy(temp_stack, ptr, size);}
#define pop_bottom(type) ({type val = *(type *)temp_stack; memmove(temp_stack, temp_stack + 1, --temp_stack_ptr * 8); val;})

void *append_to_text_buffer(const char *text, int len) {

    
    memcpy(text_buffer + text_buffer_ptr, &len, sizeof(int));
    text_buffer_ptr += sizeof(int);

    void *retval = text_buffer + text_buffer_ptr;
    
    memcpy(text_buffer + text_buffer_ptr, text, len);
    text_buffer_ptr += len + 1;

    return retval;
}

void preprocess_string_literals(Inst *instructions) {
    
    int len = array_length(instructions);
    
    for (int i = 0; i < len; i++) {
        Inst *inst = &instructions[i];
        if (inst->type == I_PUSH && inst->arg2.type == TYPE_str && inst->arg2.as_str) {
            inst->arg2.as_str = append_to_text_buffer(inst->arg2.as_str, strlen(inst->arg2.as_str));
        }
    }
}

char *convert_insts_to_byte_arr(const Inst *instructions) {

    int len = array_length(instructions);

    int *new_indicies = malloc(sizeof(int) * len);
    int offset = 0;
    for (int i = 0; i < len; i++) {
        new_indicies[i] = i + offset;
        offset += instructions[i].arg1.type == TYPE_void ? 0 : get_typekind_size(instructions[i].arg1.type);
        offset += instructions[i].arg2.type == TYPE_void ? 0 : get_typekind_size(instructions[i].arg2.type);
    }

    // printf("new indicies: \n");
    // for (int i = 0; i < len; i++) {
    //     printf("#%d: %d \n", i, new_indicies[i]);
    // }



    char *byte_arr = array(char, 50);

    for (int i = 0; i < len; i++) {

        array_append(byte_arr, instructions[i].type);

        if (instructions[i].arg1.type != TYPE_void) {

            if (instructions[i].type == I_FREE) {
                print_err("what the fuck %d ", instructions[i].arg1.type);
            }

            int byte_count = get_typekind_size(instructions[i].arg1.type);

            char value[8] = {0};

            my_memcpy(value, &instructions[i].arg1.as_ptr, 8);

            if (instructions[i].type == I_JUMP 
                || instructions[i].type == I_JUMP_NOT 
                || instructions[i].type == I_JUMP_IF
                || instructions[i].type == I_STACK_STORE_FUNC_ADDR) {
                
                *(int *)value = new_indicies[instructions[i].arg1.as_int];
            }

            for (int j = 0; j < byte_count; j++) {
                array_append(byte_arr, value[j]);
            }
        }
        if (instructions[i].arg2.type != TYPE_void) {

            if (instructions[i].type == I_FREE) {
                print_err("what the fuck %d ", instructions[i].arg1.type);
            }

            int byte_count = get_typekind_size(instructions[i].arg2.type);

            for (int j = 0; j < byte_count; j++) {
                array_append(byte_arr, ((char *)&instructions[i].arg2.as_ptr)[j]);
            }
        }
    }

    free(new_indicies);

    return byte_arr;
}

void run_bytecode_instructions(Inst *instructions, double *time) {
    memset(temp_stack, 0, STACK_SIZE);
    temp_stack_ptr = 0;
    memset(text_buffer, 0, TEXT_BUF_SIZE);
    text_buffer_ptr = 0;
    memset(var_stack, 0, STACK_SIZE);
    stack_ptr = 0;
    frame_ptr = 0;
    runtime_frees = 0;
    runtime_mallocs = 0;
    
    preprocess_string_literals(instructions);

    char *byte_arr = convert_insts_to_byte_arr(instructions);

    // printf("bytecode: \n");
    // for (int i = 0; i < array_length(byte_arr); i++) {
    //     printf("#%d: [%u] (%s) \n", i, byte_arr[i], in_range(byte_arr[i], 0, INST_COUNT) ? inst_names[(int)byte_arr[i]] : "null");
    // }
    // printf("\n");

    // #EXECUTE
    
    srand(clock());
    double start = get_current_process_time_seconds();

    int len = array_length(byte_arr); // which is not equal to array_length(instructions)
    for (int inst_ptr = 0; inst_ptr < len; inst_ptr++) {

        match (byte_arr[inst_ptr]) {

            case (I_STACK_STORE_FUNC_ADDR) {

                int addr = *(int *)&byte_arr[++inst_ptr];
                inst_ptr += sizeof(int) - 1;
                int pos = *(int *)&byte_arr[++inst_ptr];
                inst_ptr += sizeof(int) - 1;

                my_memcpy(var_stack + frame_ptr + pos, &addr, sizeof(int));
            }
        
            case (I_PRINT_CHAR_ARRAY) {

                
                void *obj = pop(void *);
                int len = *(int *)(obj + sizeof(ObjectHeader));
                char *str = obj + sizeof(ObjectHeader) + sizeof(int);
                printf("%.*s", len, str);
            }

            case (I_STR_SUBSCRIPT) {
                int idx = pop(int);
                char *str = pop(char *);
                int len = *(int *)(str - sizeof(int));


                if (idx < 0 || idx >= len) {
                    print_err(
                        "Invalid str subscript! Tried to access idx %d, but length is %d!",
                        idx,
                        len
                    );
                    exit(EXIT_FAILURE);
                }

                char final = str[idx];

                append(&final, sizeof(char *));
            }

            case (I_SLEEP) {
                double seconds = pop(double);
                sleep(seconds);
            }
            case (I_CLEAR_TERMI_LINES) {
                int lines = pop(int);
                clear_n_lines(lines);
            }
            case (I_PUSH_RAND) {
                int rand_value = rand();
                append(&rand_value, sizeof(int));
            }
            case (I_INIT_N_DIM_ARRAY) {
                int elem_size = byte_arr[++inst_ptr];
                inst_ptr += sizeof(int) - 1;
                int n = byte_arr[++inst_ptr];
                inst_ptr += sizeof(int) - 1;
                int *dims = calloc(sizeof(int), n);
                for (int i = n - 1; i >= 0; i--) { /* bc we're popping */
                    dims[i] = pop(int);
                }
                void *arr = init_n_dim_array(elem_size, n, dims);
                free(dims);
                append(&arr, sizeof(void *));
            }
            case (I_ALLOC_DYNAMIC) {
                int size = pop(int);
                void *addr = alloc_object(size);
                append(&addr, sizeof(addr));
            }
            case (I_ARRAY_SUBSCRIPT_ADDR) {
                inst_ptr += 1;
                int elem_size = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int) - 1;
                int idx = pop(int);
                void *addr = pop(char *);
                if (!addr) {
                    print_err(
                        "Tried to subscript a null reference!"
                    );
                    exit(EXIT_FAILURE);
                }
                int *len = addr + sizeof(ObjectHeader);
                if (idx >= (*len) || idx < 0) {
                    print_err(
                        "Array index out of bounds! array length: %d, tried to access index %d",
                        *len,
                        idx
                    );
                    exit(EXIT_FAILURE);
                }
                void *final = addr + calculate_array_offset(idx, elem_size);
                append(&final, sizeof(void *));
            }
            case (I_INIT_ARRAY_HEADER) {
                void *obj = pop(void *);
                TypeKind typekind = byte_arr[++inst_ptr];
                inst_ptr += sizeof(int);
                int len = byte_arr[inst_ptr];
                inst_ptr += sizeof(int) - 1;
                object_init_header(obj, typekind);
                *(int *)(obj + sizeof(ObjectHeader)) = len;
            }
            case (I_DEC_REFCOUNT_ARRAY) {
                void *arr = pop(void *);
                object_dec_ref(arr, true);
            }
            case (I_ARRAY_SUBSCRIPT) {
                inst_ptr += 1;
                int elem_size = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int) - 1;
                int idx = pop(int);
                void *addr = pop(char *);
                if (!addr) {
                    print_err(
                        "Tried to subscript a null reference!"
                    );
                    exit(EXIT_FAILURE);
                }
                int *len = addr + sizeof(ObjectHeader);
                if (idx >= (*len) || idx < 0) {
                    print_err(
                        "Array index out of bounds! array length: %d, tried to access index %d",
                        *len,
                        idx
                    );
                    exit(EXIT_FAILURE);
                }
                int offset = calculate_array_offset(idx, elem_size);
                void *final = addr + offset;
                append(final, elem_size);
            }
            case (I_POP_BOTTOM) {
                u64 obj = pop_bottom(u64);
                append(&obj, 8);
            }
            case (I_TUCK) {
                u64 obj = pop(u64);
                tuck(&obj, 8);
            }
            case (I_INIT_OBJ_HEADER) {;
                void *obj = pop(void *);
                u8 struct_meta_idx = byte_arr[++inst_ptr];
                inst_ptr += sizeof(int) - 1;
                object_init_header(obj, struct_meta_idx);
            }
            case(I_INC_REFCOUNT) {;
                void *obj = pop(void *);
                object_inc_ref(obj);
            }
            case(I_DEC_REFCOUNT) {;
                void *obj = pop(void *);
                object_dec_ref(obj, false);
            }
            case (I_NOP);
            case (I_PUSH);
                int size = *(int *)&byte_arr[++inst_ptr];
                append(byte_arr + (inst_ptr += sizeof(int)), size);
                inst_ptr += size - 1;
            case (I_PUSH_MAYBE);
                bool m = rand() % 2;
                temp_stack[temp_stack_ptr++] = m;
            case (I_DUP) {
                dup();
            }
            case (I_READ) {
                inst_ptr += 1;
                int size = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int);
                int pos = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int) - 1;
                append(var_stack + frame_ptr + pos, size);
            }
            case (I_READ_GLOBAL) {
                inst_ptr += 1;
                int size = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int);
                int pos = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int) - 1;
                append(var_stack + pos, size);
            }
            case (I_READ_ATTR) {
                inst_ptr += 1;
                int size = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int);
                int offset = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int) - 1;
                char *addr = pop(char *);

                if (!addr) print_err("Tried to get attribute of 'null'! inst: #%d \n", inst_ptr);
                append(addr + offset, size);
            }
            case (I_GET_ATTR_ADDR) {
                inst_ptr += 1;
                int offset = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int) - 1;
                char *addr = pop(char *);
                if (!addr) print_err("Tried to get attribute of 'null'! inst: #%d \n", inst_ptr);
                addr += offset;
                append(&addr, sizeof(char *));
            }
            case (I_STACK_STORE) {
                inst_ptr += 1;
                int size = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int);
                int pos = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int) - 1;
                my_memcpy(var_stack + frame_ptr + pos, temp_stack + --temp_stack_ptr, size);
            }
            case (I_STACK_STORE_GLOBAL) {
                inst_ptr += 1;
                int size = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int);
                int pos = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int) - 1;
                my_memcpy(var_stack + pos, temp_stack + --temp_stack_ptr, size);
            }
            case (I_HEAP_STORE) {
                inst_ptr += 1;
                int size = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int) - 1;
                u64 value = pop(u64);
                char *addr = pop(char *);
                my_memcpy(addr, &value, size);
            }
            case (I_ALLOC) {
                inst_ptr += 1;
                int size = *(int *)&byte_arr[inst_ptr];
                inst_ptr += sizeof(int) - 1;
                void *addr = alloc_object(size);
                append(&addr, sizeof(addr));
            }
            case (I_FREE) {
                print_err("Bro tried to delete manually");
            }
            case (I_JUMP) {
                int pos = *(int *)&byte_arr[++inst_ptr];
                inst_ptr = pos - 1;
            }
            case (I_JUMP_IF) {
                bool b = pop(bool);
                int pos = *(int *)&byte_arr[++inst_ptr];
                if (b) inst_ptr = pos - 1;
                else inst_ptr += sizeof(int) - 1;
            }
            case (I_JUMP_NOT) {
                bool b = pop(bool);
                int pos = *(int *)&byte_arr[++inst_ptr];
                if (!b) inst_ptr = pos - 1;
                else inst_ptr += sizeof(int) - 1;
            }
            case (I_CALL) {
                int callpos = pop(int);
                int val = inst_ptr + 1;
                tuck(&frame_ptr, sizeof(int));
                tuck(&stack_ptr, sizeof(int));
                tuck(&val, sizeof(int));
                frame_ptr += stack_ptr;
                stack_ptr = 0;
                inst_ptr = callpos - 1;
            }
            case (I_RETURN) {
                int ret_addr = pop_bottom(int);
                inst_ptr = ret_addr - 1;
                stack_ptr = pop_bottom(int);
                frame_ptr = pop_bottom(int);
            }
            case (I_STACK_PTR_ADD) {
                inst_ptr += 1;
                int size = *(int *)(byte_arr + inst_ptr);
                inst_ptr += sizeof(int) - 1;
                stack_ptr += size;
            }
            case (I_PRINT_INT) {
                int num = pop(int);
                printf("%d", num);
            }
            case (I_PRINT_CHAR) {
                char c = pop(char);
                printf("%c", c);
            }
            case (I_PRINT_STR) {
                char *str = pop(char *);
                if (!str) {
                    print_err("Tried to print null string!");
                    exit(EXIT_FAILURE);
                }
                printf("%s", str);
            }
            case (I_PRINT_FLOAT) {
                double num = pop(double);
                print_double(num);
            }
            case (I_PRINT_BOOL) {
                bool b = pop(bool);
                printf("%s", b ? "true" : "false");
            }
            case (I_PRINT_NEWLINE) {
                printf("\n");
            }
            case (I_ADD) {
                int num = pop(int);
                int *top = (int *)(temp_stack + temp_stack_ptr - 1);
                (*top) += num;
            }
            case (I_SUB) {
                int num = pop(int);
                int *top = (int *)(temp_stack + temp_stack_ptr - 1);
                (*top) -= num;
            }
            case (I_MUL) {
                int num = pop(int);
                int *top = (int *)(temp_stack + temp_stack_ptr - 1);
                (*top) *= num;
            }
            case (I_DIV) {
                int num = pop(int);
                int *top = (int *)(temp_stack + temp_stack_ptr - 1);
                (*top) /= num;
            }
            case (I_MOD) {
                int num = pop(int);
                int *top = (int *)(temp_stack + temp_stack_ptr - 1);
                (*top) %= num;
            }
            case (I_ADD_FLOAT) {
                double num = pop(double);
                double *top = (double *)(temp_stack + temp_stack_ptr - 1);
                (*top) += num;
            }
            case (I_SUB_FLOAT) {
                double num = pop(double);
                double *top = (double *)(temp_stack + temp_stack_ptr - 1);
                (*top) -= num;
            }
            case (I_MUL_FLOAT) {
                double num = pop(double);
                double *top = (double *)(temp_stack + temp_stack_ptr - 1);
                (*top) *= num;
            }
            case (I_DIV_FLOAT) {
                double num = pop(double);
                double *top = (double *)(temp_stack + temp_stack_ptr - 1);
                (*top) /= num;
            }
            case (I_MOD_FLOAT) {
                double num = pop(double);
                double *top = (double *)(temp_stack + temp_stack_ptr - 1);
                (*top) = fmod(*top, num);
            }
            case (I_CONVERT_BOOL_FLOAT) {
                double num = pop(bool) ? 1.0 : 0.0;
                append(&num, 8);
            }
            case (I_CONVERT_BOOL_INT) {
                int num = pop(bool) ? 1 : 0;
                append(&num, 4);
            }
            case (I_CONVERT_BOOL_STR) {
                StringRef string = pop(bool) == 0 ? StringRef("false") : StringRef("true"); 
                char *str = append_to_text_buffer(string.data, string.len);
                append(&str, 8);
            }
            case (I_CONVERT_FLOAT_BOOL) {
                bool b = pop(double) > 0 ? true : false;
                append(&b, 1);
            }
            case (I_CONVERT_FLOAT_INT) {
                int num = pop(double);
                append(&num, 4);
            }
            case (I_CONVERT_FLOAT_STR) {
                String string = String_from_double(pop(double), 2);
                char *str = append_to_text_buffer(string.data, string.len);
                String_delete(&string);
                append(&str, 8);
            }
            case (I_CONVERT_INT_BOOL) {
                bool b = pop(int) ? true : false;
                append(&b, 1);
            }
            case (I_CONVERT_INT_FLOAT) {
                double num = pop(int);
                append(&num, 8);
            }
            case (I_CONVERT_INT_STR) {
                String string = String_from_int(pop(int));
                char *str = append_to_text_buffer(string.data, string.len);
                String_delete(&string);
                append(&str, 8);
            }
            case (I_CONVERT_INT_CHAR) {
                char c = pop(int);
                append(&c, sizeof(char));
            }
            case (I_CONVERT_CHAR_INT) {
                int c = pop(char);
                append(&c, sizeof(int));
            }
            case (I_CONVERT_STR_FLOAT) {
                char *str = pop(char *);
                double res = String_to_double(StringRef(str));
                append(&res, 8);
            }
            case (I_CONVERT_STR_BOOL) {
                char *str = pop(char *);
                bool b = String_equal(StringRef(str), StringRef("true"));
                append(&b, 1);
            }
            case (I_CONVERT_STR_INT) {
                char *str = pop(char *);
                int res = String_to_int(StringRef(str));
                append(&res, 4);
            }
            case (I_GREATER) {
                int num = pop(int);
                int *top = (int *)(temp_stack + temp_stack_ptr - 1);
                (*top) = (*top) > num;
            }
            case (I_GREATER_EQUAL) {
                int num = pop(int);
                int *top = (int *)(temp_stack + temp_stack_ptr - 1);
                (*top) = (*top) >= num;
            }
            case (I_LESS) {
                int num = pop(int);
                int *top = (int *)(temp_stack + temp_stack_ptr - 1);
                (*top) = (*top) < num;
            }
            case (I_LESS_EQUAL) {
                int num = pop(int);
                int *top = (int *)(temp_stack + temp_stack_ptr - 1);
                (*top) = (*top) <= num;
            }
            case (I_GREATER_FLOAT) {
                double num = pop(double);
                bool res = pop(double) > num;
                append(&res, 1);
            }
            case (I_GREATER_EQUAL_FLOAT) {
                double num = pop(double);
                bool res = pop(double) >= num;
                append(&res, 1);
            }
            case (I_LESS_FLOAT) {
                double num = pop(double);
                bool res = pop(double) < num;
                append(&res, 1);
            }
            case (I_LESS_EQUAL_FLOAT) {
                double num = pop(double);
                bool res = pop(double) <= num;
                append(&res, 1);
            }
            case (I_EQUAL) {
                int num = pop(int);
                bool res = pop(int) == num;
                append(&res, 1);
            }
            case (I_EQUAL_FLOAT) {
                double num = pop(double);
                bool res = pop(double) == num;
                append(&res, 1);
            }
            case (I_EQUAL_BOOL) {
                bool b = pop(bool);
                bool b2 = pop(bool);
                bool res = b2 == b;
                append(&res, 1);
            }
            case (I_EQUAL_STR) {
                char *str = pop(char *);
                char *str2 = pop(char *);
                bool res = !strcmp(str, str2);
                append(&res, 1);
            }
            case (I_EQUAL_REF) {
                char *p1 = pop(char *);
                char *p2 = pop(char *);
                bool res = p1 == p2;
                append(&res, 1);
            }
            case (I_NOT_EQUAL) {
                int num = pop(int);
                bool res = pop(int) != num;
                append(&res, 1);
            }
            case (I_NOT_EQUAL_FLOAT) {
                double num = pop(double);
                bool res = pop(double) != num;
                append(&res, 1);
            }
            case (I_NOT_EQUAL_BOOL) {
                bool b = pop(bool);
                bool res = pop(bool) != b;
                append(&res, 1);
            }
            case (I_NOT_EQUAL_STR) {
                char *str = pop(char *);
                char *str2 = pop(char *);
                bool res = !(!strcmp(str, str2));
                append(&res, 1);
            }
            case (I_NOT_EQUAL_REF) {
                char *p1 = pop(char *);
                char *p2 = pop(char *);
                bool res = p1 != p2;
                append(&res, 1);
            }
            case (I_AND) {
                bool b = pop(bool);
                b = pop(bool) && b;
                append(&b, 1);
            }
            case (I_OR) {
                bool b = pop(bool);
                b = pop(bool) || b;
                append(&b, 1);
            }
            case (I_NOT) {
                bool b = !pop(bool);
                append(&b, 1);
            }
            case (I_INPUT) {
                char *string_im_not_gonna_free = malloc(INPUT_BUFFER_SIZE);
                fgets(string_im_not_gonna_free, INPUT_BUFFER_SIZE, stdin);
                string_im_not_gonna_free[strlen(string_im_not_gonna_free) - 1] = 0;
                char *str = append_to_text_buffer(string_im_not_gonna_free, strlen(string_im_not_gonna_free));
                free(string_im_not_gonna_free);
                append(&str, 4);
            }
            case (I_LABEL);
            default () {
                print_err("Too stupid. cant.\n");
                printf("instruction: #%d: %s \n", inst_ptr, inst_names[(int)byte_arr[inst_ptr]]);
            }
        }
    }

    double end = get_current_process_time_seconds();

    
    if (time != NULL) *time = end - start;


    array_free(byte_arr);
}

void print_text_buffer() {
    printf("text buffer: [");
    for (int i = 0; i < TEXT_BUF_SIZE; i++) {
        printf("%c", text_buffer[i]);
        if (text_buffer[i] != 0) printf("|");
    }
    printf("]\n");
}

void run_program(Inst *instructions) {
    printf("Running struct array \n");
    printf("Program output: \n");

    double time;

    printf("\n----------------------------------\n");

    run_bytecode_instructions(instructions, &time);

    printf("\n----------------------------------\n");

    if (runtime_mallocs > runtime_frees) {
        print_err("Detected a memory leak! Where? you figure it out. Mallocs: %d, Frees: %d ", runtime_mallocs, runtime_frees);
    } else if (runtime_frees > runtime_mallocs) {
        print_err("Detected excess memory deletions! How did the program even survive this far?");
    } else {
        printf("No memory leaks. \n\tAllocations: %d \n\tDeletions: %d \n", runtime_mallocs, runtime_frees);
    }


    printf("Program finished successfully after %.2f seconds! \n", time);

    print_text_buffer();
}

void run_program_bytes(Inst *instructions) {
    printf("Running bytecode \n");
    printf("Program output: \n");

    double time;

    run_bytecode_instructions(instructions, &time);

    printf("Program finished successfully after %.2f seconds! \n", time);

    print_text_buffer();
}


void run_benchmark(Inst *instructions) {

    printf("Program output: \n");

    double avg = 0;
    for (int lap = 1; lap <= BENCHMARK_ITERS; lap++) {

        double time;

        run_bytecode_instructions(instructions, &time);

        avg += time;
        printf("Time of %dth lap: %.3f \n", lap, time);
    }

    printf("Finished benchmarking. Average time: %.3f \n", avg / BENCHMARK_ITERS);
    
    print_text_buffer();
}

// void compile_instructions(Inst *instructions) {
//     FILE *file = fopen("out.asm", "w");

//     if (file == NULL) {
//         print_err("Failed to open output file!");
//         return;
//     }

//     #define write(...) fprintf(file, __VA_ARGS__)

//     int len = array_length(instructions);

//     write("section .text \n");
//     write("\tglobal _start \n");
//     write("_start: \n");

//     for (int inst_ptr = 0; inst_ptr < len; inst_ptr++) {
//         Inst inst = instructions[inst_ptr];

//         printf("; %s\n", inst_names[inst.type]);
//         write("; %s\n", inst_names[inst.type]);
//         switch (inst.type) {
//             case (I_STACK_PTR_ADD)
//                 write("\tadd rsp, %d \n", inst.arg1.as_int);
//                 break;
//             case (I_PUSH)
//                 if (inst.arg1.type == TYPE_int) {
//                     write("\tpush %d \n", inst.arg1.as_int);
//                 }
//                 break;
//             case (I_ADD)
//                 write("\tpop rax \n\tpop rbx \n\tadd rax, rbx \n\tpush rax \n");
//                 break;
//             default:
//                 break;
//         }
//     }

//     fclose(file);

// }






void print_ast(ASTNode node, int level) {
    if (is_null_ast(node)) {
        printf("---NULL AST--- \n");
        return;
    }
    for (int i = 0; i < level; i++) {
        printf("|   ");
    }
    if (!node.complete) printf("!!");
    printf("<");
    print_type(node.expected_return_type);
    printf(">");
    print_token(node.token, 0);
    for (int i = 0; i < array_length(node.children); i++) {
        print_ast(node.children[i], level + 1);
    }
}


bool is_stop_char(char c) {

    for (size_t i = 0; i < sizeof(SYMBOLS); i++) {
        if (c == SYMBOLS[i]) {
            return true;
        }
    }

    return false;
}


Lexeme *lex(StringRef text) {

    Lexeme *Ls = array(Lexeme, 10);
    
    char buf[1024] = {0};
    int pos = 0;

    char current_literal_bound = 0;

    bool in_comment = false;

    int line = 1;


    for (int i = 0; i < text.len - 1; i++) {
        
        char c = text.data[i];

        if (c == '#' && !current_literal_bound) {
            if (pos > 0) {
                array_append(Ls, Lexeme_new(String_ncopy_from_literal(buf, pos), line));
                pos = 0;
            }
            in_comment = true;
        }
        else if (c == '\n') in_comment = false;

        if (in_comment) continue;

        if (c == '"' || c == '\'') {
            if (!current_literal_bound) {
                if (pos > 0) {
                    array_append(Ls, Lexeme_new(String_ncopy_from_literal(buf, pos), line));
                    pos = 0;
                }
                current_literal_bound = c;
            } else if (c == current_literal_bound) {
                buf[pos++] = c;
                array_append(Ls, Lexeme_new(String_ncopy_from_literal(buf, pos), line));
                pos = 0;
                current_literal_bound = 0;
                continue;
            }
        }

        if (current_literal_bound) {
            buf[pos++] = c;
            continue;
        }

        bool stop_char = is_stop_char(c);
        

        if (stop_char && pos > 0) {
            array_append(Ls, Lexeme_new(String_ncopy_from_literal(buf, pos), line));
            pos = 0;

        }
        
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r' ) {
            if (c == '\n') line++;
            continue;
        }

        buf[pos++] = c;

        #define check(c1, c2) (c == c1 && text.data[i + 1] == c2)

        bool part_of_double_symbol = (stop_char && i + 1 < text.len) && (
            check('=', '=')
            || check('>', '=')
            || check('<', '=')
            || check('!', '=')
            || check('&', '&')
            || check('|', '|')
            || check('+', '=')
            || check('-', '=')
            || check('*', '=')
            || check('/', '=')
            || check('%', '=')
            || check('-', '>')
        );

        #undef check

        if (stop_char) {

            if (part_of_double_symbol) {
                buf[pos++] = text.data[i + 1];
                i++;
            }

            array_append(Ls, Lexeme_new(String_ncopy_from_literal(buf, pos), line));
            pos = 0;
        }
    }

    if (pos > 0) {
        for (int i = 0; i < pos; i++) {
            if ((buf[pos] == '"' || buf[pos] == '\'') 
                && current_literal_bound == buf[pos]) {

                current_literal_bound = (!current_literal_bound) ? buf[pos] : 0;
            }
        }
        array_append(Ls, Lexeme_new(String_ncopy_from_literal(buf, pos), line));
        pos = 0;
    }

    if (current_literal_bound) {
        print_err("Missing closing quotes in string literal!");
    }

    return Ls;

} 

void print_token(Token token, int level) {
    for (int i = 0; i < level; i++) {
        printf("\t");
    }

    if (token.type < 0 || token.type >= TOKEN_TYPE_COUNT) {
        printf("[UNDEFINED TOKEN, %d] \n", token.type);
        return;
    }

    printf("[%s", token_type_names[token.type]);
    match (token.type) {
        case (INTEGER)
            printf(", %d", token.as_int);
        case (FLOAT)
            printf(", %.2f", token.as_double);
        case (BOOL)
            printf(", %s", token.as_int == 1 ? "true" : ((char)token.as_int == MAYBE ? "maybe" : "false"));
        case (STRING_LITERAL)
            printf(", \"%s\"", token.text.data);
        case (CHAR)
            printf(", '%c'", token.as_char);
        case (NAME)
            printf(", %s", token.text.data);
        case (KEYWORD)
            printf(", %s", token.text.data);
        case (TYPE)
            printf(", ");
            print_type(token.var_type);
        default();
    }

    if (token.line)
        printf("] line: %d \n", token.line);
    else
        printf("] \n");        
}

void print_tokens(Token *tokens) {
    for (int i = 0; i < array_length(tokens); i++) {
        printf("#%d: ", i);
        print_token(tokens[i], 0);
    }
}

void print_lexemes(Lexeme *Ls) {
    for (int i = 0; i < array_length(Ls); i++) {
        printf("> '%s' len: %d, line: %d \n", Ls[i].text.data, Ls[i].text.len, Ls[i].line);
    }
}

#define free_lexemes(Ls) do { \
    _free_lexemes(Ls); \
    Ls = NULL; \
} while (0)

void _free_lexemes(Lexeme *Ls) {
    for (int i = 0; i < array_length(Ls); i++) {
        String_delete(&Ls[i].text);
    }

    array_free(Ls);
}

#define RESULT_OK 0
#define RESULT_INVALID_COMMAND 1
#define RESULT_COULDNT_OPEN_FILE 2

// this code is not meant to be pretty..
int handle_text_interface(char *buf, int bufsize, bool *benchmark) {

    printf("File or raw code? ('file' for file, 'code' for code)\n");

    char answer[20] = {0};
    fgets(answer, sizeof(answer), stdin);

    if (!strncmp(answer, "file", 4)) {

        *benchmark = !strncmp(answer + 5, "bench", 5);

        printf("Enter relative path: ");

        char filepath[100] = {0};


        fgets((char *)filepath, sizeof(filepath), stdin);

        filepath[strlen(filepath) - 1] = 0;

        printf("Reading from file: '%s' \n", filepath);

        FILE *file = fopen(filepath, "r");

        if (!file) 
            return RESULT_COULDNT_OPEN_FILE;
        

        char *buf_ptr = buf;

        while (fgets(buf_ptr, bufsize - (buf_ptr - buf), file)) {
            int len = strlen(buf_ptr);
            if (len > 0 && buf_ptr[len - 1] == '\n') {
                buf_ptr += len;
            }
        }

        buf[strlen(buf)] = 10; // dont ask, it works


        fclose(file);

        return RESULT_OK;

    } else if (!strncmp(answer, "code", 4)) {
        printf("Write the program here:\n>");

        fgets(buf, bufsize, stdin);
        return RESULT_OK;
    }

    return RESULT_INVALID_COMMAND;

}

#define KB 1024
#define MB 1024 * KB
#define GB 1024 * MB

#define CODE_MAX_LEN 4 * KB

// #MAIN

int main() {

    init_intrinsic_func_types();

    while (true) {



        char buf[CODE_MAX_LEN] = {0};
        bool benchmark = false;

        int result = handle_text_interface(buf, CODE_MAX_LEN, &benchmark);

        match (result) {
            case (RESULT_COULDNT_OPEN_FILE) 
                print_err("Couldn't open file! errno: %d", errno);
            
            case (RESULT_INVALID_COMMAND) 
                print_err("Invalid command! use 'file' to open a file and 'code' to enter raw code.");
        }
        if (result != RESULT_OK) continue;

        Lexeme *Ls = lex(StringRef(buf));

        if (LEXER_PRINT) print_lexemes(Ls);

        if (COMPILATION_STAGE < STAGE_TOKENIZER) {
            free_lexemes(Ls);
            continue;
        }

        Token *tokens = tokenize(Ls);

        if (TOKENIZER_PRINT) print_tokens(tokens);

        set_parse_tokens(tokens);

        if (COMPILATION_STAGE < STAGE_PARSER) {
            free_tokens(tokens);
            free_lexemes(Ls);
            continue;
        }

        ParseResult res = parse_program();        
        if (!res.success || res.endpos < array_length(tokens)) {
            printf("%s \n", parse_err);
            print_err("Invalid program! Failed to parse AST!");
            free_tokens(tokens);
            free_lexemes(Ls);
            continue;
        }
        
        if (PREPROCESS_AST) preprocess_ast(&res.node);
        
        if (PARSER_PRINT) {
            printf(">>> RESULT AST <<<\n");
            print_ast(res.node, 0);
        }
        
        if (COMPILATION_STAGE < STAGE_IR_GEN) {
            free_ast(res.node);
            free_tokens(tokens);
            free_lexemes(Ls);
            continue;
        }

        Inst *instructions = generate_instructions(&res.node);

        if (IR_PRINT) print_instructions(instructions);

        if (COMPILATION_STAGE < STAGE_RUN_CODE) {
            array_free(instructions);
            free_ast(res.node);
            free_tokens(tokens);
            free_lexemes(Ls);
            continue;
        }

        // place for chaos. increment when this made you want to kys: 3
        if (benchmark) {
            run_benchmark(instructions);
        } else {
            run_program(instructions);
        }

        array_free(instructions);

        free_tokens(tokens);

        free_lexemes(Ls);
    }

    free_intrinsic_func_types();

    return 0;
}

// #END