
// #FLAGS
#define DEBUG

#define STAGE_LEXER 0
#define STAGE_TOKENIZER 1
#define STAGE_PARSER 2
#define STAGE_IR_GEN 3
#define STAGE_RUN_CODE 4

#define COMPILATION_STAGE STAGE_PARSER

#define PREPROCESS_AST 0

#define LEXER_PRINT 0
#define TOKENIZER_PRINT 1
#define PARSER_PRINT 1
#define IR_PRINT 0
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

VarHeader create_funcs_header(String name, VarHeader *funcs) {
    return (VarHeader) {
        .name = name,
        .funcs = funcs
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

    int len = sizeof(type_kind_names) / sizeof (char *);

    for (int i = 0; i < len; i++) {
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

Token *tokenize_parts(String *parts) {
    Token *tokens = array(Token, 10);

    int len = array_length(parts);

    for (int i = 0; i < len; i++) {
        if (String_equal(parts[i], StringRef("true"))) {
            Token tk = {.type = BOOL, .int_val = true};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("false"))) {
            Token tk = {.type = BOOL, .int_val = false};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("maybe")) || String_equal(parts[i], StringRef("mabye"))) {
            Token tk = {.type = BOOL, .int_val = MAYBE};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("null"))) {
            Type *t = &_const_types[TYPE_struct];
            Token tk = {.type = NULL_REF, .var_type = t};
            array_append(tokens, tk);
            continue;
        }

        if (String_equal(parts[i], StringRef("."))) { // check this early because it conflicts with float
            Token tk = {.type = ATTR_ACCESS};
            array_append(tokens, tk);
            continue;
        }
        if (is_keyword(parts[i])) {
            Token tk = {.type = KEYWORD, .text = parts[i]};
            array_append(tokens, tk);
            continue;
        }
        if (str_is_type(parts[i])) {
            Token tk = {.type = TYPE, .var_type = type_from_str(parts[i])};
            array_append(tokens, tk);

            continue;
        }

        if (is_name(parts[i])) {
            Token tk = {.type = NAME, .text = parts[i]};
            array_append(tokens, tk);
            continue;
        }

        if (is_int(parts[i])) {

            if (i + 1 < len && parts[i + 1].data[0] == '.') {
                if (i + 2 < len && is_int(parts[i + 2])) {
                    String full_str = String_concatf(String_concat(parts[i], StringRef(".")), String_copy(parts[i + 2]));
                    Token tk = {.type = FLOAT, .double_val = parse_double(full_str)};
                    array_append(tokens, tk);
                    String_delete(&full_str);
                    i += 2;
                } else {// in cases like '32.' we dont have to use parts[i + 2] because all we care about is parts[i] ('32' in the example)
                    Token tk = {.type = FLOAT, .double_val = parse_double(parts[i])};
                    array_append(tokens, tk);
                    i++;
                }
            } else {
                Token tk = {.type = INTEGER, .int_val = parse_int(parts[i])};
                array_append(tokens, tk);
            }
            continue;


        }

        if (String_equal(parts[i], StringRef("="))) {
            Token tk = {.type = OP_ASSIGN};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("+="))) {
            Token tk = {.type = OP_ASSIGN_ADD};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("-="))) {
            Token tk = {.type = OP_ASSIGN_SUB};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("*="))) {
            Token tk = {.type = OP_ASSIGN_MUL};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("/="))) {
            Token tk = {.type = OP_ASSIGN_DIV};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("%="))) {
            Token tk = {.type = OP_ASSIGN_MOD};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("=="))) {
            Token tk = {.type = OP_EQ};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("!"))) {
            Token tk = {.type = OP_NOT};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("!="))) {
            Token tk = {.type = OP_NOTEQ};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef(">="))) {
            Token tk = {.type = OP_GREATEREQ};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef(">"))) {
            Token tk = {.type = OP_GREATER};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("<="))) {
            Token tk = {.type = OP_LESSEQ};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("<"))) {
            Token tk = {.type = OP_LESS};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("&&"))) {
            Token tk = {.type = OP_AND};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("||"))) {
            Token tk = {.type = OP_OR};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("+"))) {
            Token tk = {.type = OP_ADD};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("-"))) {
            Token tk = {.type = OP_SUB};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("/"))) {
            Token tk = {.type = OP_DIV};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("*"))) {
            Token tk = {.type = OP_MUL};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("("))) {
            Token tk = {.type = LPAREN};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef(")"))) {
            Token tk = {.type = RPAREN};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("{"))) {
            Token tk = {.type = LCURLY};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("}"))) {
            Token tk = {.type = RCURLY};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef(","))) {
            Token tk = {.type = COMMA};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef(";"))) {
            Token tk = {.type = STMT_END};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("%"))) {
            Token tk = {.type = OP_MOD};
            array_append(tokens, tk);
            continue;
        }
        if (parts[i].data[0] == '"') { // only need to check the first because at this point it's guranteed to be a valid literal
            Token tk = {.type = STRING_LITERAL, .text = String_cslice(parts[i], 1, parts[i].len - 1)};
            array_append(tokens, tk);
            continue;
        }
        
        

        Token invalid_token = {.type = INVALID};
        array_append(tokens, invalid_token);
        print_err("Invalid token!");
        printf("Token: %s \n", parts[i].data);

    }

    return tokens;

}

void free_tokens(Token *tokens) {
    array_free(tokens);
}

ASTNode create_ast_node(Token tk, bool complete) {
    ASTNode node = {0};
    node.children = array(ASTNode, 2);
    node.token = tk;
    node.complete = complete;
    node.expected_return_type = make_type(TYPE_void);
    
    return node;
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

// 1 + 2 + 3
// +: [3]
// +: [+: [2], 3]
// +: [+: [1, 2], 3]

// 1 + (2 + 3)
// in brackets:
// +: [3]
// +: [2, 3]
// +: [+: [2, 3]]
// +: [1, +: [2, 3]]

#define check_keyword(token, keyword_literal) (token.type == KEYWORD && String_equal(token.text, StringRef(keyword_literal)))

bool is_valid_stmt_boundary(Token tok) {
    match (tok.type) {
        case (RCURLY, STMT_END) then (
            return true
        );
        case (KEYWORD) then (
            return check_keyword(tok, "if")
                || check_keyword(tok, "for")
                || check_keyword(tok, "while")
                || check_keyword(tok, "print")
                || check_keyword(tok, "input")
                || check_keyword(tok, "delete")
                || check_keyword(tok, "struct");
        )
        default (
            return false;
        )
    }
}

#define PARSE_ERR_SIZE 1024

char parse_err[PARSE_ERR_SIZE] = {0};
int parse_err_most_tokens = 0;

#define START_PARSE ASTNode *__ast_free_list = array(ASTNode, 2); bool print_fails = false; int _start_idx = idx;
#define START_PARSE_DEBUG ASTNode *__ast_free_list = array(ASTNode, 2); bool print_fails = true; int _start_idx = idx;

#define FINISH_PARSE(ast) \
    do { \
        array_free(__ast_free_list); \
        return create_parse_result(true, ast, idx); \
    } while (0)

#define set_parse_error_if_deepest(...) \
    if (idx >= parse_err_most_tokens) { \
        parse_err_most_tokens = idx; \
        memset(parse_err, 0, PARSE_ERR_SIZE); \
        sprintf(parse_err, __VA_ARGS__); \
    }

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

#define MATCH_PARSE(varname, expr, ...) \
    ParseResult varname = expr; \
    if (!varname.success) { \
        _destroy_free_list(); \
        set_parse_error_if_deepest("Parse failed! Expected "#expr"!"); \
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
                "Parse failed! Expected '%s' after '%s', got '%s' instead!", \
                token_type_to_pretty_str(t__), \
                token_type_to_pretty_str(get_token(idx - 1).type), \
                token_type_to_pretty_str(get_token(idx).type) \
            ); \
            return_correct_parse_res(__VA_ARGS__); \
        } \
        idx++; \
    }

#define MATCH_TOKEN_WITH_KEYWORD(kw, ...) \
    if (!check_keyword(get_token(idx), kw)) { \
        _destroy_free_list(); \
        set_parse_error_if_deepest("Parse failed! Expected '%s', got '%s'!", kw, token_type_to_pretty_str(get_token(idx).type)); \
        return_correct_parse_res(__VA_ARGS__); \
    } \
    idx++;

#define MATCH_TOKEN(truthy_cond, ...) \
    if (!(truthy_cond)) { \
        _destroy_free_list(); \
        set_parse_error_if_deepest("Parse failed! Expected condition '"#truthy_cond"', got '%s'!", token_type_to_pretty_str(get_token(idx).type)); \
        return_correct_parse_res(__VA_ARGS__); \
    } \
    idx++;

#define MATCH_RETURN_IF_PARSED(expr) \
    { \
        ParseResult __res = expr; \
        if (__res.success) return __res; \
    }


ASTNode wrap_with_block(ASTNode node) {
    ASTNode block = create_ast_node((Token){.type = BLOCK}, true);
    array_append(block.children, node);

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
        );
        FINISH_PARSE(create_ast_node(get_token(token_idx), true));
    }
}

#define is_null_ast(ast) (!ast.children)

ParseResult parse_base_rule(int idx) {
    START_PARSE {
        MATCH_RETURN_IF_PARSED(parse_value(idx));
        MATCH_RETURN_IF_PARSED(parse_new_rule(idx));
        MATCH_TOKEN_WITH_TYPE(LPAREN);
        MATCH_PARSE(expr_res, parse_expr(idx));
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
        MATCH_PARSE(attr_h_res, parse_attr_rule_h(idx));

        ASTNode node = create_ast_node(get_token(op_idx), false);
    
        array_append(node.children, create_ast_node(get_token(name_idx), true));

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
        MATCH_PARSE(base_rule_res, parse_base_rule(idx));
        MATCH_PARSE(attr_h_res, parse_attr_rule_h(idx));

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

        MATCH_PARSE(unary_res, parse_unary_rule(idx));
        MATCH_PARSE(mul_rule_h_res, parse_mul_rule_h(idx));

        ASTNode node = create_ast_node(get_token(op_idx), false);
        
        array_append(node.children, unary_res.node);

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
        MATCH_PARSE(unary_res, parse_unary_rule(idx));

        MATCH_PARSE(mul_rule_h_res, parse_mul_rule_h(idx));

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

        MATCH_PARSE(mul_rule_res, parse_mul_rule(idx));
        MATCH_PARSE(add_rule_h_res, parse_add_rule_h(idx));

        ASTNode node = create_ast_node(get_token(op_idx), false);
        
        array_append(node.children, mul_rule_res.node);

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
        MATCH_PARSE(mul_rule_res, parse_mul_rule(idx));

        MATCH_PARSE(add_rule_h_res, parse_add_rule_h(idx));

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

        MATCH_PARSE(add_rule_res, parse_add_rule(idx));

        MATCH_PARSE(rel_rule_h_res, parse_rel_rule_h(idx));

        ASTNode node = create_ast_node(get_token(op_idx), false);
        
        array_append(node.children, add_rule_res.node);        

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
        MATCH_PARSE(add_rule_res, parse_add_rule(idx));

        MATCH_PARSE(rel_rule_h_res, parse_rel_rule_h(idx));

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

        MATCH_PARSE(rel_rule_res, parse_rel_rule(idx));

        MATCH_PARSE(and_rule_h_res, parse_and_rule_h(idx));

        ASTNode node = create_ast_node(get_token(op_idx), false);
        
        array_append(node.children, rel_rule_res.node);

        

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
        MATCH_PARSE(rel_rule_res, parse_rel_rule(idx));

        MATCH_PARSE(and_rule_h_res, parse_and_rule_h(idx));

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

        MATCH_PARSE(and_rule_res, parse_and_rule(idx));

        MATCH_PARSE(expr_h_res, parse_expr_h(idx));

        ASTNode node = create_ast_node(get_token(op_idx), false);
        
        array_append(node.children, and_rule_res.node);

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
        MATCH_PARSE(and_rule_res, parse_and_rule(idx));

        MATCH_PARSE(expr_h_res, parse_expr_h(idx));

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
        
        MATCH_PARSE(expr_res, parse_expr(idx));
        
        MATCH_TOKEN_WITH_TYPE(RPAREN);
        
        MATCH_PARSE(stmt_res, parse_stmt(idx));
        
        if (check_keyword(get_token(idx), "else")) {
            
            MATCH_TOKEN_WITH_KEYWORD("else");

            MATCH_PARSE(stmt2_res, parse_stmt(idx));

            ASTNode node = create_ast_node((Token){.type = IF_ELSE_STMT}, true);

            array_append(node.children, expr_res.node);
            if (stmt_res.node.token.type != BLOCK) {
                stmt_res.node = wrap_with_block(stmt_res.node);
            }
            
            if (stmt2_res.node.token.type != BLOCK) {
                stmt2_res.node = wrap_with_block(stmt2_res.node);
            }
            

            array_append(node.children, stmt_res.node);
            array_append(node.children, stmt2_res.node);

            FINISH_PARSE(node);

        } else {
            ASTNode node = create_ast_node((Token){.type = IF_STMT}, true);
            array_append(node.children, expr_res.node);

            if (stmt_res.node.token.type != BLOCK) {
                stmt_res.node = wrap_with_block(stmt_res.node);
            }
            
            array_append(node.children, stmt_res.node);

            FINISH_PARSE(node);
        }
    }
}

ParseResult parse_expr_stmt(int idx) {
    START_PARSE {
        MATCH_PARSE(expr_res, parse_expr(idx));
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
        MATCH_PARSE(stmt_res, parse_stmt(idx));
        
        ASTNode node = create_ast_node((Token){.type = STMT_SEQ}, true);
        
        array_append(node.children, stmt_res.node);
        
        while (true) {
            TRY_MATCH_PARSE(res, parse_stmt(idx));
            
            if (!res.success) break;

            array_append(node.children, res.node);
        }
        
        FINISH_PARSE(node);
        
    }
}


ParseResult parse_block(int idx) {
    START_PARSE {
        
        MATCH_TOKEN_WITH_TYPE(LCURLY);
        
        MATCH_PARSE(stmt_seq_res, parse_stmt_seq(idx));
        
        MATCH_TOKEN_WITH_TYPE(RCURLY);
        
        stmt_seq_res.node.token.type = BLOCK;

        FINISH_PARSE(stmt_seq_res.node);
    }
}

ParseResult parse_val_seq(int idx) {

    START_PARSE {
        MATCH_PARSE(expr_res, parse_expr(idx));

        ASTNode node = create_ast_node((Token){.type = VAL_SEQ}, true);

        array_append(node.children, expr_res.node);

        while (true) {
            if (get_token(idx).type != COMMA) break;
            MATCH_TOKEN_WITH_TYPE(COMMA);

            TRY_MATCH_PARSE(res, parse_expr(idx));
            if (!res.success) break;

            array_append(node.children, res.node);
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
    
        MATCH_PARSE(val_seq, parse_val_seq(idx));

        MATCH_TOKEN_WITH_TYPE(STMT_END);
        
        val_seq.node.token.type = newline ? PRINT_STMT : WRITE_STMT;

        FINISH_PARSE(val_seq.node);
    }
}

ParseResult parse_while_stmt(int idx) {

    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("while");
        MATCH_TOKEN_WITH_TYPE(LPAREN);
        MATCH_PARSE(expr_res, parse_expr(idx));
        MATCH_TOKEN_WITH_TYPE(RPAREN);
        MATCH_PARSE(stmt_res, parse_stmt(idx));

        ASTNode node = create_ast_node((Token){.type =  WHILE_STMT}, true);

        array_append(node.children, expr_res.node);

        if (stmt_res.node.token.type != BLOCK) {
            stmt_res.node = wrap_with_block(stmt_res.node);
        }

        array_append(node.children, stmt_res.node);

        FINISH_PARSE(node);
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

        Type *t = is_struct ? make_struct_type(get_token(type_idx).text) : make_type(get_token(type_idx).var_type->kind);

        ASTNode node = create_ast_node(
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
        MATCH_PARSE(type_res, parse_type(idx, NULL));

        int name_idx = idx;
        MATCH_TOKEN_WITH_TYPE(NAME);

        MATCH_TOKEN_WITH_TYPE(STMT_END);

        ASTNode node = create_ast_node((Token){.type = DECL_STMT}, true);

        array_append(node.children, type_res.node);

        array_append(node.children, create_ast_node(get_token(name_idx), true));

        FINISH_PARSE(node);
    }
}

ParseResult parse_assign_stmt(int idx) {
    
    START_PARSE {
        MATCH_PARSE(primary_res, parse_primary(idx));

        MATCH_TOKEN_WITH_TYPE(OP_ASSIGN);

        MATCH_PARSE(expr_res, parse_expr(idx));

        MATCH_TOKEN_WITH_TYPE(STMT_END);

        ASTNode node = create_ast_node((Token){.type = ASSIGN_STMT}, true);
    
        array_append(node.children, primary_res.node);
        
        array_append(node.children, expr_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_vardecl_assign_stmt(int idx) {

    START_PARSE {
        MATCH_PARSE(type_res, parse_type(idx, NULL));

        int name_idx = idx;
        MATCH_TOKEN_WITH_TYPE(NAME);

        MATCH_TOKEN_WITH_TYPE(OP_ASSIGN);

        MATCH_PARSE(expr_res, parse_expr(idx));

        MATCH_TOKEN_WITH_TYPE(STMT_END);

        ASTNode node = create_ast_node((Token){.type = DECL_ASSIGN_STMT}, true);
    
        array_append(node.children, type_res.node);
        
        array_append(node.children, create_ast_node(get_token(name_idx), true));
        
        array_append(node.children, expr_res.node);
        
        FINISH_PARSE(node);
    }
}

ParseResult parse_input_stmt(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("input");

        MATCH_PARSE(primary_res, parse_primary(idx));

        MATCH_TOKEN_WITH_TYPE(STMT_END);

        ASTNode node = create_ast_node((Token){.type = INPUT_STMT}, true);

        array_append(node.children, primary_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_func_arg(int idx) {
    START_PARSE {
        MATCH_PARSE(type_res, parse_type(idx, NULL));

        int name_idx = idx;
        MATCH_TOKEN_WITH_TYPE(NAME);

        ASTNode node = create_ast_node((Token){.type = FUNC_ARG}, true);
        array_append(node.children, type_res.node);
        array_append(node.children, create_ast_node(get_token(name_idx), true));

        FINISH_PARSE(node);
    }
}

ParseResult parse_func_args_seq(int idx) {
    START_PARSE {
        MATCH_PARSE(func_arg_res, parse_func_arg(idx), CAN_FAIL);

        ASTNode node = create_ast_node((Token){.type = FUNC_ARGS_SEQ}, true);

        array_append(node.children, func_arg_res.node);

        while (true) {

            if (get_token(idx).type != COMMA) break;

            MATCH_TOKEN_WITH_TYPE(COMMA);


            TRY_MATCH_PARSE(res, parse_func_arg(idx));

            if (!res.success) break;

            array_append(node.children, res.node);
        }

        FINISH_PARSE(node);

    }
}

ParseResult parse_func_decl_stmt(int idx) {
    START_PARSE {
        MATCH_PARSE(type_res, parse_type(idx, NULL));

        int name_idx = idx;
        MATCH_TOKEN_WITH_TYPE(NAME);

        MATCH_TOKEN_WITH_TYPE(LPAREN);

        MATCH_PARSE(func_args_res, parse_func_args_seq(idx));

        MATCH_TOKEN_WITH_TYPE(RPAREN);

        MATCH_PARSE(stmt_res, parse_stmt(idx));

        // turn one liners into blocks so i can gurantee every function has a block (to make dealing with argument lifetimes easier)
        if (stmt_res.node.token.type != BLOCK) {
            stmt_res.node = wrap_with_block(stmt_res.node);
        }

        ASTNode node = create_ast_node((Token){.type = FUNC_DECL_STMT}, true);

        array_append(node.children, type_res.node);

        array_append(node.children, create_ast_node(get_token(name_idx), true));

        if (!is_null_ast(func_args_res.node)) {
            array_append(node.children, func_args_res.node);
        } else {
            // empty placeholder node
            array_append(node.children, create_ast_node((Token){.type = FUNC_ARGS_SEQ}, true));
        }

        array_append(node.children, stmt_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_return_stmt(int idx) {

    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("return");

        TRY_MATCH_PARSE(expr_res, parse_expr(idx));
        if (expr_res.success) {
            MATCH_TOKEN_WITH_TYPE(STMT_END);

            ASTNode node = create_ast_node((Token){.type = RETURN_STMT}, true);

            array_append(node.children, expr_res.node);

            FINISH_PARSE(node);
        }

        MATCH_TOKEN_WITH_TYPE(STMT_END);

        FINISH_PARSE(create_ast_node((Token){.type = RETURN_STMT}, true));
    }
}

ParseResult parse_modify_stmt(int idx) {
    START_PARSE {
        MATCH_PARSE(primary_res, parse_primary(idx));

        int op_idx = idx;
        MATCH_TOKEN(in_range(get_token(idx).type, MODIFY_TOKENS_START, MODIFY_TOKENS_END));

        MATCH_PARSE(expr_res, parse_expr(idx));

        MATCH_TOKEN_WITH_TYPE(STMT_END);



        ASTNode node = create_ast_node((Token){.type = ASSIGN_STMT}, true);

        array_append(node.children, primary_res.node);
        TokenType op_type;

        match (get_token(op_idx).type) {
            case (OP_ASSIGN_ADD) then (
                op_type = OP_ADD;
            )
            case (OP_ASSIGN_SUB) then (
                op_type = OP_SUB;
            )
            case (OP_ASSIGN_MUL) then (
                op_type = OP_MUL;
            )
            case (OP_ASSIGN_DIV) then (
                op_type = OP_DIV;
            )
            case (OP_ASSIGN_MOD) then (
                op_type = OP_MOD;
            )
            default (
                print_err("LITERALLY CAN'T HAPPEN. KYS.");
            )
        }

        ASTNode op_node = create_ast_node((Token){.type = op_type}, true);
        array_append(op_node.children, primary_res.node);
        array_append(op_node.children, expr_res.node);
        array_append(node.children, op_node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_defer_stmt(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("defer");

        MATCH_PARSE(stmt_res, parse_stmt(idx));

        MATCH_TOKEN_WITH_TYPE(STMT_END);

        ASTNode node = create_ast_node((Token){.type = DEFER_STMT}, true);
        array_append(node.children, stmt_res.node);

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
        MATCH_PARSE(decl_res, _parse_vardecl_and_or_assign_decl(idx));

        ASTNode node = create_ast_node((Token){.type = DECL_SEQ}, true);

        array_append(node.children, decl_res.node);

        while (true) {
            TRY_MATCH_PARSE(res, _parse_vardecl_and_or_assign_decl(idx));
            if (!res.success) break;

            array_append(node.children, res.node);
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

        MATCH_PARSE(decl_seq_res, _parse_decl_seq(idx));

        MATCH_TOKEN_WITH_TYPE(RCURLY);

        ASTNode node = create_ast_node((Token){.type = STRUCT_DECL_STMT}, true);

        array_append(node.children, create_ast_node(get_token(name_idx), true));
        array_append(node.children, decl_seq_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_attr_postfix(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(ATTR_ACCESS);

        int name_idx = idx;
        MATCH_TOKEN_WITH_TYPE(NAME);
        
        ASTNode node = create_ast_node((Token){.type = ATTR_ACCESS}, false);
        array_append(node.children, create_ast_node(get_token(name_idx), true));
        FINISH_PARSE(node);
    }
}

ParseResult parse_func_call_postfix(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(LPAREN);

        TRY_MATCH_PARSE(val_seq_res, parse_val_seq(idx));

        if (val_seq_res.success) {
            MATCH_TOKEN_WITH_TYPE(RPAREN);

            ASTNode node = create_ast_node((Token){.type = FUNC_CALL}, false);

            array_append(node.children, val_seq_res.node);
            FINISH_PARSE(node);
        }

        MATCH_TOKEN_WITH_TYPE(RPAREN);

        ASTNode node = create_ast_node((Token){.type = FUNC_CALL}, false);

        array_append(node.children, create_ast_node((Token){.type = VAL_SEQ}, true));

        FINISH_PARSE(node);
    }
}

ParseResult parse_postfix(int idx) {

    START_PARSE {
        MATCH_RETURN_IF_PARSED(parse_attr_postfix(idx));
        MATCH_RETURN_IF_PARSED(parse_func_call_postfix(idx));


        FINISH_PARSE(null(ASTNode));
    }
}
// .b.c.d
ParseResult parse_postfix_seq(int idx) {
    START_PARSE {
        MATCH_PARSE(postfix_res, parse_postfix(idx));

        if (is_null_ast(postfix_res.node)) FINISH_PARSE(null(ASTNode));

        while (true) {
            MATCH_PARSE(res, parse_postfix(idx)); // we know this can't fail, so no need for TRY_MATCH_PARSE
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
        MATCH_PARSE(base_rule_res, parse_base_rule(idx));

        MATCH_PARSE(postfix_seq_res, parse_postfix_seq(idx));

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
        MATCH_PARSE(unary_res, parse_unary_rule(idx));
        ASTNode node = create_ast_node((Token){.type = OP_UNARY_MINUS}, true);
        array_append(node.children, unary_res.node);
        FINISH_PARSE(node);
    }
}

ParseResult parse_unary_not(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_TYPE(OP_NOT);
        MATCH_PARSE(unary_res, parse_unary_rule(idx));
        ASTNode node = create_ast_node((Token){.type = OP_NOT}, true);
        array_append(node.children, unary_res.node);
        FINISH_PARSE(node);
    }
}

ParseResult parse_unary_rule(int idx) {
    
    START_PARSE {

        MATCH_RETURN_IF_PARSED(parse_primary(idx));

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

        MATCH_PARSE(expr_res, parse_expr(idx));

        ASTNode node = create_ast_node((Token){.type = OP_ASSIGN}, true);
        array_append(node.children, create_ast_node(get_token(name_idx), true));
        array_append(node.children, expr_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_assign_seq(int idx) {
    START_PARSE {
        MATCH_PARSE(assign_res, _parse_assign(idx));

        ASTNode node = create_ast_node((Token){.type = ASSIGN_SEQ}, true);

        array_append(node.children, assign_res.node);

        while (true) {

            if (get_token(idx).type != COMMA) break;

            MATCH_TOKEN_WITH_TYPE(COMMA);


            TRY_MATCH_PARSE(res, _parse_assign(idx));
            if (!res.success) break;

            array_append(node.children, res.node);
        }

        FINISH_PARSE(node);
    }
}

ParseResult parse_new_rule(int idx) {

    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("new");

        bool is_struct;

        int name_idx = idx;
        MATCH_PARSE(type_res, parse_type(idx, &is_struct));

        if (!is_struct) {
            print_err("Can't use 'new' keyword on a primitive type! if you want it on the heap, wrap it in a struct");
            MATCH_TOKEN(false);
        }

        MATCH_TOKEN_WITH_TYPE(LPAREN);

        TRY_MATCH_PARSE(assign_seq_res, parse_assign_seq(idx));

        if (assign_seq_res.success) {
            MATCH_TOKEN_WITH_TYPE(RPAREN);

            ASTNode node = create_ast_node((Token){.type = OP_NEW}, true);
            array_append(node.children, create_ast_node(get_token(name_idx), true));
            array_append(node.children, assign_seq_res.node);

            FINISH_PARSE(node);
        }

        MATCH_TOKEN_WITH_TYPE(RPAREN);

        ASTNode node = create_ast_node((Token){.type = OP_NEW}, true);
        array_append(node.children, create_ast_node(get_token(name_idx), true));
        array_append(node.children, create_ast_node((Token){.type = ASSIGN_SEQ}, true));

        FINISH_PARSE(node);
    }
}

ParseResult parse_delete_stmt(int idx) {

    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("delete");
        MATCH_PARSE(primary_res, parse_primary(idx));
        MATCH_TOKEN_WITH_TYPE(STMT_END);

        ASTNode node = create_ast_node((Token){.type = DELETE_STMT}, true);
        array_append(node.children, primary_res.node);

        FINISH_PARSE(node);
    }
}

ParseResult parse_for_stmt(int idx) {
    START_PARSE {
        MATCH_TOKEN_WITH_KEYWORD("for");
        MATCH_TOKEN_WITH_TYPE(LPAREN);
        MATCH_PARSE(init_stmt, parse_stmt(idx));
        MATCH_PARSE(cond_expr, parse_expr(idx));
        MATCH_TOKEN_WITH_TYPE(STMT_END);
        MATCH_PARSE(update_stmt, parse_stmt(idx));
        MATCH_TOKEN_WITH_TYPE(RPAREN);
        MATCH_PARSE(code_stmt, parse_stmt(idx));
        ASTNode node = create_ast_node((Token){.type = BLOCK}, true);
        array_append(node.children, init_stmt.node);

        ASTNode while_node = create_ast_node((Token){.type = WHILE_STMT}, true);
        array_append(while_node.children, cond_expr.node);

        if (code_stmt.node.token.type != BLOCK) {
            code_stmt.node = wrap_with_block(code_stmt.node);
        }

        array_append(code_stmt.node.children, update_stmt.node);

        array_append(while_node.children, code_stmt.node);

        array_append(node.children, while_node);

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

// just an experiment
// void run_ast(ASTNode root) {
//     char stack[1024] = {0};


// }

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





// for now, since types are already sorted for least to most precedent, this function is meaningless. but it might change in the future.
int get_type_precedence(Type *type) {
    if (type == NULL) return -1;

    match (type->kind) {
        case (TYPE_bool) then (
            return 0;
        )
        case (TYPE_int) then (
            return 1;
        )
        case (TYPE_float) then (
            return 2;
        )
        case (TYPE_string) then (
            return 3;
        )
        case (TYPE_struct) then (
            return 4;
        )
        default (
            return -1;
        )
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

// TODO: A BLOCK CAN BE A BLOCKER!
int _validate_return_paths(ASTNode *ast, Type *return_type) {

    if (ast->token.type != BLOCK) {
        print_err("I don't know what to do here! look into this further. node type: '%s'", token_type_names[ast->token.type]);
        return false;
    }
    for (int i = 0; i < array_length(ast->children); i++) {
        ASTNode *child = &ast->children[i];
        if (child->token.type == RETURN_STMT && types_are_equal(child->children[0].expected_return_type, return_type)) {
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

int get_match_score_for_types(Type *t1, Type *t2) {
    if (t1->kind == TYPE_array || t1->kind == TYPE_struct || t2->kind == TYPE_array || t2->kind == TYPE_struct) {
        return types_are_equal(t1, t2) ? 4 : 0;
    }
    if (t1->kind == TYPE_int) {
        if (t2->kind == TYPE_float) return 3;
        if (t2->kind == TYPE_bool) return 2;
        if (t2->kind == TYPE_string) return 1;
    }
    if (t1->kind == TYPE_float) {
        if (t2->kind == TYPE_int) return 3;
        if (t2->kind == TYPE_bool) return 2;
        if (t2->kind == TYPE_string) return 1;
    }
    if (t1->kind == TYPE_bool) {
        if (t2->kind == TYPE_int) return 3;
        if (t2->kind == TYPE_float) return 2;
        if (t2->kind == TYPE_string) return 1;
    }
    if (t1->kind == TYPE_string) {
        if (t2->kind == TYPE_int) return 1;
        if (t2->kind == TYPE_float) return 1;
        if (t2->kind == TYPE_bool) return 1;
    }

    return 0;
}

int get_overload_match_score(VarHeader *func_args, ASTNode *call_args_ast) {
    
    assert(
        array_length(func_args) == array_length(call_args_ast->children)
    );

    int score = 0;

    for (int i = 0; i < array_length(func_args); i++) {
        score += get_match_score_for_types(call_args_ast->children[i].expected_return_type, func_args[i].var_type);
    }

    return score;
}

VarHeader *get_best_overload(VarHeader *overloads, ASTNode *call_args_ast) {

    VarHeader *best_match = NULL;
    bool ambiguous = false;
    int best_score = 0;

    for (int i = 0; i < array_length(overloads->funcs); i++) {
        VarHeader *func = &overloads->funcs[i];

        if (array_length(func->func_args) != array_length(call_args_ast->children)) continue;

        if (array_length(func->func_args) == 0) return func;

        int current_score = get_overload_match_score(func->func_args, call_args_ast);
        
        if (current_score == best_score) {
            ambiguous = true;
        }
        if (current_score > best_score) {
            best_score = current_score;
            best_match = func;
            ambiguous = false;
        }

    }

    if (!best_match) {
        print_err("Tried to call function '%s()' with invalid arguments!", overloads->name.data);

        printf("Argument types: ");
        for (int i = 0; i < array_length(call_args_ast->children); i++) {
            printf("%s, ", type_kind_names[call_args_ast->children[i].expected_return_type->kind]);
        }
    }

    if (ambiguous) {
        print_err("Tried to call function '%s()', but the call is ambiguous (function cannot be inferred by the argument types)!"
                        , overloads->name.data);
        exit(1); // unrecoverable
    }

    return best_match;
    
}

bool _func_vh_args_equal(VarHeader *args1, VarHeader *args2) {
    if (array_length(args1) != array_length(args2)) return false;

    for (int i = 0; i < array_length(args1); i++) {
        if (args1[i].var_type != args2[i].var_type) return false;
    }

    return true;
}

void add_func_vh_to_overloads(VarHeader *overloads_vh, VarHeader vh) {

    for (int i = 0; i < array_length(overloads_vh->funcs); i++) {
        if (_func_vh_args_equal(overloads_vh->funcs[i].func_args, vh.func_args))
            return_err("Tried to define '%s()' twice with the same signature! ", vh.name.data);
    }

    array_append(overloads_vh->funcs, vh);
}

// anything this function doesn't touch is meant to return void

void typeify_tree(ASTNode *node, HashMap *var_map) {


    match (node->token.type) {
        case(BLOCK) then({
            HashMap *copy = HashMap_copy(var_map);

            for (int i = 0; i < array_length(node->children); i++) {
                typeify_tree(&node->children[i], copy);
            }

            HashMap_free(copy);
        })
        case(DECL_STMT, DECL_ASSIGN_STMT) then({

            String var_name = node->children[1].token.text;
            Type *type = node->children[0].token.var_type;
            
            VarHeader vh = create_var_header(var_name, type, -1);
            HashMap_put(var_map, var_name, &vh);

            for (int i = 0; i < array_length(node->children); i++) {
                typeify_tree(&node->children[i], var_map);
            }
        })
        case(INTEGER) then({
            node->expected_return_type = make_type(TYPE_int);
        })
        case(FLOAT) then({
            node->expected_return_type = make_type(TYPE_float);
        })
        case(BOOL) then({
            node->expected_return_type = make_type(TYPE_bool);
        })
        case(STRING_LITERAL) then({
            node->expected_return_type = make_type(TYPE_string);
        })
        case(NULL_REF) then({
            node->expected_return_type = make_type(TYPE_struct);
        })
        case(NAME) then ({
            VarHeader *vh = HashMap_get_safe(var_map, node->token.text, NULL);
            if (!vh) 
                return_err("Identifier '%s' Doesn't exist within the current scope!", node->token.text.data);
            
            
            node->expected_return_type = copy_type(vh->var_type);
        })
        case(FUNC_DECL_STMT) then({
            ASTNode func_args = node->children[2];

            String func_name = node->children[1].token.text;

            VarHeader vh = create_func_header(
                func_name, 
                node->children[0].token.var_type, 
                -1, 
                get_args_from_func_decl_ast(&func_args)
            );

            VarHeader *overloads_ptr = HashMap_get_safe(var_map, func_name, NULL);
            if (overloads_ptr) {
                add_func_vh_to_overloads(overloads_ptr, vh);
            } else {
                VarHeader overloads = create_funcs_header(func_name, array(VarHeader, 2));
                array_append(overloads.funcs, vh);
                
                HashMap_put(var_map, func_name, &overloads);
            }

            var_map = HashMap_copy(var_map);
            
            int len = array_length(func_args.children);
            for (int i = 0; i < len; i++) {
                
                StringRef var_name = func_args.children[i].children[1].token.text;
                
                Type *type = func_args.children[i].children[0].token.var_type;
                
                VarHeader vh = create_var_header(var_name, type, -1);
                
                HashMap_put(var_map, var_name, &vh);
            }


            ASTNode *func_scope = &node->children[3];

            // this ensures we don't copy the hashmap twice because of the scope
            for (int i = 0; i < array_length(func_scope->children); i++) {
                typeify_tree(&func_scope->children[i], var_map);
            }

            HashMap_free(var_map);



            int result = validate_return_paths(node);
            
            match (result) {
                case(UNREACHABLE_CODE) then (
                    return_err("Return might cause unreachable code in function '%s()'!", func_name.data);
                )
                case(MISSING_RETURN_PATHS) then (
                    return_err("Not all return paths return type '%s' in function '%s()'!", 
                        type_kind_names[node->children[0].token.var_type->kind], func_name.data);    
                )
                case(RETURN_FROM_VOID_FUNCTION) then (
                    return_err("Tried to return a value from '%s()', which returns void!", func_name.data);
                )
            }

        })
        case (STRUCT_DECL_STMT) then({
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
    
            HashMap_put(var_map, name, &vh);    
        })
        case (ATTR_ACCESS) then({
            typeify_tree(&node->children[0], var_map);
            Type *left_type = node->children[0].expected_return_type;

            if (left_type->kind != TYPE_struct) {
                return_err("Tried accessing attribute in '%s' type instead of struct!", type_get_name(left_type).data);
            }

            String attr_name = node->children[1].token.text;

            String struct_name = type_get_name(left_type);

            VarHeader *struct_header = HashMap_get_safe(var_map, struct_name, NULL);

            if (!struct_header) {
                return_err("Tried accessing attribute of struct '%s' which is not defined!", struct_name.data);
            }

            VarHeader *attr_header = find_attr_in_struct(struct_header, attr_name);

            node->expected_return_type = attr_header->var_type;
        })

        case (OP_NEW) then({
            String struct_name = node->children[0].token.text;
            VarHeader *struct_vh = HashMap_get_safe(var_map, struct_name, NULL);
            if (!struct_vh) {
                return_err("can't create new '%s', as that struct is not defined!", struct_name.data);
            }

            node->expected_return_type = make_struct_type(struct_name);


            ASTNode *assign_seq = &node->children[1];
            for (int i = 0; i < array_length(assign_seq->children); i++) {
                ASTNode *member = &assign_seq->children[i].children[0];
                ASTNode *expr = &assign_seq->children[i].children[1];

                VarHeader *member_vh = find_attr_in_struct(struct_vh, member->token.text);

                member->expected_return_type = copy_type(member_vh->var_type);
                
                typeify_tree(expr, var_map);
            }
        })

        case (OP_UNARY_MINUS) then({
            typeify_tree(&node->children[0], var_map);
            node->expected_return_type = node->children[0].expected_return_type;    
        })

        case (FUNC_CALL) then({
            VarHeader *overloads = HashMap_get_safe(var_map, node->children[0].token.text, NULL);
            if (!overloads) return_err("Tried to call function '%s()' which doesn't exist!", node->children[0].token.text.data);
    
            (&node->children[0])->expected_return_type = make_type(TYPE_void);
    
            ASTNode *args_ast = &node->children[1];
    
            for (int i = 0; i < array_length(args_ast->children); i++) {
                typeify_tree(&args_ast->children[i], var_map);
            }
    
            VarHeader *vh = get_best_overload(overloads, args_ast);
    
            node->expected_return_type = copy_type(vh->func_return_type);
        })

        default({
            if (in_range(node->token.type, ARITHOPS_START, ARITHOPS_END)) {
                Type *highest_precedence_type = NULL;
                int len = array_length(node->children);
                for (int i = 0; i < len; i++) {
                    typeify_tree(&node->children[i], var_map);
                    if (get_type_precedence(node->children[i].expected_return_type) > get_type_precedence(highest_precedence_type)) {
                        highest_precedence_type = node->children[i].expected_return_type;
                    }
                }
                node->expected_return_type = highest_precedence_type; // #MAYBENULL
            } else if (in_range(node->token.type, BOOLOPS_START, BOOLOPS_END)) {
        
                node->expected_return_type = make_type(TYPE_bool);
        
                int len = array_length(node->children);
                for (int i = 0; i < len; i++) {
                    typeify_tree(&node->children[i], var_map);
                }
            } else {
                int len = array_length(node->children);
                for (int i = 0; i < len; i++) {
                    typeify_tree(&node->children[i], var_map);
                }
            } 
        })
    }
}

void typeify_tree_wrapper(ASTNode *node) {
    HashMap *var_map = HashMap(VarHeader);
    typeify_tree(node, var_map);
    
    HashMap_free(var_map);
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
        array_append(node->children, defers[i].children[0]); // we dont actually care about the defer, only about it's statement
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
        node->token.int_val = node->children[0].token.int_val op node->children[1].token.int_val; \
        node_clear_children(node); \
    } else { \
        double num1, num2; \
        if (node->children[0].token.type == INTEGER) num1 = (double)node->children[0].token.int_val; \
        else num1 = node->children[0].token.double_val; \
        if (node->children[1].token.type == INTEGER) num2 = (double)node->children[1].token.int_val; \
        else num2 = node->children[1].token.double_val; \
        node->token.type = FLOAT; \
        node->token.double_val = num1 op num2; \
        node_clear_children(node); \
    }

    #define BINOP_MOD_LOGIC() if (node->children[0].token.type == INTEGER && node->children[1].token.type == INTEGER) { \
        node->token.type = INTEGER; \
        node->token.int_val = node->children[0].token.int_val % node->children[1].token.int_val; \
        node_clear_children(node); \
    } else { \
        double num1, num2; \
        if (node->children[0].token.type == INTEGER) num1 = (double)node->children[0].token.int_val; \
        else num1 = node->children[0].token.double_val; \
        if (node->children[1].token.type == INTEGER) num2 = (double)node->children[1].token.int_val; \
        else num2 = node->children[1].token.double_val; \
        node->token.type = FLOAT; \
        node->token.double_val = fmod(num1, num2); \
        node_clear_children(node); \
    }

    switch (node->token.type) {
        case OP_ADD:
            BINOP_LOGIC(+)
            break;
        case OP_SUB:
            BINOP_LOGIC(-)
            break;
        case OP_DIV:
            BINOP_LOGIC(/)
            break;
        case OP_MUL:
            BINOP_LOGIC(*)
            break;
        case OP_MOD:
            BINOP_MOD_LOGIC()
            break;
        case OP_UNARY_MINUS:
            if (node->expected_return_type->kind == TYPE_int) {
                node->token.type = INTEGER;
                node->token.int_val = node->children[0].token.int_val;
                node->token.int_val *= -1;
                node_clear_children(node);
            } else {
                node->token.type = FLOAT;
                node->token.double_val = node->children[0].token.double_val;
                node->token.double_val *= -1;
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

void preprocess_ast(ASTNode *ast) {
    typeify_tree_wrapper(ast);
    move_defers_to_end(ast);
    apply_constant_folding(ast);
    // and anything else i might wanna do
}


// #INST
#define INSTRUCTIONS \
X(I_INVALID) \
X(I_PUSH) \
X(I_PUSH_MAYBE) \
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
X(I_PRINT_STR) \
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
X(I_FREE) \
X(I_DUP) \
X(I_INC_REFCOUNT) \
X(I_DEC_REFCOUNT) \
X(I_INIT_OBJ_HEADER) \
X(I_TUCK) \
X(I_POP_BOTTOM) \
X(INST_COUNT)

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

// - 1 + 1
// 1 + - 1
// 1 - 1


void print_val(Val val) {
    printf("(%s, ", type_kind_names[val.type]);
    switch (val.type) {
        case TYPE_int:
            printf("%d", val.i_val);
            break;
        case TYPE_float:
            printf("%.2f", val.f_val);
            break;
        case TYPE_bool:
            printf("%s", val.b_val ? "true" : "false");
            break;
        case TYPE_string:
            printf("\"%s\"", val.s_val);
            break;
        case TYPE_struct:
            if (!val.any_val) printf("null");
            else printf("%p", val.any_val);
            break;
        default:
            printf("dunno");
            break;
    }
    printf(")");
}

void print_instruction(Inst inst) {
    printf("[%s", inst_names[inst.type]);
    
    switch (inst.type) {
        case I_PUSH:
            printf(", ");
            print_val(inst.arg2);
            break;
        case I_CALL:
        case I_JUMP:
        case I_JUMP_IF:
        case I_JUMP_NOT:
            printf(", #%d", inst.arg1.i_val);
            break;
        case I_STACK_PTR_ADD:
            printf(", %d", inst.arg1.i_val);
            break;
        case I_STACK_STORE:
        case I_READ:
            printf(", sz: %d, pos: fp + %d", inst.arg1.i_val, inst.arg2.i_val);
            break;
        case I_STACK_STORE_GLOBAL:
        case I_READ_GLOBAL:
            printf(", sz: %d, pos: %d", inst.arg1.i_val, inst.arg2.i_val);
            break;
        case I_READ_ATTR:
            printf(", sz: %d, offset: %d", inst.arg1.i_val, inst.arg2.i_val);
            break;
        case I_GET_ATTR_ADDR:
            printf(", offset: %d", inst.arg1.i_val);
            break;
        case I_ALLOC:
        case I_HEAP_STORE:
            printf(", sz: %d", inst.arg1.i_val);
            break;
        default:
            break;
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
    // avert your eyes
    if (from->kind == TYPE_bool) {
        if (to->kind == TYPE_int) return I_CONVERT_BOOL_INT;
        if (to->kind == TYPE_float) return I_CONVERT_BOOL_FLOAT;
        if (to->kind == TYPE_string) return I_CONVERT_BOOL_STR;
    }
    if (from->kind == TYPE_int) {
        if (to->kind == TYPE_bool) return I_CONVERT_INT_BOOL;
        if (to->kind == TYPE_float) return I_CONVERT_INT_FLOAT;
        if (to->kind == TYPE_string) return I_CONVERT_INT_STR;
    }
    if (from->kind == TYPE_float) {
        if (to->kind == TYPE_bool) return I_CONVERT_FLOAT_BOOL;
        if (to->kind == TYPE_int) return I_CONVERT_FLOAT_INT;
        if (to->kind == TYPE_string) return I_CONVERT_FLOAT_STR;
    }
    if (from->kind == TYPE_string) {
        if (to->kind == TYPE_bool) return I_CONVERT_STR_BOOL;
        if (to->kind == TYPE_int) return I_CONVERT_STR_INT;
        if (to->kind == TYPE_float) return I_CONVERT_STR_FLOAT;
    }

    return I_INVALID;
}

bool generate_cvt_inst_for_types(Type *from, Type *to, Inst **instructions) {
    InstType inst = _get_cvt_inst_type_for_types(from, to);
    if (inst == I_INVALID) return false;

    array_append(*instructions, create_inst(inst, null(Val), null(Val)));
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
        if (kind == TYPE_string) return I_EQUAL_STR;
        if (kind == TYPE_struct) return I_EQUAL_REF;
    }
    if (op == OP_NOTEQ) {
        if (kind == TYPE_int) return I_NOT_EQUAL;
        if (kind == TYPE_float) return I_NOT_EQUAL_FLOAT;
        if (kind == TYPE_bool) return I_NOT_EQUAL_BOOL;
        if (kind == TYPE_string) return I_NOT_EQUAL_STR;
        if (kind == TYPE_struct) return I_NOT_EQUAL_REF;
    }
    if (op == OP_AND && kind == TYPE_bool) {
        return I_AND;
    }
    if (op == OP_OR && kind == TYPE_bool) {
        return I_OR;
    }

    return I_INVALID;
}

int gi_stack_pos = 0;
int gi_label_idx = 0;
ASTNode *gi_current_function = NULL;


VarHeader *get_varheader_from_map_list_safe(LinkedList *var_map_list, String name, bool *global) {
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

VarHeader *get_varheader_from_map_list(LinkedList *var_map_list, String name, bool *global) {
    VarHeader *result = get_varheader_from_map_list_safe(var_map_list, name, global);
    if (!result) 
        print_err("Identifier '%s' doesn't exist within the current scope!", name.data);
    
    return result;
}


void add_varheader_to_map_list(LinkedList *var_map_list, String key, VarHeader *vh) {
    HashMap *map = var_map_list->head->val;
    HashMap_put(map, key, vh);
}


//#INST GEN START

int calc_stack_space_for_scope(ASTNode *ast) {
    if (ast->token.type == DECL_ASSIGN_STMT || ast->token.type == DECL_STMT) return get_vartype_size(ast->children[0].token.var_type);
    if (ast->token.type == FUNC_DECL_STMT) return 0; // because thats a seperate scope

    int sum = 0;
    int len = array_length(ast->children);
    for (int i = 0; i < len; i++) {
        sum += calc_stack_space_for_scope(&ast->children[i]);
    }

    return sum;
}

void generate_instructions_for_node(ASTNode *ast, Inst **instructions, LinkedList *var_map_list);

static inline bool is_reference(ASTNode *ast) {
    return ast->expected_return_type->kind == TYPE_struct;
}

static inline bool is_temporary_reference(ASTNode *ast) {
    return ast->token.type == OP_NEW
        || (ast->token.type == FUNC_CALL && ast->expected_return_type->kind == TYPE_struct);
}

static inline bool is_nontemporary_reference(ASTNode *ast) {
    return is_reference(ast) && !is_temporary_reference(ast);
}

void generate_instructions_for_vardecl(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    String var_name = ast->children[1].token.text;

    Type *type = ast->children[0].token.var_type;
    
    VarHeader vh = create_var_header(var_name, type, gi_stack_pos);

    add_varheader_to_map_list(var_map_list, var_name, &vh);
    
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
        Val val = null(Val);
        val.type = type->kind;
        array_append(*instructions, create_inst(I_PUSH, val, null(Val)));
    }

    bool inc_refcounter = is_nontemporary_reference(&ast->children[2]);

    if (inc_refcounter) {
        array_append(*instructions, create_inst(I_DUP, null(Val), null(Val)));
    }
        
    array_append(*instructions, create_inst(I_STACK_STORE, (Val){.type = TYPE_int, .i_val = get_vartype_size(type)}, (Val){.type = TYPE_int, .i_val = gi_stack_pos}));
    gi_stack_pos += size;


    if (inc_refcounter) {
        array_append(*instructions, create_inst(I_INC_REFCOUNT, null(Val), null(Val)));
    }


}

void generate_instructions_for_attr_addr(ASTNode *ast, Inst **instructions, LinkedList *var_map_list);

// #UPDATE FOR STRUCTS
void generate_instructions_for_assign(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    ASTNode *left_side = &ast->children[0];
    ASTNode *right_side = &ast->children[1];


    // RC
    if (is_reference(left_side)) {
        generate_instructions_for_node(left_side, instructions, var_map_list);
        array_append(*instructions, create_inst(I_DEC_REFCOUNT, null(Val), null(Val)));
    }

    bool inc_refcount_for_right_side = is_nontemporary_reference(right_side);

    if (left_side->token.type == NAME) {

        
        String var_name = left_side->token.text;
        
        bool isglobal;
        
        VarHeader *vh = get_varheader_from_map_list(var_map_list, var_name, &isglobal);

        generate_instructions_for_node(right_side, instructions, var_map_list);

        if (inc_refcount_for_right_side) {
            array_append(*instructions, create_inst(I_DUP, null(Val), null(Val)));
            array_append(*instructions, create_inst(I_INC_REFCOUNT, null(Val), null(Val)));
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

        array_append(*instructions, create_inst((isglobal? I_STACK_STORE_GLOBAL : I_STACK_STORE), (Val){.type = TYPE_int, .i_val = get_vartype_size(vh->var_type)}, (Val){.type = TYPE_int, .i_val = vh->var_pos}));

    } else if (left_side->token.type == ATTR_ACCESS) {
        
        Type *goal_type = left_side->expected_return_type;

        generate_instructions_for_attr_addr(left_side, instructions, var_map_list);



        generate_instructions_for_node(right_side, instructions, var_map_list);

        if (inc_refcount_for_right_side) {
            array_append(*instructions, create_inst(I_DUP, null(Val), null(Val)));
            array_append(*instructions, create_inst(I_INC_REFCOUNT, null(Val), null(Val)));
        }


        if (!types_are_equal(goal_type, right_side->expected_return_type)) {
            bool result = generate_cvt_inst_for_types(right_side->expected_return_type, goal_type, instructions);
            if (!result) return_err(
                "Invalid conversion on assignment! Tried to convert from type '%s' to '%s'", 
                type_get_name(right_side->expected_return_type).data, 
                type_get_name(goal_type).data
            );
        }

        array_append(*instructions, create_inst(I_HEAP_STORE, (Val){.type = TYPE_int, .i_val = get_vartype_size(left_side->expected_return_type)}, null(Val)));
    }
    
    
}


void generate_instructions_for_binop(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    int len = array_length(ast->children);

    bool is_bool_op = in_range(ast->token.type, BOOLOPS_START, BOOLOPS_END);
    bool is_pure_bool_op = ast->token.type == OP_AND || ast->token.type == OP_OR;

    Type *highest_prec_type = NULL;

    if (is_bool_op) {
        for (int i = 0; i < len; i++) {
            if (get_type_precedence(ast->children[i].expected_return_type) > get_type_precedence(highest_prec_type)) {
                highest_prec_type = ast->children[i].expected_return_type;
            }
        }
    }

    Type *goal_type = is_pure_bool_op ? &_const_types[TYPE_bool] : (is_bool_op ? highest_prec_type : ast->expected_return_type);

    for (int i = 0; i < len; i++) {

        generate_instructions_for_node(&ast->children[i], instructions, var_map_list);

        if (goal_type != ast->children[i].expected_return_type) {
            bool result = generate_cvt_inst_for_types(ast->children[i].expected_return_type, goal_type, instructions);
            if (!result) return_err(
                "Invalid conversion in binary operation! Tried to convert between '%s' and '%s'!",
                type_get_name(ast->children[i].expected_return_type).data,
                type_get_name(goal_type).data
            );
        }
    }

    InstType inst_type = get_inst_type_for_op(ast->token.type, goal_type);
    if (inst_type == I_INVALID) {
        print_err("Invalid operator!");
        printf("Tried to get operator '%s' between '%s' type operands! \n", token_type_names[ast->token.type], type_get_name(goal_type).data);
    } else {
        array_append(*instructions, create_inst(inst_type, null(Val), null(Val)));
    }

}

InstType get_print_inst_for_type(Type *type) {
    match (type->kind) {
        case (TYPE_int) then (
            return I_PRINT_INT;
        )
        case (TYPE_float) then (
            return I_PRINT_FLOAT;
        )
        case (TYPE_bool) then (
            return I_PRINT_BOOL;
        )
        case (TYPE_string) then (
            return I_PRINT_STR;
        )
        default (
            return I_INVALID;
        )
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
            array_append(*instructions, create_inst(inst_type, null(Val), null(Val)));
        }
    }

    if (ast->token.type == PRINT_STMT) array_append(*instructions, create_inst(I_PRINT_NEWLINE, null(Val), null(Val)));
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
    array_append(*instructions, create_inst(I_JUMP_NOT, (Val){.type = TYPE_int, .i_val = -1}, null(Val)));

    // if-body
    generate_instructions_for_node(&ast->children[1], instructions, var_map_list);
    
    int if_body_jump_idx = -1;

    if (ast->token.type == IF_ELSE_STMT) {

        if_body_jump_idx = array_length(*instructions);
        array_append(*instructions, create_inst(I_JUMP, (Val){.type = TYPE_int, .i_val = end_label_idx}, null(Val)));

        else_label_true_idx = array_length(*instructions);
        array_append(*instructions, create_inst(I_LABEL, null(Val), null(Val)));

        // else-body
        generate_instructions_for_node(&ast->children[2], instructions, var_map_list);
    }

    end_label_true_idx = array_length(*instructions);
    array_append(*instructions, create_inst(I_LABEL, null(Val), null(Val)));

    if (ast->token.type == IF_STMT) {
        (*instructions)[first_jump_idx].arg1.i_val = end_label_true_idx;
    } else {
        (*instructions)[first_jump_idx].arg1.i_val = else_label_true_idx;
        (*instructions)[if_body_jump_idx].arg1.i_val = end_label_true_idx;
    }

}

void generate_instructions_for_while(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    int start_label_idx = array_length(*instructions);

    array_append(*instructions, create_inst(I_LABEL, null(Val), null(Val)));

    generate_instructions_for_node(&ast->children[0], instructions, var_map_list);

    if (ast->children[0].expected_return_type->kind != TYPE_bool) {
        bool result = generate_cvt_inst_for_types(ast->children[0].expected_return_type, &_const_types[TYPE_bool], instructions);

        if (!result) return_err(
            "Type '%s' is ambigous! Cannot be used as a while condition. (you did badly.)",
            type_get_name(ast->children[0].expected_return_type).data
        );
    }

    int jump_not_inst_idx = array_length(*instructions);
    array_append(*instructions, create_inst(I_JUMP_NOT, (Val){.type = TYPE_int, .i_val = -1}, null(Val)));

    // while body
    generate_instructions_for_node(&ast->children[1], instructions, var_map_list);

    array_append(*instructions, create_inst(I_JUMP, (Val){.type = TYPE_int, .i_val = start_label_idx}, null(Val)));

    int end_label_idx = array_length(*instructions);
    array_append(*instructions, create_inst(I_LABEL, null(Val), null(Val)));

    (*instructions)[jump_not_inst_idx].arg1.i_val = end_label_idx;


}

void generate_instructions_for_input(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    array_append(*instructions, create_inst(I_INPUT, null(Val), null(Val)));

    Type *goal_type = ast->children[0].expected_return_type;

    if (goal_type->kind != TYPE_string) {
        bool result = generate_cvt_inst_for_types(&_const_types[TYPE_string], goal_type, instructions);
        if (!result) return_err("Can't convert string to '%s'!", type_get_name(goal_type).data);
    }

    bool isglobal;

    VarHeader *vh = get_varheader_from_map_list(var_map_list, ast->children[0].token.text, &isglobal);

    array_append(*instructions, create_inst((isglobal? I_STACK_STORE_GLOBAL : I_STACK_STORE), (Val){.type = TYPE_int, .i_val = get_vartype_size(goal_type)}, (Val){.type = TYPE_int, .i_val = vh->var_pos}));

}


bool _get_all_vardecls_before_return(ASTNode *node, ASTNode *return_node, VarHeader **arr, LinkedList *var_map_list) {
    
    if (node == return_node) return true;

    bool found = return_node == NULL; // if it is null, just act like it's at the very end of the function


    for (int i = array_length(node->children) - 1; i >= 0; i--) {

        ASTNode *child = &node->children[i];

        if (!found) {
            bool result = _get_all_vardecls_before_return(child, return_node, arr, var_map_list);
            if (result) {
                found = true;
                continue;
            }
        } else {
            match (child->token.type) {
                case (DECL_ASSIGN_STMT, DECL_STMT) then ( ;
                    String name = child->children[1].token.text;
                    array_append(arr, get_varheader_from_map_list(var_map_list, name, NULL));
                )
                default (

                )
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

        array_append(arr, get_varheader_from_map_list(var_map_list, arg_name, NULL));
    }


    bool res = _get_all_vardecls_before_return(func_node, return_node, arr, var_map_list);
    if (!res && return_node != NULL) {
        print_err("Didn't find the return in the function provided! \n");
        return NULL;
    }

    return arr;
}

void generate_instructions_for_func_decl(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    array_append(*instructions, create_inst(I_JUMP, (Val){.type = TYPE_int, .i_val = -1}, null(Val)));
    int jump_inst_idx = array_length(*instructions) - 1;

    array_append(*instructions, create_inst(I_LABEL, null(Val), null(Val)));

    ASTNode var_args = ast->children[2];

    String func_name = ast->children[1].token.text;

    Type *type = ast->children[0].token.var_type;

    VarHeader vh = create_func_header(
        func_name, 
        type, 
        array_length(*instructions) - 1, 
        get_args_from_func_decl_ast(&var_args)
    );

    VarHeader *overloads = get_varheader_from_map_list_safe(var_map_list, func_name, NULL);
    if (overloads) {
        add_func_vh_to_overloads(overloads, vh);
    } else {
        VarHeader new_overloads = create_funcs_header(func_name, array(VarHeader, 2));
        array_append(new_overloads.funcs, vh);
        add_varheader_to_map_list(var_map_list, func_name, &new_overloads);
    }

    LL_prepend(var_map_list, LLNode_create(HashMap(VarHeader)));
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
    array_append(*instructions, create_inst(I_STACK_PTR_ADD, (Val){.type = TYPE_int, .i_val = size}, null(Val)));

    for (int i = array_length(var_args.children) - 1; i >= 0; i--) {
        String var_name = var_args.children[i].children[1].token.text;
        Type *type = var_args.children[i].children[0].token.var_type;
        array_append(*instructions, create_inst(I_STACK_STORE, (Val){.type = TYPE_int, .i_val = get_vartype_size(type)}, (Val){.type = TYPE_int, .i_val = gi_stack_pos}));
        VarHeader vh = create_var_header(var_name, type, gi_stack_pos);
        add_varheader_to_map_list(var_map_list, var_name, &vh);
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
            if (vardecls[i]->var_type->kind != TYPE_struct) continue;

            array_append(
                *instructions, 
                create_inst(I_READ, 
                    (Val){.type = TYPE_int, .i_val = get_vartype_size(vardecls[i]->var_type)}, 
                    (Val){.type = TYPE_int, .i_val = vardecls[i]->var_pos}
                )
            );

            array_append(*instructions, create_inst(I_DEC_REFCOUNT, null(Val), null(Val)));
        }

        array_free(vardecls);
    }




    gi_current_function = gi_prev_function;
    gi_stack_pos = prev_gi_stack_pos;
    HashMap_free(var_map_list->head->val);
    LL_pop_head(var_map_list);

    if ((*instructions)[array_length(*instructions) - 1].type != I_RETURN)
        array_append(*instructions, create_inst(I_RETURN, null(Val), null(Val)));


    (*instructions)[jump_inst_idx].arg1.i_val = array_length(*instructions);
}

// #UPDATE FOR STRUCTS
void generate_instructions_for_func_call(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    ASTNode func_args = ast->children[1];

    String func_name = ast->children[0].token.text;
    
    int len = array_length(func_args.children);
    

    VarHeader *overloads_vh = get_varheader_from_map_list(var_map_list, func_name, NULL);
    
    if (!overloads_vh) return_err("Tried to call function '%s' which doesn't exist!", func_name.data);
    
    VarHeader *func_vh = get_best_overload(overloads_vh, &func_args);


    for (int i = 0; i < len; i++) {
        ASTNode *arg = &func_args.children[i];
        generate_instructions_for_node(arg, instructions, var_map_list);

        // RC
        if (is_nontemporary_reference(arg)) {
            array_append(*instructions, create_inst(I_DUP, null(Val), null(Val)));
            array_append(*instructions, create_inst(I_INC_REFCOUNT, null(Val), null(Val)));            
        }


        Type *goal_type = func_vh->func_args[i].var_type;
        if (arg->expected_return_type != goal_type) {
            bool result = generate_cvt_inst_for_types(arg->expected_return_type, goal_type, instructions);
            if (!result) return_err(
                "Function argument #%d expected type '%s' but got '%s'!",
                i,
                type_get_name(goal_type).data,
                type_get_name(arg->expected_return_type).data
            );
        }
    }

    array_append(*instructions, create_inst(I_CALL, (Val){.type = TYPE_int, .i_val = func_vh->func_pos}, null(Val)));
}

void generate_instructions_for_unary_minus(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    generate_instructions_for_node(&ast->children[0], instructions, var_map_list);

    if (ast->expected_return_type->kind == TYPE_int) {
        array_append(*instructions, create_inst(I_PUSH, (Val){.type = TYPE_int, .i_val = 4}, (Val){.type = TYPE_int, .i_val = -1}));
        array_append(*instructions, create_inst(I_MUL, null(Val), null(Val)));
    } else if (ast->expected_return_type->kind == TYPE_float) {
        array_append(*instructions, create_inst(I_PUSH, (Val){.type = TYPE_int, .i_val = 8}, (Val){.type = TYPE_float, .f_val = -1}));
        array_append(*instructions, create_inst(I_MUL_FLOAT, null(Val), null(Val)));
    } else {
        print_err("Cannot apply unary minus to value of type '%s'!", type_get_name(ast->expected_return_type).data);
    }
}

void generate_instructions_for_struct_decl(ASTNode *ast, LinkedList *var_map_list) {
    String name = ast->children[0].token.text;
    ASTNode members = ast->children[1];

    VarHeader *arr = array(VarHeader, 2);

    int offset = sizeof(ObjectHeader);

    u32 *ref_offsets = array(int, 2);

    for (int i = 0; i < array_length(members.children); i++) {
        
        String var_name = members.children[i].children[1].token.text;
        Type *var_type = members.children[i].children[0].token.var_type;
        
        if (var_type->kind == TYPE_struct) {
            array_append(ref_offsets, (u32)offset);
        }

        VarHeader vh = create_var_header(var_name, var_type, offset);
    
        array_append(arr, vh);
        offset += get_vartype_size(var_type);
    }

    // debug {
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

    add_varheader_to_map_list(var_map_list, name, &vh);

}   


void generate_instructions_for_attr_access(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    generate_instructions_for_node(&ast->children[0], instructions, var_map_list);

    bool temp_refcount = is_temporary_reference(&ast->children[0]);

    if (temp_refcount) {
        array_append(*instructions, create_inst(I_DUP, null(Val), null(Val)));
        array_append(*instructions, create_inst(I_TUCK, null(Val), null(Val)));
    }

    // find member offset
    VarHeader *struct_vh = get_varheader_from_map_list(var_map_list, type_get_name(ast->children[0].expected_return_type), NULL);

    VarHeader *member_vh = find_attr_in_struct(struct_vh, ast->children[1].token.text);

    array_append(*instructions, create_inst(I_READ_ATTR, (Val){.type = TYPE_int, .i_val = get_vartype_size(member_vh->var_type)}, (Val){.type = TYPE_int, .i_val = member_vh->var_pos}));

    if (temp_refcount) {
        array_append(*instructions, create_inst(I_POP_BOTTOM, null(Val), null(Val)));
        array_append(*instructions, create_inst(I_DEC_REFCOUNT, null(Val), null(Val)));
    }
}
void generate_instructions_for_attr_addr(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    generate_instructions_for_node(&ast->children[0], instructions, var_map_list);

    // find member offset
    VarHeader *struct_vh = get_varheader_from_map_list(var_map_list, type_get_name(ast->children[0].expected_return_type), NULL);

    VarHeader *member_vh = find_attr_in_struct(struct_vh, ast->children[1].token.text);

    array_append(*instructions, create_inst(I_GET_ATTR_ADDR, (Val){.type = TYPE_int, .i_val = member_vh->var_pos}, null(Val)));

}

void generate_instructions_for_new(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    VarHeader *struct_vh = get_varheader_from_map_list(var_map_list, ast->children[0].token.text, NULL);

    array_append(*instructions, create_inst(I_ALLOC, (Val){.type = TYPE_int, .i_val = struct_vh->struct_size}, null(Val)));

    array_append(*instructions, create_inst(I_DUP, null(Val), null(Val))); // use the allocated address
    array_append(*instructions, create_inst(I_INIT_OBJ_HEADER, (Val){.type = TYPE_int, .i_val = struct_vh->struct_metadata_idx}, null(Val))); // use the allocated address

    ASTNode *member_assigns = &ast->children[1];

    for (int i = 0; i < array_length(member_assigns->children); i++) {
        String member_name = member_assigns->children[i].children[0].token.text;
        VarHeader *member_vh = find_attr_in_struct(struct_vh, member_name);

        array_append(*instructions, create_inst(I_DUP, null(Val), null(Val))); // use the allocated address
        array_append(*instructions, create_inst(I_GET_ATTR_ADDR, (Val){.type = TYPE_int, .i_val = member_vh->var_pos}, null(Val)));

        ASTNode *expr = &member_assigns->children[i].children[1];

        generate_instructions_for_node(expr, instructions, var_map_list);

        if (is_nontemporary_reference(expr)) {
            array_append(*instructions, create_inst(I_DUP, null(Val), null(Val)));
            array_append(*instructions, create_inst(I_INC_REFCOUNT, null(Val), null(Val)));
        }

        Type *goal_type = member_vh->var_type;
        Type *curr_type = member_assigns->children[i].children[1].expected_return_type;

        if (curr_type != goal_type) {
            bool result = generate_cvt_inst_for_types(curr_type, goal_type, instructions);
            if (!result) return_err("Couldn't convert type '%s' to type '%s' in attribute assignment!", 
                    type_get_name(curr_type).data,
                    type_get_name(goal_type).data
            );
        }

        array_append(*instructions, create_inst(I_HEAP_STORE, (Val){.type = TYPE_int, .i_val = get_vartype_size(member_vh->var_type)}, null(Val)));

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


    array_append(*instructions, create_inst(I_FREE, null(Val), null(Val)));
}

void generate_instructions_for_scope_ref_dec(HashMap *scope_map, Inst **instructions, bool global) {
    String *keys = scope_map->keys;

    for (int i = 0; i < array_length(keys); i++) {
        VarHeader *vh = HashMap_get(scope_map, keys[i]);
        if (vh->type != VH_VAR) continue;
        if (vh->var_type->kind != TYPE_struct) continue;

        array_append(*instructions, create_inst(global ? I_READ_GLOBAL : I_READ,
            (Val){.i_val = get_vartype_size(vh->var_type), .type = TYPE_int},
            (Val){.i_val = vh->var_pos, .type = TYPE_int}));
        array_append(*instructions, create_inst(I_DEC_REFCOUNT, null(Val), null(Val)));

    }
}



void generate_instructions_for_return_stmt(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {

    if (!gi_current_function) return_err("Tried to return outside of a function!");

    generate_instructions_for_node(&ast->children[0], instructions, var_map_list);

    if (is_nontemporary_reference(&ast->children[0])) {
        array_append(*instructions, create_inst(I_DUP, null(Val), null(Val)));
        array_append(*instructions, create_inst(I_INC_REFCOUNT, null(Val), null(Val)));
    }

    { 
        VarHeader **vardecls = get_all_vardecls_before_return(gi_current_function, ast, var_map_list);

        for (int i = 0; i < array_length(vardecls); i++) {
            VarHeader *decl = vardecls[i];

            printf("Name: %s, Type: %s \n", decl->name.data, type_get_name(decl->var_type).data);

            if (decl->var_type->kind != TYPE_struct) continue;

            array_append(
                *instructions, 
                create_inst(
                    I_READ, 
                    (Val){.type = TYPE_int, .i_val = get_vartype_size(decl->var_type)}, 
                    (Val){.type = TYPE_int, .i_val = decl->var_pos}
                )
            );

            array_append(*instructions, create_inst(I_DEC_REFCOUNT, null(Val), null(Val)));
        }


        array_free(vardecls);
    }

    array_append(*instructions, create_inst(I_RETURN, null(Val), null(Val)));
}

void generate_instructions_for_node(ASTNode *ast, Inst **instructions, LinkedList *var_map_list) {
    
    bool handled = true;

    match (ast->token.type) {
        case (DECL_ASSIGN_STMT, DECL_STMT) then (
            generate_instructions_for_vardecl(ast, instructions, var_map_list);
        )
        case (ASSIGN_STMT) then (
            generate_instructions_for_assign(ast, instructions, var_map_list);
        )
        case (PRINT_STMT, WRITE_STMT) then (
            generate_instructions_for_print(ast, instructions, var_map_list);
        )
        case (IF_STMT, IF_ELSE_STMT) then (
            generate_instructions_for_if(ast, instructions, var_map_list);
        )
        case (WHILE_STMT) then (
            generate_instructions_for_while(ast, instructions, var_map_list);
        )
        case (INPUT_STMT) then (
            generate_instructions_for_input(ast, instructions, var_map_list);
        )
        case (FUNC_DECL_STMT) then (
            generate_instructions_for_func_decl(ast, instructions, var_map_list);
        )
        case (FUNC_CALL) then (
            generate_instructions_for_func_call(ast, instructions, var_map_list);
        )
        case (OP_UNARY_MINUS) then (
            generate_instructions_for_unary_minus(ast, instructions, var_map_list);
        )
        case (STRUCT_DECL_STMT) then (
            generate_instructions_for_struct_decl(ast, var_map_list);
        )
        case (ATTR_ACCESS) then (
            generate_instructions_for_attr_access(ast, instructions, var_map_list);
        )
        case (OP_NEW) then (
            generate_instructions_for_new(ast, instructions, var_map_list);
        )
        case (DELETE_STMT) then (
            generate_instructions_for_delete(ast, instructions, var_map_list);
        )
        case (RETURN_STMT) then (
            generate_instructions_for_return_stmt(ast, instructions, var_map_list);
        )
        default (
            if (in_range(ast->token.type, BINOPS_START, BINOPS_END)) {
                generate_instructions_for_binop(ast, instructions, var_map_list);
            } else {
                handled = false;
            }
        )
    }

    if (handled) return;

    int temp_stack_ptr;

    // pre children operators
    match (ast->token.type) {
        case (STMT_SEQ) then ( ;
            int size = calc_stack_space_for_scope(ast);
            array_append(*instructions, create_inst(I_STACK_PTR_ADD, (Val){.type = TYPE_int, .i_val = size}, null(Val)));
        )
        case (BLOCK) then (
            temp_stack_ptr = gi_stack_pos;
            LL_prepend(var_map_list, LLNode_create(HashMap(VarHeader)));
        )
        
        default (

        )
    }
        
    for (int i = 0; i < array_length(ast->children); i++) {
        generate_instructions_for_node(&ast->children[i], instructions, var_map_list);
    }


    // post children operators
    match (ast->token.type) {
        case (INTEGER) then (
            array_append(*instructions, create_inst(I_PUSH, (Val){.type = TYPE_int, .i_val = 4}, 
                                                    (Val){.type = TYPE_int, .i_val = ast->token.int_val}));
        )
        case (FLOAT) then (
            array_append(*instructions, create_inst(I_PUSH, (Val){.type = TYPE_int, .i_val = 8}, 
                                                    (Val){.type = TYPE_float, .f_val = ast->token.double_val}));
        )
        case (BOOL) then (
            if (ast->token.int_val == MAYBE) {
                array_append(*instructions, create_inst(I_PUSH_MAYBE, null(Val), null(Val)));
            } else {
                array_append(*instructions, create_inst(I_PUSH, (Val){.type = TYPE_int, .i_val = 1}, 
                                                        (Val){.type = TYPE_bool, .b_val = ast->token.int_val}));
            }
        )
        case (NULL_REF) then (
            array_append(*instructions, create_inst(I_PUSH, (Val){.type = TYPE_int, .i_val = 8}, 
                                                    (Val){.type = TYPE_struct, .any_val = NULL}));
        )
        case (STRING_LITERAL) then (
            array_append(*instructions, create_inst(I_PUSH, (Val){.type = TYPE_int, .i_val = 8}, 
                                                    (Val){.type = TYPE_string, .s_val = ast->token.text.data}));
        )
        case (OP_ADD) then (
            array_append(*instructions, create_inst(I_ADD, null(Val), null(Val)));
        )
        case (OP_SUB) then (
            array_append(*instructions, create_inst(I_SUB, null(Val), null(Val)));
        )
        case (OP_MUL) then (
            array_append(*instructions, create_inst(I_MUL, null(Val), null(Val)));
        )
        case (OP_DIV) then (
            array_append(*instructions, create_inst(I_DIV, null(Val), null(Val)));
        )
        case (OP_MOD) then (
            array_append(*instructions, create_inst(I_MOD, null(Val), null(Val)));
        )
        case (OP_GREATER) then (
            array_append(*instructions, create_inst(I_GREATER, null(Val), null(Val)));
        )
        case (OP_GREATEREQ) then (
            array_append(*instructions, create_inst(I_GREATER_EQUAL, null(Val), null(Val)));
        )
        case (OP_LESS) then (
            array_append(*instructions, create_inst(I_LESS, null(Val), null(Val)));
        )
        case (OP_LESSEQ) then (
            array_append(*instructions, create_inst(I_LESS_EQUAL, null(Val), null(Val)));
        )
        case (OP_EQ) then (
            array_append(*instructions, create_inst(I_EQUAL, null(Val), null(Val)));
        )
        case (OP_NOTEQ) then (
            array_append(*instructions, create_inst(I_NOT_EQUAL, null(Val), null(Val)));
        )
        case (OP_NOT) then (
            array_append(*instructions, create_inst(I_NOT, null(Val), null(Val)));
        )
        case (NAME) then ( ;
            bool isglobal;
            VarHeader *vh = get_varheader_from_map_list(var_map_list, ast->token.text, &isglobal);
            array_append(*instructions, create_inst(isglobal ? I_READ_GLOBAL : I_READ,
                (Val){.i_val = get_vartype_size(vh->var_type), .type = TYPE_int},
                (Val){.i_val = vh->var_pos, .type = TYPE_int}));
        )
        case (BLOCK) then (

            generate_instructions_for_scope_ref_dec(var_map_list->head->val, instructions, false);

            HashMap_free(var_map_list->head->val);
            LL_pop_head(var_map_list);
            gi_stack_pos = temp_stack_ptr;
        )
        case (STMT_SEQ) then (
            generate_instructions_for_scope_ref_dec(var_map_list->head->val, instructions, true);
        )
        default (
            print_err("Unhandled case!");
            print_token(ast->token, 0);
        )
    }
}


Inst *generate_instructions(ASTNode *ast) {

    clear_struct_metadata();

    Inst *res = array(Inst, 20);
    LinkedList *var_map_list = LL_create();
    LL_append(var_map_list, LLNode_create(HashMap(VarHeader)));

    gi_stack_pos = 0;
    gi_label_idx = 0;
    generate_instructions_for_node(ast, &res, var_map_list);

    HashMap_free(var_map_list->head->val);

    LL_delete(var_map_list);

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
    switch (size) {
        case 0:
            break;
        case 1:
            *(u8 *)dst = *(u8 *)src;
            break;
        case 4:
            *(u32 *)dst = *(u32 *)src;
            break;
        case 8:
            *(u64 *)dst = *(u64 *)src;
            break;
        default:
            print_err("You should kill yourself NOW. unsupported size on my_memcpy(): %d", size);
            break;
    }
}


#define append(ptr, size) my_memcpy(temp_stack + temp_stack_ptr++, ptr, size)
#define pop(type) (*(type *)(temp_stack + --temp_stack_ptr))
#define dup() ({temp_stack[temp_stack_ptr] = temp_stack[temp_stack_ptr - 1]; temp_stack_ptr++;})
#define tuck(ptr, size) {memmove(temp_stack + 1, temp_stack, temp_stack_ptr++ * 8); my_memcpy(temp_stack, ptr, size);}
#define pop_bottom(type) ({type val = *(type *)temp_stack; memmove(temp_stack, temp_stack + 1, --temp_stack_ptr * 8); val;})

#define execute_instruction() switch (inst.type) { \
    case I_PUSH: \
        append(&inst.arg2.any_val, inst.arg1.i_val); \
        break; \
    case I_PUSH_MAYBE:; \
        bool m = rand() % 2; \
        append(&m, 1); \
        break; \
    case I_READ: { \
        append(var_stack + frame_ptr + inst.arg2.i_val, inst.arg1.i_val); \
        break; \
    } \
    case I_READ_GLOBAL: { \
        append(var_stack + inst.arg2.i_val, inst.arg1.i_val); \
        break; \
    } \
    case I_PRINT_INT: { \
        int num = pop(int); \
        printf("%d", num); \
        break; \
    } \
    case I_PRINT_STR: { \
        char *str = pop(char *); \
        printf("%s", str); \
        break; \
    } \
    case I_PRINT_FLOAT: { \
        double num = pop(double); \
        print_double(num); \
        break; \
    } \
    case I_PRINT_BOOL: { \
        bool b = pop(bool); \
        printf("%s", b ? "true" : "false"); \
        break; \
    } \
    case I_PRINT_NEWLINE: { \
        printf("\n"); \
        break; \
    } \
    case I_ADD: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) += num; \
        break; \
    } \
    case I_SUB: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) -= num; \
        break; \
    } \
    case I_MUL: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) *= num; \
        break; \
    } \
    case I_DIV: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) /= num; \
        break; \
    } \
    case I_MOD: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) %= num; \
        break; \
    } \
    case I_ADD_FLOAT: { \
        double num = pop(double); \
        double *top = (double *)(temp_stack + temp_stack_ptr - 1); \
        (*top) += num; \
        break; \
    } \
    case I_SUB_FLOAT: { \
        double num = pop(double); \
        double *top = (double *)(temp_stack + temp_stack_ptr - 1); \
        (*top) -= num; \
        break; \
    } \
    case I_MUL_FLOAT: { \
        double num = pop(double); \
        double *top = (double *)(temp_stack + temp_stack_ptr - 1); \
        (*top) *= num; \
        break; \
    } \
    case I_DIV_FLOAT: { \
        double num = pop(double); \
        double *top = (double *)(temp_stack + temp_stack_ptr - 1); \
        (*top) /= num; \
        break; \
    } \
    case I_MOD_FLOAT: { \
        double num = pop(double); \
        double *top = (double *)(temp_stack + temp_stack_ptr - 1); \
        (*top) = fmod(*top, num); \
        break; \
    } \
    case I_CONVERT_BOOL_FLOAT: { \
        double num = pop(bool) ? 1.0 : 0.0; \
        append(&num, 8); \
        break; \
    } \
    case I_CONVERT_BOOL_INT: { \
        int num = pop(bool) ? 1 : 0; \
        append(&num, 4); \
        break; \
    } \
    case I_CONVERT_BOOL_STR: { \
        StringRef string = pop(bool) == 0 ? StringRef("false") : StringRef("true");  \
        char *str = append_to_text_buffer(string.data, string.len); \
        append(&str, 8); \
        break; \
    } \
    case I_CONVERT_FLOAT_BOOL: { \
        bool b = pop(double) > 0 ? true : false; \
        append(&b, 1); \
        break; \
    } \
    case I_CONVERT_FLOAT_INT: { \
        int num = pop(double); \
        append(&num, 4); \
        break; \
    } \
    case I_CONVERT_FLOAT_STR: { \
        String string = String_from_double(pop(double), 2); \
        char *str = append_to_text_buffer(string.data, string.len); \
        String_delete(&string); \
        append(&str, 8); \
        break; \
    } \
    case I_CONVERT_INT_BOOL: { \
        bool b = pop(int) ? true : false; \
        append(&b, 1); \
        break; \
    } \
    case I_CONVERT_INT_FLOAT: { \
        double num = pop(int); \
        append(&num, 8); \
        break; \
    } \
    case I_CONVERT_INT_STR: { \
        String string = String_from_int(pop(int)); \
        char *str = append_to_text_buffer(string.data, string.len); \
        String_delete(&string); \
        append(&str, 8); \
        break; \
    } \
    case I_CONVERT_STR_FLOAT: { \
        char *str = pop(char *); \
        double res = String_to_double(StringRef(str)); \
        append(&res, 8); \
        break; \
    } \
    case I_CONVERT_STR_BOOL: { \
        char *str = pop(char *); \
        bool b = String_equal(StringRef(str), StringRef("true")); \
        append(&b, 1); \
        break; \
    } \
    case I_CONVERT_STR_INT: { \
        char *str = pop(char *); \
        int res = String_to_int(StringRef(str)); \
        append(&res, 4); \
        break; \
    } \
    case I_GREATER: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) = (*top) > num; \
        break; \
    } \
    case I_GREATER_EQUAL: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) = (*top) >= num; \
        break; \
    } \
    case I_LESS: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) = (*top) < num; \
        break; \
    } \
    case I_LESS_EQUAL: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) = (*top) <= num; \
        break; \
    } \
    case I_GREATER_FLOAT: { \
        double num = pop(double); \
        bool res = pop(double) > num; \
        append(&res, 1); \
        break; \
    } \
    case I_GREATER_EQUAL_FLOAT: { \
        double num = pop(double); \
        bool res = pop(double) >= num; \
        append(&res, 1); \
        break; \
    } \
    case I_LESS_FLOAT: { \
        double num = pop(double); \
        bool res = pop(double) < num; \
        append(&res, 1); \
        break; \
    } \
    case I_LESS_EQUAL_FLOAT: { \
        double num = pop(double); \
        bool res = pop(double) <= num; \
        append(&res, 1); \
        break; \
    } \
    case I_EQUAL: { \
        int num = pop(int); \
        bool res = pop(int) == num; \
        append(&res, 1); \
        break; \
    } \
    case I_EQUAL_FLOAT: { \
        double num = pop(double); \
        bool res = pop(double) == num; \
        append(&res, 1); \
        break; \
    } \
    case I_EQUAL_BOOL: { \
        bool b = pop(bool); \
        bool b2 = pop(bool); \
        bool res = b2 == b; \
        append(&res, 1); \
        break; \
    } \
    case I_EQUAL_STR: { \
        char *str = pop(char *); \
        char *str2 = pop(char *); \
        bool res = !strcmp(str, str2); \
        append(&res, 1); \
        break; \
    } \
    case I_NOT_EQUAL: { \
        int num = pop(int); \
        bool res = pop(int) != num; \
        append(&res, 1); \
        break; \
    } \
    case I_NOT_EQUAL_FLOAT: { \
        double num = pop(double); \
        bool res = pop(double) != num; \
        append(&res, 1); \
        break; \
    } \
    case I_NOT_EQUAL_BOOL: { \
        bool b = pop(bool); \
        bool res = pop(bool) != b; \
        append(&res, 1); \
        break; \
    } \
    case I_NOT_EQUAL_STR: { \
        char *str = pop(char *); \
        char *str2 = pop(char *); \
        bool res = !(!strcmp(str, str2)); \
        append(&res, 1); \
        break; \
    } \
    case I_STACK_STORE: { \
        my_memcpy(var_stack + frame_ptr + inst.arg2.i_val, temp_stack + --temp_stack_ptr, inst.arg1.i_val); \
        break; \
    } \
    case I_STACK_STORE_GLOBAL: { \
        my_memcpy(var_stack + inst.arg2.i_val, temp_stack + --temp_stack_ptr, inst.arg1.i_val); \
        break; \
    } \
    case I_JUMP: { \
        inst_ptr = inst.arg1.i_val - 1; \
        break; \
    } \
    case I_JUMP_IF: { \
        bool b = pop(bool); \
        if (b) inst_ptr = inst.arg1.i_val - 1; \
        break; \
    } \
    case I_JUMP_NOT: { \
        bool b = pop(bool); \
        if (!b) inst_ptr = inst.arg1.i_val - 1; \
        break; \
    } \
    case I_AND: { \
        bool b = pop(bool); \
        b = pop(bool) && b; \
        append(&b, 1); \
        break; \
    } \
    case I_OR: { \
        bool b = pop(bool); \
        b = pop(bool) || b; \
        append(&b, 1); \
        break; \
    } \
    case I_NOT: { \
        bool b = !pop(bool); \
        append(&b, 1); \
        break; \
    } \
    case I_INPUT: { \
        char *string_im_not_gonna_free = malloc(INPUT_BUFFER_SIZE); \
        fgets(string_im_not_gonna_free, INPUT_BUFFER_SIZE, stdin); \
        string_im_not_gonna_free[strlen(string_im_not_gonna_free) - 1] = 0; \
        char *str = append_to_text_buffer(string_im_not_gonna_free, strlen(string_im_not_gonna_free)); \
        free(string_im_not_gonna_free); \
        append(&str, 4); \
        break; \
    } \
    case I_CALL: { \
        Inst inst = instructions[inst_ptr]; \
        int val = inst_ptr + 1; \
        tuck(&frame_ptr, 4); \
        tuck(&stack_ptr, 4); \
        tuck(&val, 4); \
        frame_ptr += stack_ptr; \
        stack_ptr = 0; \
        inst_ptr = inst.arg1.i_val - 1; \
        break; \
    } \
    case I_RETURN: { \
        int ret_addr = pop_bottom(int); \
        inst_ptr = ret_addr - 1; \
        stack_ptr = pop_bottom(int); \
        frame_ptr = pop_bottom(int); \
        break; \
    } \
    case I_STACK_PTR_ADD: { \
        stack_ptr += instructions[inst_ptr].arg1.i_val; \
        break; \
    } \
    case I_LABEL: \
    break; \
    default: { \
        print_err("Too stupid. cant.\n"); \
        printf("instruction: %s \n", inst_names[inst.type]); \
        break; \
    } \
}

// #EXECUTE INSTRUCTIONS

#define execute_instruction_bytes() switch (byte_arr[inst_ptr]) { \
    case (I_POP_BOTTOM) then ({ \
        u64 obj = pop_bottom(u64); \
        append(&obj, 8); \
    }) \
    case (I_TUCK) then ({ \
        u64 obj = pop(u64); \
        tuck(&obj, 8); \
    }) \
    case (I_INIT_OBJ_HEADER) then ({; \
        void *obj = pop(void *); \
        u8 struct_meta_idx = byte_arr[++inst_ptr]; \
        inst_ptr += sizeof(int) - 1; \
        object_init_header(obj, struct_meta_idx); \
    }) \
    case(I_INC_REFCOUNT) then ({; \
        void *obj = pop(void *); \
        object_inc_ref(obj); \
    }) \
    case(I_DEC_REFCOUNT) then ({; \
        void *obj = pop(void *); \
        object_dec_ref(obj); \
    }) \
    case I_PUSH:; \
        int size = *(int *)&byte_arr[++inst_ptr]; \
        append(byte_arr + (inst_ptr += sizeof(int)), size); \
        inst_ptr += size - 1; \
        break; \
    case I_PUSH_MAYBE:; \
        bool m = rand() % 2; \
        temp_stack[temp_stack_ptr++] = m; \
        break; \
    case I_DUP: { \
        dup(); \
        break; \
    } \
    case I_READ: { \
        inst_ptr += 1; \
        int size = *(int *)&byte_arr[inst_ptr]; \
        inst_ptr += sizeof(int); \
        int pos = *(int *)&byte_arr[inst_ptr]; \
        inst_ptr += sizeof(int) - 1; \
        append(var_stack + frame_ptr + pos, size); \
        break; \
    } \
    case I_READ_GLOBAL: { \
        inst_ptr += 1; \
        int size = *(int *)&byte_arr[inst_ptr]; \
        inst_ptr += sizeof(int); \
        int pos = *(int *)&byte_arr[inst_ptr]; \
        inst_ptr += sizeof(int) - 1; \
        append(var_stack + pos, size); \
        break; \
    } \
    case I_READ_ATTR: { \
        inst_ptr += 1; \
        int size = *(int *)&byte_arr[inst_ptr]; \
        inst_ptr += sizeof(int); \
        int offset = *(int *)&byte_arr[inst_ptr]; \
        inst_ptr += sizeof(int) - 1; \
        char *addr = pop(char *); \
        if (!addr) print_err("Tried to get attribute of 'null'! inst: #%d \n", inst_ptr); \
        append(addr + offset, size); \
        break; \
    } \
    case I_GET_ATTR_ADDR: { \
        inst_ptr += 1; \
        int offset = *(int *)&byte_arr[inst_ptr]; \
        inst_ptr += sizeof(int) - 1; \
        char *addr = pop(char *); \
        if (!addr) print_err("Tried to get attribute of 'null'! inst: #%d \n", inst_ptr); \
        addr += offset; \
        append(&addr, sizeof(char *)); \
        break; \
    } \
    case I_STACK_STORE: { \
        inst_ptr += 1; \
        int size = *(int *)&byte_arr[inst_ptr]; \
        inst_ptr += sizeof(int); \
        int pos = *(int *)&byte_arr[inst_ptr]; \
        inst_ptr += sizeof(int) - 1; \
        my_memcpy(var_stack + frame_ptr + pos, temp_stack + --temp_stack_ptr, size); \
        break; \
    } \
    case I_STACK_STORE_GLOBAL: { \
        inst_ptr += 1; \
        int size = *(int *)&byte_arr[inst_ptr]; \
        inst_ptr += sizeof(int); \
        int pos = *(int *)&byte_arr[inst_ptr]; \
        inst_ptr += sizeof(int) - 1; \
        my_memcpy(var_stack + pos, temp_stack + --temp_stack_ptr, size); \
        break; \
    } \
    case I_HEAP_STORE: { \
        inst_ptr += 1; \
        int size = *(int *)&byte_arr[inst_ptr]; \
        inst_ptr += sizeof(int) - 1; \
        u64 value = pop(u64); \
        char *addr = pop(char *); \
        memcpy(addr, &value, size); \
        break; \
    } \
    case I_ALLOC: { \
        inst_ptr += 1; \
        int size = *(int *)&byte_arr[inst_ptr]; \
        inst_ptr += sizeof(int) - 1; \
        void *addr = alloc_object(size); \
        append(&addr, sizeof(addr)); \
        break; \
    } \
    case I_FREE: { \
        print_err("Bro tried to delete manually"); \
        break; \
    } \
    case I_JUMP: { \
        int pos = *(int *)&byte_arr[++inst_ptr]; \
        inst_ptr = pos - 1; \
        break; \
    } \
    case I_JUMP_IF: { \
        bool b = pop(bool); \
        int pos = *(int *)&byte_arr[++inst_ptr]; \
        if (b) inst_ptr = pos - 1; \
        else inst_ptr += sizeof(int) - 1; \
        break; \
    } \
    case I_JUMP_NOT: { \
        bool b = pop(bool); \
        int pos = *(int *)&byte_arr[++inst_ptr]; \
        if (!b) inst_ptr = pos - 1; \
        else inst_ptr += sizeof(int) - 1; \
        break; \
    } \
    case I_CALL: { \
        int callpos = *(int *)&byte_arr[++inst_ptr]; \
        inst_ptr += sizeof(int) - 1; \
        int val = inst_ptr + 1; \
        tuck(&frame_ptr, sizeof(int)); \
        tuck(&stack_ptr, sizeof(int)); \
        tuck(&val, sizeof(int)); \
        frame_ptr += stack_ptr; \
        stack_ptr = 0; \
        inst_ptr = callpos - 1; \
        break; \
    } \
    case I_RETURN: { \
        int ret_addr = pop_bottom(int); \
        inst_ptr = ret_addr - 1; \
        stack_ptr = pop_bottom(int); \
        frame_ptr = pop_bottom(int); \
        break; \
    } \
    case I_STACK_PTR_ADD: { \
        inst_ptr += 1; \
        int size = *(int *)(byte_arr + inst_ptr);\
        inst_ptr += sizeof(int) - 1; \
        stack_ptr += size; \
        break; \
    } \
    case I_PRINT_INT: { \
        int num = pop(int); \
        printf("%d", num); \
        break; \
    } \
    case I_PRINT_STR: { \
        char *str = pop(char *); \
        printf("%s", str); \
        break; \
    } \
    case I_PRINT_FLOAT: { \
        double num = pop(double); \
        print_double(num); \
        break; \
    } \
    case I_PRINT_BOOL: { \
        bool b = pop(bool); \
        printf("%s", b ? "true" : "false"); \
        break; \
    } \
    case I_PRINT_NEWLINE: { \
        printf("\n"); \
        break; \
    } \
    case I_ADD: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) += num; \
        break; \
    } \
    case I_SUB: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) -= num; \
        break; \
    } \
    case I_MUL: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) *= num; \
        break; \
    } \
    case I_DIV: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) /= num; \
        break; \
    } \
    case I_MOD: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) %= num; \
        break; \
    } \
    case I_ADD_FLOAT: { \
        double num = pop(double); \
        double *top = (double *)(temp_stack + temp_stack_ptr - 1); \
        (*top) += num; \
        break; \
    } \
    case I_SUB_FLOAT: { \
        double num = pop(double); \
        double *top = (double *)(temp_stack + temp_stack_ptr - 1); \
        (*top) -= num; \
        break; \
    } \
    case I_MUL_FLOAT: { \
        double num = pop(double); \
        double *top = (double *)(temp_stack + temp_stack_ptr - 1); \
        (*top) *= num; \
        break; \
    } \
    case I_DIV_FLOAT: { \
        double num = pop(double); \
        double *top = (double *)(temp_stack + temp_stack_ptr - 1); \
        (*top) /= num; \
        break; \
    } \
    case I_MOD_FLOAT: { \
        double num = pop(double); \
        double *top = (double *)(temp_stack + temp_stack_ptr - 1); \
        (*top) = fmod(*top, num); \
        break; \
    } \
    case I_CONVERT_BOOL_FLOAT: { \
        double num = pop(bool) ? 1.0 : 0.0; \
        append(&num, 8); \
        break; \
    } \
    case I_CONVERT_BOOL_INT: { \
        int num = pop(bool) ? 1 : 0; \
        append(&num, 4); \
        break; \
    } \
    case I_CONVERT_BOOL_STR: { \
        StringRef string = pop(bool) == 0 ? StringRef("false") : StringRef("true");  \
        char *str = append_to_text_buffer(string.data, string.len); \
        append(&str, 8); \
        break; \
    } \
    case I_CONVERT_FLOAT_BOOL: { \
        bool b = pop(double) > 0 ? true : false; \
        append(&b, 1); \
        break; \
    } \
    case I_CONVERT_FLOAT_INT: { \
        int num = pop(double); \
        append(&num, 4); \
        break; \
    } \
    case I_CONVERT_FLOAT_STR: { \
        String string = String_from_double(pop(double), 2); \
        char *str = append_to_text_buffer(string.data, string.len); \
        String_delete(&string); \
        append(&str, 8); \
        break; \
    } \
    case I_CONVERT_INT_BOOL: { \
        bool b = pop(int) ? true : false; \
        append(&b, 1); \
        break; \
    } \
    case I_CONVERT_INT_FLOAT: { \
        double num = pop(int); \
        append(&num, 8); \
        break; \
    } \
    case I_CONVERT_INT_STR: { \
        String string = String_from_int(pop(int)); \
        char *str = append_to_text_buffer(string.data, string.len); \
        String_delete(&string); \
        append(&str, 8); \
        break; \
    } \
    case I_CONVERT_STR_FLOAT: { \
        char *str = pop(char *); \
        double res = String_to_double(StringRef(str)); \
        append(&res, 8); \
        break; \
    } \
    case I_CONVERT_STR_BOOL: { \
        char *str = pop(char *); \
        bool b = String_equal(StringRef(str), StringRef("true")); \
        append(&b, 1); \
        break; \
    } \
    case I_CONVERT_STR_INT: { \
        char *str = pop(char *); \
        int res = String_to_int(StringRef(str)); \
        append(&res, 4); \
        break; \
    } \
    case I_GREATER: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) = (*top) > num; \
        break; \
    } \
    case I_GREATER_EQUAL: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) = (*top) >= num; \
        break; \
    } \
    case I_LESS: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) = (*top) < num; \
        break; \
    } \
    case I_LESS_EQUAL: { \
        int num = pop(int); \
        int *top = (int *)(temp_stack + temp_stack_ptr - 1); \
        (*top) = (*top) <= num; \
        break; \
    } \
    case I_GREATER_FLOAT: { \
        double num = pop(double); \
        bool res = pop(double) > num; \
        append(&res, 1); \
        break; \
    } \
    case I_GREATER_EQUAL_FLOAT: { \
        double num = pop(double); \
        bool res = pop(double) >= num; \
        append(&res, 1); \
        break; \
    } \
    case I_LESS_FLOAT: { \
        double num = pop(double); \
        bool res = pop(double) < num; \
        append(&res, 1); \
        break; \
    } \
    case I_LESS_EQUAL_FLOAT: { \
        double num = pop(double); \
        bool res = pop(double) <= num; \
        append(&res, 1); \
        break; \
    } \
    case I_EQUAL: { \
        int num = pop(int); \
        bool res = pop(int) == num; \
        append(&res, 1); \
        break; \
    } \
    case I_EQUAL_FLOAT: { \
        double num = pop(double); \
        bool res = pop(double) == num; \
        append(&res, 1); \
        break; \
    } \
    case I_EQUAL_BOOL: { \
        bool b = pop(bool); \
        bool b2 = pop(bool); \
        bool res = b2 == b; \
        append(&res, 1); \
        break; \
    } \
    case I_EQUAL_STR: { \
        char *str = pop(char *); \
        char *str2 = pop(char *); \
        bool res = !strcmp(str, str2); \
        append(&res, 1); \
        break; \
    } \
    case I_EQUAL_REF: { \
        char *p1 = pop(char *); \
        char *p2 = pop(char *); \
        bool res = p1 == p2; \
        append(&res, 1); \
        break; \
    } \
    case I_NOT_EQUAL: { \
        int num = pop(int); \
        bool res = pop(int) != num; \
        append(&res, 1); \
        break; \
    } \
    case I_NOT_EQUAL_FLOAT: { \
        double num = pop(double); \
        bool res = pop(double) != num; \
        append(&res, 1); \
        break; \
    } \
    case I_NOT_EQUAL_BOOL: { \
        bool b = pop(bool); \
        bool res = pop(bool) != b; \
        append(&res, 1); \
        break; \
    } \
    case I_NOT_EQUAL_STR: { \
        char *str = pop(char *); \
        char *str2 = pop(char *); \
        bool res = !(!strcmp(str, str2)); \
        append(&res, 1); \
        break; \
    } \
    case I_NOT_EQUAL_REF: { \
        char *p1 = pop(char *); \
        char *p2 = pop(char *); \
        bool res = p1 != p2; \
        append(&res, 1); \
        break; \
    } \
    case I_AND: { \
        bool b = pop(bool); \
        b = pop(bool) && b; \
        append(&b, 1); \
        break; \
    } \
    case I_OR: { \
        bool b = pop(bool); \
        b = pop(bool) || b; \
        append(&b, 1); \
        break; \
    } \
    case I_NOT: { \
        bool b = !pop(bool); \
        append(&b, 1); \
        break; \
    } \
    case I_INPUT: { \
        char *string_im_not_gonna_free = malloc(INPUT_BUFFER_SIZE); \
        fgets(string_im_not_gonna_free, INPUT_BUFFER_SIZE, stdin); \
        string_im_not_gonna_free[strlen(string_im_not_gonna_free) - 1] = 0; \
        char *str = append_to_text_buffer(string_im_not_gonna_free, strlen(string_im_not_gonna_free)); \
        free(string_im_not_gonna_free); \
        append(&str, 4); \
        break; \
    } \
    case I_LABEL: \
        break; \
    default: { \
        print_err("Too stupid. cant.\n"); \
        printf("instruction: #%d: %s \n", inst_ptr, inst_names[(int)byte_arr[inst_ptr]]); \
        break; \
    } \
}


void *append_to_text_buffer(const char *text, int len) {
    memcpy(text_buffer + text_buffer_ptr, text, len);
    void *retval = text_buffer + text_buffer_ptr;
    text_buffer_ptr += len + 1;
    return retval;
}

void preprocess_string_literals(Inst *instructions) {
    
    int len = array_length(instructions);
    
    for (int i = 0; i < len; i++) {
        Inst inst = instructions[i];
        if (inst.type == I_PUSH && inst.arg1.type == TYPE_string) {
            inst.arg1.s_val = append_to_text_buffer(inst.arg1.s_val, strlen(inst.arg1.s_val));
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

            my_memcpy(value, &instructions[i].arg1.any_val, 8);

            if (instructions[i].type == I_JUMP 
                || instructions[i].type == I_JUMP_NOT 
                || instructions[i].type == I_JUMP_IF
                || instructions[i].type == I_CALL) {
                *(int *)value = new_indicies[instructions[i].arg1.i_val];
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
                array_append(byte_arr, ((char *)&instructions[i].arg2.any_val)[j]);
            }
        }
    }

    free(new_indicies);

    return byte_arr;
}

void run_bytecode_instructions(Inst *instructions, double *time) {
    memset(temp_stack, 0, STACK_SIZE);
    temp_stack_ptr = 0;
    memset(var_stack, 0, STACK_SIZE);
    memset(text_buffer, 0, TEXT_BUF_SIZE);
    text_buffer_ptr = 0;
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

    

    double start = get_current_process_time_seconds();

    int len = array_length(byte_arr); // which is not equal to array_length(instructions)
    for (int inst_ptr = 0; inst_ptr < len; inst_ptr++) {
        // printf("inst ptr: %d \n", inst_ptr);
        execute_instruction_bytes();
    }

    double end = get_current_process_time_seconds();

    if (runtime_mallocs > runtime_frees) {
        print_err("Detected a memory leak! Where? you figure it out.");
    } else if (runtime_frees > runtime_mallocs) {
        print_err("Detected excess memory deletions! How did the program even survive this far?");
    } else {
        printf("No memory leaks. \n");
    }

    if (time != NULL) *time = end - start;


    array_free(byte_arr);
}

void run_instructions(Inst *instructions, double *time) {
    
    memset(temp_stack, 0, STACK_SIZE);
    temp_stack_ptr = 0;
    memset(var_stack, 0, STACK_SIZE);
    memset(text_buffer, 0, TEXT_BUF_SIZE);
    text_buffer_ptr = 0;
    stack_ptr = 0;
    frame_ptr = 0;

    preprocess_string_literals(instructions);

    int len = array_length(instructions);
    
    double start = get_current_process_time_seconds();

    for (int inst_ptr = 0; inst_ptr < len; inst_ptr++) {
        Inst inst = instructions[inst_ptr];
        // printf("inst %d \n", inst_ptr);
        execute_instruction();
    }

    double end = get_current_process_time_seconds();

    if (time != NULL) *time = end - start;
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

    run_bytecode_instructions(instructions, &time);

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
//             case I_STACK_PTR_ADD:
//                 write("\tadd rsp, %d \n", inst.arg1.i_val);
//                 break;
//             case I_PUSH:
//                 if (inst.arg1.type == TYPE_int) {
//                     write("\tpush %d \n", inst.arg1.i_val);
//                 }
//                 break;
//             case I_ADD:
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

String *lex(StringRef text) {

    String *parts = array(String, 10);
    
    char buf[1024] = {0};
    int pos = 0;

    bool in_literal = false;


    for (int i = 0; i < text.len - 1; i++) {
        
        char c = text.data[i];

        if (c == '"') {
            if (!in_literal) {
                if (pos > 0) {
                    array_append(parts, String_ncopy_from_literal(buf, pos));
                    pos = 0;
                }
                in_literal = true;
            } else {
                buf[pos++] = c;
                array_append(parts, String_ncopy_from_literal(buf, pos));
                pos = 0;
                in_literal = false;
                continue;
            }
        }

        if (in_literal) {
            buf[pos++] = c;
            continue;
        }

        bool stop_char = is_stop_char(c);
        

        if (stop_char && pos > 0) {
            array_append(parts, String_ncopy_from_literal(buf, pos));
            pos = 0;

        }
        
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r' ) continue;

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
        );

        #undef check

        if (stop_char) {

            if (part_of_double_symbol) {
                buf[pos++] = text.data[i + 1];
                i++;
            }

            array_append(parts, String_ncopy_from_literal(buf, pos));
            pos = 0;
        }
    }

    if (pos > 0) {
        for (int i = 0; i < pos; i++) {
            if (buf[pos] == '"') in_literal = !in_literal;
        }
        array_append(parts, String_ncopy_from_literal(buf, pos));
        pos = 0;
    }

    if (in_literal) {
        print_err("Missing closing quotes in string literal!");
    }

    return parts;

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
    switch (token.type) {
        case INTEGER:
            printf(", %d", token.int_val);
            break;
        case FLOAT:
            printf(", %.2f", token.double_val);
            break;
        case BOOL:
            printf(", %s", token.int_val == 1 ? "true" : ((char)token.int_val == MAYBE ? "maybe" : "false"));
            break;
        case STRING_LITERAL:
            printf(", \"%s\"", token.text.data);
            break;
        case NAME:
            printf(", %s", token.text.data);
            break;
        case KEYWORD:
            printf(", %s", token.text.data);
            break;
        case TYPE:
            printf(", ");
            print_type(token.var_type);
            break;

        default:
            break;
    }

    printf("] \n");
}

void print_tokens(Token *tokens) {
    for (int i = 0; i < array_length(tokens); i++) {
        printf("#%d: ", i);
        print_token(tokens[i], 0);
    }
}

void print_str_parts(String *parts) {
    for (int i = 0; i < array_length(parts); i++) {
        printf("> '%s' len: %d\n", parts[i].data, parts[i].len);
    }
}

#define free_parts(parts) do { \
    _free_parts(parts); \
    parts = NULL; \
} while (0)

void _free_parts(String *parts) {
    for (int i = 0; i < array_length(parts); i++) {
        String_delete(&parts[i]);
    }

    array_free(parts);
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

        filepath[0] = '.'; filepath[1] = '.'; filepath[2] = '/';

        fgets((char *)filepath + 3, sizeof(filepath) - 3, stdin);

        filepath[strlen(filepath) - 1] = 0;

        printf("Reading from file: '%s' \n", filepath);

        FILE *file = fopen(filepath, "r");

        if (!file) {
            print_err("Couldn't open file! errno: %d ", errno);                
            return RESULT_COULDNT_OPEN_FILE;
        }

        char *buf_ptr = buf;

        while (fgets(buf_ptr, bufsize - (buf_ptr - buf), file)) {
            int len = strlen(buf_ptr);
            if (len > 0 && buf_ptr[len - 1] == '\n') {
                buf_ptr[len - 1] = ' '; // get rid of \n
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

    while (true) {


        char buf[CODE_MAX_LEN] = {0};
        bool benchmark;

        int result = handle_text_interface(buf, CODE_MAX_LEN, &benchmark);

        if (result != RESULT_OK) {
            continue;
        }

        String *parts = lex(StringRef(buf));

        if (LEXER_PRINT) print_str_parts(parts);

        if (COMPILATION_STAGE < STAGE_TOKENIZER) {
            free_parts(parts);
            continue;
        }

        Token *tokens = tokenize_parts(parts);

        if (TOKENIZER_PRINT) print_tokens(tokens);

        set_parse_tokens(tokens);

        if (COMPILATION_STAGE < STAGE_PARSER) {
            free_tokens(tokens);
            free_parts(parts);
            continue;
        }

        ParseResult res = parse_program();
        
        if (!res.success) {
            printf("%s \n", parse_err);
            debug printf("%d tokens deep \n", parse_err_most_tokens);
            print_err("Invalid program!");
        }

        if (PREPROCESS_AST) preprocess_ast(&res.node);

        if (PARSER_PRINT) {
            printf(">>> RESULT AST <<<\n");
            print_ast(res.node, 0);
        }

        if (!res.success) {
            print_err("Failed to parse AST! Invalid expression!");
            free_tokens(tokens);
            free_parts(parts);
            continue;
        }

        if (COMPILATION_STAGE < STAGE_IR_GEN) {
            free_ast(res.node);
            free_tokens(tokens);
            free_parts(parts);
            continue;
        }

        Inst *instructions = generate_instructions(&res.node);

        if (IR_PRINT) print_instructions(instructions);

        if (COMPILATION_STAGE < STAGE_RUN_CODE) {
            array_free(instructions);
            free_ast(res.node);
            free_tokens(tokens);
            free_parts(parts);
            continue;
        }

        // place for chaos. increment when this made you want to kys: 2
        if (benchmark) {
            run_benchmark(instructions);
        } else {
            run_program(instructions);
        }

        array_free(instructions);

        free_tokens(tokens);

        free_parts(parts);
    }

    

    return 0;
}

// #END