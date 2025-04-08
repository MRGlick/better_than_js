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

const char SYMBOLS[] = {
    ' ', ',', ';', '(', ')', '{', '}', '+', '-', '/', '*', '=', '>', '<', '!', '&', '|'
};

char *KEYWORDS[] = {
    "if",
    "while",
    "for",
    "print",
    "else",
    "input",
    "return",
    "defer", // TODO
    "struct" // TODO
};

// dont ask
#define MAYBE 2


typedef struct ASTNode {
    Token token;
    struct ASTNode *children;
    VarType expected_return_type;
    bool complete;
} ASTNode;

void print_token(Token token, int level);

bool is_char_alpha(char c) {
    return (c >= 'a' && c <= 'z');
}

bool is_char_num(char c) {
    return (c >= '0' && c <= '9');
}

VarType check_vartype(String str) {
    int len = sizeof(var_type_names) / sizeof(char *);

    for (int i = 0; i < len; i++) {
        if (String_equal(str, StringRef(var_type_names[i]))) {
            return i;
        }
    }

    return -1;
}

int is_num(String part) {

    if (part.len == 0) return 0;

    int dot_count = 0;

    for (int i = 0; i < part.len; i++) {
        if (!is_char_num(part.data[i]) && part.data[i] != '.') return 0;
        if (part.data[i] == '.') dot_count++;
    }

    if (dot_count > 1) {
        pause_err("Invalid number! More than one decimal dot isn't allowed!");
        return 0;
    }

    if (dot_count == 1) {
        return 2;
    }

    return 1;
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

bool is_vartype(String part) {
    if (part.len == 0) return false;

    int len = sizeof(var_type_names) / sizeof (char *);

    for (int i = 0; i < len; i++) {
        if (String_equal(part, StringRef(var_type_names[i]))) {
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
        if (is_keyword(parts[i])) {
            Token tk = {.type = KEYWORD, .text = parts[i]};
            array_append(tokens, tk);
            continue;
        }
        if (is_vartype(parts[i])) {
            Token tk = {.type = TYPE, .var_type = check_vartype(parts[i])};
            array_append(tokens, tk);

            continue;
        }

        if (is_name(parts[i])) {
            Token tk = {.type = NAME, .text = parts[i]};
            array_append(tokens, tk);
            continue;
        }
        int isnum = is_num(parts[i]);

        if (isnum == 1) {
            Token tk = {.type = INTEGER, .int_val = parse_int(parts[i])};
            array_append(tokens, tk);
            continue;
        } else if (isnum == 2) {
            Token tk = {.type = FLOAT, .double_val = parse_double(parts[i])};
            array_append(tokens, tk);
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
    node.expected_return_type = T_VOID;
    
    return node;
}

#define free_ast(ast) do { \
    int result = _free_ast(ast); \
    if (result == -1) printf("On line %d \n", __LINE__); \
    ast = (ASTNode){0}; \
} while (0)

int _free_ast(ASTNode ast) {
    
    if (ast.children == NULL) {
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

#define PARSE_FAILED ((ParseResult){0})

int parse_idx = 0;
int parse_anchor = 0;
Token *parse_tokens = NULL;

void set_parse_tokens(Token *tokens) {
    parse_tokens = tokens;
    parse_idx = 0;
}

Token get_token(int idx) {
    if (parse_tokens == NULL) {
        print_err("Tried to match tokens, but there aren't any!");
        return null(Token);
    }
    if (idx >= array_length(parse_tokens)) {
        //print_err("Tried to match token, but reached end of tokens!");
        return null(Token);
    }
    return parse_tokens[idx];
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


ParseResult parse_expr(int idx);

ParseResult parse_base_rule(int idx);

ParseResult parse_add_rule(int idx);

ParseResult parse_func_call(int idx);

void print_ast(ASTNode node, int level);



ParseResult parse_value(int idx) {
    
    ParseResult func_call_res = parse_func_call(idx);
    if (func_call_res.success) {
        return create_parse_result(true, func_call_res.node, func_call_res.endpos);
    }
    if (get_token(idx).type == BOOL
        || get_token(idx).type == STRING_LITERAL
        || get_token(idx).type == INTEGER
        || get_token(idx).type == FLOAT
        || get_token(idx).type == NAME
    ) { 
        int token_idx = idx;
        idx += 1;
        return ((ParseResult){.success = 1, .node = create_ast_node(get_token(token_idx), 1), .endpos = idx});
    }

    if (get_token(idx).type == OP_SUB) {
        idx += 1;
        ParseResult base_rule_res = parse_base_rule(idx);
        if (base_rule_res.success) {
            idx = base_rule_res.endpos;
            if (base_rule_res.node.token.type == INTEGER) base_rule_res.node.token.int_val *= -1;
            else if (base_rule_res.node.token.type == FLOAT) base_rule_res.node.token.double_val *= -1;
            else {
                ASTNode node = create_ast_node((Token){.type = OP_MUL}, true);
                array_append(node.children, base_rule_res.node);
                array_append(node.children, create_ast_node((Token){.type = INTEGER, .int_val = -1}, true));                
                base_rule_res.node = node;
            }
            return create_parse_result(true, base_rule_res.node, idx);
        }
    }



    return PARSE_FAILED;
}

ParseResult parse_base_rule(int idx) {
    ParseResult value_res = parse_value(idx);

    if (value_res.success) {
        return value_res;
    }

    if (get_token(idx).type == OP_NOT) {
        int op_idx = idx;
        idx += 1;

        ParseResult base_rule_res = parse_base_rule(idx);
        if (base_rule_res.success) {
            idx = base_rule_res.endpos;

            ASTNode node = create_ast_node(get_token(op_idx), true);

            array_append(node.children, base_rule_res.node);

            return create_parse_result(true, node, idx);
        }

        return PARSE_FAILED;

    }

    if (get_token(idx).type == LPAREN) {
        idx += 1;
        ParseResult expr_res = parse_expr(idx);
        if (expr_res.success) {
            idx = expr_res.endpos;
            if (get_token(idx).type == RPAREN) {
                idx += 1;
                expr_res.node.complete = true;
                return create_parse_result(true, expr_res.node, idx);
            }
            free_ast(expr_res.node);
            return PARSE_FAILED;
            
        }
        return PARSE_FAILED;
        
    }

    return PARSE_FAILED;
}

#define is_null_ast(ast) (ast.children == NULL)

ParseResult parse_mul_rule_h(int idx) {

    if (get_token(idx).type == OP_MUL 
        || get_token(idx).type == OP_DIV
        || get_token(idx).type == OP_MOD) {
        int op_idx = idx;
        idx += 1;
        ParseResult factor_res = parse_base_rule(idx);
        if (factor_res.success) {
            idx = factor_res.endpos;
            ParseResult term_h_res = parse_mul_rule_h(idx);
            if (term_h_res.success) {

                idx = term_h_res.endpos;
                ASTNode node = create_ast_node(get_token(op_idx), false);
                
                array_append(node.children, factor_res.node);

                if (!is_null_ast(term_h_res.node)) {
                    ASTNode leaf = term_h_res.node;
                    while (!leaf.children[0].complete) {
                        leaf = leaf.children[0];
                    }
                    array_insert(leaf.children, node, 0);
                    node.complete = true;
                    return create_parse_result(true, term_h_res.node, idx);
                }

                return create_parse_result(true, node, idx);
            }
            free_ast(factor_res.node);
            return PARSE_FAILED;
        }

        return PARSE_FAILED;
    }

    return create_parse_result(true, null(ASTNode), idx);
}

ParseResult parse_mul_rule(int idx) {
    ParseResult factor_res = parse_base_rule(idx);
    if (factor_res.success) {
        idx = factor_res.endpos;
        ParseResult term_h_res = parse_mul_rule_h(idx);
        if (term_h_res.success) {

            idx = term_h_res.endpos;
            if (is_null_ast(term_h_res.node)) {
                return create_parse_result(true, factor_res.node, idx);
            } else {
                ASTNode leaf = term_h_res.node;
                while (!leaf.children[0].complete) {
                    leaf = leaf.children[0];
                }
                array_insert(leaf.children, factor_res.node, 0);

                term_h_res.node.complete = true;
                return create_parse_result(true, term_h_res.node, idx);
            }
        }

        free_ast(factor_res.node);
        return PARSE_FAILED;
    }

    return PARSE_FAILED;
}

ParseResult parse_add_rule_h(int idx) {
    
    if (get_token(idx).type == OP_ADD || get_token(idx).type == OP_SUB) {
        int op_idx = idx;
        idx += 1;
        ParseResult term_res = parse_mul_rule(idx);
        if (term_res.success) {
            idx = term_res.endpos;
            ParseResult expr_h_result = parse_add_rule_h(idx);
            
            if (expr_h_result.success) {
                idx = expr_h_result.endpos;
                ASTNode node = create_ast_node(get_token(op_idx), false);
                
                array_append(node.children, term_res.node);

                

                if (!is_null_ast(expr_h_result.node)) {
                    ASTNode leaf = expr_h_result.node;
                    while (!leaf.children[0].complete) {
                        leaf = leaf.children[0];
                    }
                    array_insert(leaf.children, node, 0);
                    
                    // printf("expr h current tree: \n");
                    // print_ast(expr_h_result.node, 0);

                    expr_h_result.node.complete = true;

                    return create_parse_result(true, expr_h_result.node, idx);
                }


                // printf("expr h current tree: \n");
                // print_ast(node, 0);
                return create_parse_result(true, node, idx);
            }

            free_ast(term_res.node);
            return PARSE_FAILED;

        }
        return PARSE_FAILED;
    }

    return create_parse_result(true, null(ASTNode), idx);
}

// 1 + (1 + 1)

ParseResult parse_add_rule(int idx) {
    ParseResult term_res = parse_mul_rule(idx);
    
    if (term_res.success) {
        idx = term_res.endpos;
        ParseResult expr_h_res = parse_add_rule_h(idx);
        if (expr_h_res.success) {
            idx = expr_h_res.endpos;
            if (is_null_ast(expr_h_res.node)) {
                return create_parse_result(true, term_res.node, idx);
            } else {
                ASTNode leaf = expr_h_res.node;
                while (!leaf.children[0].complete) {
                    leaf = leaf.children[0];
                }
                array_insert(leaf.children, term_res.node, 0);
                expr_h_res.node.complete = true;
                return create_parse_result(true, expr_h_res.node, idx);
            }
        }

        free_ast(term_res.node);
        return PARSE_FAILED;
    }

    return PARSE_FAILED;
}

ParseResult parse_rel_rule_h(int idx) {
    if (get_token(idx).type == OP_EQ 
        || get_token(idx).type == OP_NOTEQ 
        || get_token(idx).type == OP_GREATER 
        || get_token(idx).type == OP_GREATEREQ
        || get_token(idx).type == OP_LESS
        || get_token(idx).type == OP_LESSEQ
        ) {
        int op_idx = idx;
        idx += 1;
        ParseResult add_rule_res = parse_add_rule(idx);
        if (add_rule_res.success) {
            idx = add_rule_res.endpos;
            ParseResult rel_rule_h_result = parse_rel_rule_h(idx);
            
            if (rel_rule_h_result.success) {
                idx = rel_rule_h_result.endpos;
                ASTNode node = create_ast_node(get_token(op_idx), false);
                
                array_append(node.children, add_rule_res.node);

                

                if (!is_null_ast(rel_rule_h_result.node)) {
                    ASTNode leaf = rel_rule_h_result.node;
                    while (!leaf.children[0].complete) {
                        leaf = leaf.children[0];
                    }
                    array_insert(leaf.children, node, 0);
                    
                    // printf("expr h current tree: \n");
                    // print_ast(expr_h_result.node, 0);

                    rel_rule_h_result.node.complete = true;

                    return create_parse_result(true, rel_rule_h_result.node, idx);
                }


                // printf("expr h current tree: \n");
                // print_ast(node, 0);
                return create_parse_result(true, node, idx);
            }

            free_ast(add_rule_res.node);
            return PARSE_FAILED;

        }
        return PARSE_FAILED;
    }

    return create_parse_result(true, null(ASTNode), idx);
}

ParseResult parse_rel_rule(int idx) {
    ParseResult add_rule_res = parse_add_rule(idx);
    
    if (add_rule_res.success) {
        idx = add_rule_res.endpos;
        ParseResult rel_rule_h_res = parse_rel_rule_h(idx);
        if (rel_rule_h_res.success) {
            idx = rel_rule_h_res.endpos;
            if (is_null_ast(rel_rule_h_res.node)) {
                return create_parse_result(true, add_rule_res.node, idx);
            } else {
                ASTNode leaf = rel_rule_h_res.node;
                while (!leaf.children[0].complete) {
                    leaf = leaf.children[0];
                }
                array_insert(leaf.children, add_rule_res.node, 0);
                rel_rule_h_res.node.complete = true;
                return create_parse_result(true, rel_rule_h_res.node, idx);
            }
        }

        free_ast(add_rule_res.node);
        return PARSE_FAILED;
    }

    return PARSE_FAILED;
}

ParseResult parse_and_rule_h(int idx) {
    if (get_token(idx).type == OP_AND) {
        int op_idx = idx;
        idx += 1;
        ParseResult rel_rule_res = parse_rel_rule(idx);
        if (rel_rule_res.success) {
            idx = rel_rule_res.endpos;
            ParseResult and_rule_h_res = parse_and_rule_h(idx);
            
            if (and_rule_h_res.success) {
                idx = and_rule_h_res.endpos;
                ASTNode node = create_ast_node(get_token(op_idx), false);
                
                array_append(node.children, rel_rule_res.node);

                

                if (!is_null_ast(and_rule_h_res.node)) {
                    ASTNode leaf = and_rule_h_res.node;
                    while (!leaf.children[0].complete) {
                        leaf = leaf.children[0];
                    }
                    array_insert(leaf.children, node, 0);
                    
                    // printf("expr h current tree: \n");
                    // print_ast(expr_h_result.node, 0);

                    and_rule_h_res.node.complete = true;

                    return create_parse_result(true, and_rule_h_res.node, idx);
                }


                // printf("expr h current tree: \n");
                // print_ast(node, 0);
                return create_parse_result(true, node, idx);
            }

            free_ast(rel_rule_res.node);
            return PARSE_FAILED;

        }
        return PARSE_FAILED;
    }

    return create_parse_result(true, null(ASTNode), idx);
}

ParseResult parse_and_rule(int idx) {
    ParseResult rel_rule_res = parse_rel_rule(idx);
    
    if (rel_rule_res.success) {
        idx = rel_rule_res.endpos;
        ParseResult and_rule_h_res = parse_and_rule_h(idx);
        if (and_rule_h_res.success) {
            idx = and_rule_h_res.endpos;
            if (is_null_ast(and_rule_h_res.node)) {
                return create_parse_result(true, rel_rule_res.node, idx);
            } else {
                ASTNode leaf = and_rule_h_res.node;
                while (!leaf.children[0].complete) {
                    leaf = leaf.children[0];
                }
                array_insert(leaf.children, rel_rule_res.node, 0);
                and_rule_h_res.node.complete = true;
                return create_parse_result(true, and_rule_h_res.node, idx);
            }
        }

        free_ast(rel_rule_res.node);
        return PARSE_FAILED;
    }

    return PARSE_FAILED;
}

ParseResult parse_expr_h(int idx) {
    if (get_token(idx).type == OP_OR) {
        int op_idx = idx;
        idx += 1;
        ParseResult and_rule_res = parse_and_rule(idx);
        if (and_rule_res.success) {
            idx = and_rule_res.endpos;
            ParseResult expr_h_res = parse_expr_h(idx);
            
            if (expr_h_res.success) {
                idx = expr_h_res.endpos;
                ASTNode node = create_ast_node(get_token(op_idx), false);
                
                array_append(node.children, and_rule_res.node);

                

                if (!is_null_ast(expr_h_res.node)) {
                    ASTNode leaf = expr_h_res.node;
                    while (!leaf.children[0].complete) {
                        leaf = leaf.children[0];
                    }
                    array_insert(leaf.children, node, 0);
                    
                    // printf("expr h current tree: \n");
                    // print_ast(expr_h_result.node, 0);

                    expr_h_res.node.complete = true;

                    return create_parse_result(true, expr_h_res.node, idx);
                }


                // printf("expr h current tree: \n");
                // print_ast(node, 0);
                return create_parse_result(true, node, idx);
            }

            free_ast(and_rule_res.node);
            return PARSE_FAILED;

        }
        return PARSE_FAILED;
    }

    return create_parse_result(true, null(ASTNode), idx);
}

ParseResult parse_expr(int idx) {
    ParseResult and_rule_res = parse_and_rule(idx);
    
    if (and_rule_res.success) {
        idx = and_rule_res.endpos;
        ParseResult expr_h_res = parse_expr_h(idx);
        if (expr_h_res.success) {
            idx = expr_h_res.endpos;
            if (is_null_ast(expr_h_res.node)) {
                return create_parse_result(true, and_rule_res.node, idx);
            } else {
                ASTNode leaf = expr_h_res.node;
                while (!leaf.children[0].complete) {
                    leaf = leaf.children[0];
                }
                array_insert(leaf.children, and_rule_res.node, 0);
                expr_h_res.node.complete = true;
                return create_parse_result(true, expr_h_res.node, idx);
            }
        }

        free_ast(and_rule_res.node);
        return PARSE_FAILED;
    }

    return PARSE_FAILED;
}

#define check_keyword(token, keyword_literal) (token.type == KEYWORD && String_equal(token.text, StringRef(keyword_literal)))


ParseResult parse_stmt(int idx);

ParseResult parse_if_stmt(int idx) {

    if (!check_keyword(get_token(idx), "if")) {
        return PARSE_FAILED;
    }
    
    idx += 1;

    if (get_token(idx).type != LPAREN) {
        return PARSE_FAILED;
    }

    idx += 1;

    ParseResult expr_res = parse_expr(idx);

    if (!expr_res.success) {
        return PARSE_FAILED;
    }
    
    idx = expr_res.endpos;

    if (get_token(idx).type != RPAREN) {
        free_ast(expr_res.node);
        return PARSE_FAILED;
    }

    idx += 1;

    ParseResult stmt_res = parse_stmt(idx);

    if (!stmt_res.success) {
        free_ast(expr_res.node);
        return PARSE_FAILED;
    }

    
    idx = stmt_res.endpos;

    if (check_keyword(get_token(idx), "else")) {
        idx += 1;

        ParseResult stmt2_res = parse_stmt(idx);

        if (stmt2_res.success) {
            idx = stmt2_res.endpos;

            ASTNode node = create_ast_node((Token){.type = IF_ELSE_STMT}, true);

            array_append(node.children, expr_res.node);
            array_append(node.children, stmt_res.node);
            array_append(node.children, stmt2_res.node);

            return create_parse_result(true, node, idx);
        }

        free_ast(expr_res.node);
        free_ast(stmt_res.node);
        return PARSE_FAILED;

    } else {
        ASTNode node = create_ast_node((Token){.type = IF_STMT}, true);
        array_append(node.children, expr_res.node);
        array_append(node.children, stmt_res.node);

        return create_parse_result(true, node, idx);
    }
}

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

ParseResult parse_stmt(int idx) {

    int start_idx = idx;

    ParseResult expr_res = parse_expr(idx);
    if (expr_res.success) {

        idx = expr_res.endpos;

        if (get_token(idx).type == STMT_END) {
            idx += 1;

            return create_parse_result(true, expr_res.node, idx);
        }

        free_ast(expr_res.node);
        idx = start_idx;
        
    }

    ParseResult if_stmt_res = parse_if_stmt(idx);
    if (if_stmt_res.success) {
        return create_parse_result(true, if_stmt_res.node, if_stmt_res.endpos);
    }

    ParseResult block_res = parse_block(idx);
    if (block_res.success) {
        return create_parse_result(true, block_res.node, block_res.endpos);
    }

    ParseResult print_res = parse_print_stmt(idx);
    if (print_res.success) {
        return create_parse_result(true, print_res.node, print_res.endpos);
    }

    ParseResult while_res = parse_while_stmt(idx);
    if (while_res.success) {
        return create_parse_result(true, while_res.node, while_res.endpos);
    }

    ParseResult vardecl_res = parse_vardecl_stmt(idx);
    if (vardecl_res.success) {
        return create_parse_result(true, vardecl_res.node, vardecl_res.endpos);
    }

    ParseResult assign_res = parse_assign_stmt(idx);
    if (assign_res.success) {
        return create_parse_result(true, assign_res.node, assign_res.endpos);
    }

    ParseResult modify_res = parse_modify_stmt(idx);
    if (modify_res.success) {
        return create_parse_result(true, modify_res.node, modify_res.endpos);
    }

    ParseResult vardecl_assign_res = parse_vardecl_assign_stmt(idx);
    if (vardecl_assign_res.success) {
        return create_parse_result(true, vardecl_assign_res.node, vardecl_assign_res.endpos);
    }

    ParseResult input_res = parse_input_stmt(idx);
    if (input_res.success) { 
        return create_parse_result(true, input_res.node, input_res.endpos);
    }

    ParseResult funcdecl_res = parse_func_decl_stmt(idx);
    if (funcdecl_res.success) {
        return create_parse_result(true, funcdecl_res.node, funcdecl_res.endpos);
    }

    ParseResult return_res = parse_return_stmt(idx);
    if (return_res.success) {
        return create_parse_result(true, return_res.node, return_res.endpos);
    }

    ParseResult defer_res = parse_defer_stmt(idx);
    if (defer_res.success) {
        return create_parse_result(true, defer_res.node, defer_res.endpos);
    }

    return PARSE_FAILED;
}

ParseResult parse_stmt_seq(int idx) {
    ParseResult stmt_res = parse_stmt(idx);

    if (!stmt_res.success) {
        return PARSE_FAILED;
    }

    ASTNode node = create_ast_node((Token){.type = STMT_SEQ}, true);

    do {
        idx = stmt_res.endpos;

        array_append(node.children, stmt_res.node);

        stmt_res = parse_stmt(idx);
    } while(stmt_res.success);


    return create_parse_result(true, node, idx);
}

ParseResult parse_block(int idx) {

    if (get_token(idx).type != LCURLY) {
        return PARSE_FAILED;
    }

    idx += 1;

    ParseResult stmt_seq_res = parse_stmt_seq(idx);

    if (!stmt_seq_res.success) {
        return PARSE_FAILED;
    }

    idx = stmt_seq_res.endpos;

    if (get_token(idx).type != RCURLY) {
        free_ast(stmt_seq_res.node);
        return PARSE_FAILED;
    }

    idx += 1;

    stmt_seq_res.node.token.type = BLOCK;
    return create_parse_result(true, stmt_seq_res.node, idx);
}

ParseResult parse_val_seq(int idx) {

    ParseResult expr_res = parse_expr(idx);

    if (!expr_res.success) {
        return PARSE_FAILED;
    }

    ASTNode node = create_ast_node((Token){.type = VAL_SEQ}, true);

    do {

        
        idx = expr_res.endpos;
        
        array_append(node.children, expr_res.node);
        
        if (get_token(idx).type != COMMA) {
            return create_parse_result(true, node, idx);
        }
        
        idx += 1;
        
        expr_res = parse_expr(idx);
    } while(expr_res.success);
    
    
    return create_parse_result(true, node, idx);
}

ParseResult parse_print_stmt(int idx) {

    if (!check_keyword(get_token(idx), "print")) {
        return PARSE_FAILED;
    }

    idx += 1;

    ParseResult val_seq = parse_val_seq(idx);

    if (!val_seq.success) {
        printf("couldnt handle the vals!! \n");
        return PARSE_FAILED;
    }

    idx = val_seq.endpos;

    if (get_token(idx).type != STMT_END) {
        print_err("forgor semi colon on print?");
        return PARSE_FAILED;
    }

    idx += 1;

    val_seq.node.token.type = PRINT_STMT;

    return create_parse_result(true, val_seq.node, idx);
}

ParseResult parse_while_stmt(int idx) {

    if (!check_keyword(get_token(idx), "while")) {
        return PARSE_FAILED;
    }

    idx += 1;


    if (get_token(idx).type != LPAREN) {
        return PARSE_FAILED;
    }

    idx += 1;


    ParseResult expr_res = parse_expr(idx);

    if (!expr_res.success) {
        return PARSE_FAILED;
    }

    idx = expr_res.endpos;

    if (get_token(idx).type != RPAREN) {
        free_ast(expr_res.node);
        return PARSE_FAILED;
    }

    idx += 1;

    ParseResult stmt_res = parse_stmt(idx);

    if (!stmt_res.success) {
        free_ast(expr_res.node);
        return PARSE_FAILED;
    }

    idx = stmt_res.endpos;

    ASTNode node = create_ast_node((Token){.type =  WHILE_STMT}, true);

    array_append(node.children, expr_res.node);

    array_append(node.children, stmt_res.node);

    return create_parse_result(true, node, idx);
}

ParseResult parse_vardecl_stmt(int idx) {

    if (get_token(idx).type != TYPE) {
        return PARSE_FAILED;
    }

    int type_idx = idx;

    idx += 1;

    if (get_token(idx).type != NAME) {
        return PARSE_FAILED;
    }

    int name_idx = idx;

    idx += 1;

    if (get_token(idx).type != STMT_END) {
        return PARSE_FAILED;
    }

    idx += 1;

    ASTNode node = create_ast_node((Token){.type = DECL_STMT}, true);

    array_append(node.children, create_ast_node(get_token(type_idx), true));

    array_append(node.children, create_ast_node(get_token(name_idx), true));


    return create_parse_result(true, node, idx);
}

#define check_semicolon() do { \
    if (get_token(idx).type != STMT_END) { \
        print_err("forgot semicolon!"); \
        return PARSE_FAILED; \
    } \
} while (0)

ParseResult parse_assign_stmt(int idx) {
    
    if (get_token(idx).type != NAME) {
        return PARSE_FAILED;
    }
    
    int name_idx = idx;
    
    idx += 1;
    
    if (get_token(idx).type != OP_ASSIGN) {
        return PARSE_FAILED;
    }
    
    idx += 1;
    
    ParseResult expr_res = parse_expr(idx);
    
    if (!expr_res.success) {
        return PARSE_FAILED;
    }
    
    idx = expr_res.endpos;
    
    
    if (get_token(idx).type != STMT_END) {
        free_ast(expr_res.node);
        print_err("forgot semicolon!");
        return PARSE_FAILED;
    }
    
    idx += 1;
    
    ASTNode node = create_ast_node((Token){.type = ASSIGN_STMT}, true);
    
    array_append(node.children, create_ast_node(get_token(name_idx), true));
    
    array_append(node.children, expr_res.node);
    
    return create_parse_result(true, node, idx);
}

ParseResult parse_vardecl_assign_stmt(int idx) {
    if (get_token(idx).type != TYPE) {
        return PARSE_FAILED;
    }
    
    
    
    int type_idx = idx;
    
    idx += 1;
    
    if (get_token(idx).type != NAME) {
        return PARSE_FAILED;
    }
    
    int name_idx = idx;
    
    idx += 1;
    
    if (get_token(idx).type != OP_ASSIGN) {
        return PARSE_FAILED;
    }
    
    idx += 1;
    
    ParseResult expr_res = parse_expr(idx);
    
    if (!expr_res.success) {
        return PARSE_FAILED;
    }
    
    idx = expr_res.endpos;
    
    
    if (get_token(idx).type != STMT_END) {
        free_ast(expr_res.node);
        print_err("forgot semicolon!");
        return PARSE_FAILED;
    }
    
    idx += 1;
    
    ASTNode node = create_ast_node((Token){.type = DECL_ASSIGN_STMT}, true);
    
    array_append(node.children, create_ast_node(get_token(type_idx), true));
    
    array_append(node.children, create_ast_node(get_token(name_idx), true));
    
    array_append(node.children, expr_res.node);
    
    return create_parse_result(true, node, idx);
}

ParseResult parse_input_stmt(int idx) {

    if (!check_keyword(get_token(idx), "input")) return PARSE_FAILED;

    idx += 1;

    if (get_token(idx).type != NAME) return PARSE_FAILED;

    Token var_token = get_token(idx);

    idx += 1;

    if (get_token(idx).type != STMT_END) return PARSE_FAILED;

    idx += 1;

    ASTNode node = create_ast_node((Token){.type = INPUT_STMT}, true);

    array_append(node.children, create_ast_node(var_token, true));

    return create_parse_result(true, node, idx);
}

ParseResult parse_func_arg(int idx) {
    if (get_token(idx).type != TYPE) return PARSE_FAILED;
    int type_idx = idx;
    idx += 1;
    if (get_token(idx).type != NAME) return PARSE_FAILED;
    int name_idx = idx;
    idx += 1;
    ASTNode node = create_ast_node((Token){.type = FUNC_ARG}, true);
    array_append(node.children, create_ast_node(get_token(type_idx), true));
    array_append(node.children, create_ast_node(get_token(name_idx), true));

    return create_parse_result(true, node, idx);
}

ParseResult parse_func_args_seq(int idx) {
    
    printf("parsing func args from idx: %d \n", idx);

    if (get_token(idx).type == COMMA) {
        idx += 1;
        ParseResult func_arg_res = parse_func_arg(idx);
        if (!func_arg_res.success) return PARSE_FAILED;
        idx = func_arg_res.endpos;

        ParseResult func_args_res = parse_func_args_seq(idx);
        if (!func_args_res.success) {
            free_ast(func_arg_res.node);
            return PARSE_FAILED;
        }
        idx = func_args_res.endpos;

        ASTNode node = create_ast_node((Token){.type = FUNC_ARGS_SEQ}, true);
        
        array_append(node.children, func_arg_res.node);
        if (!is_null_ast(func_args_res.node)) {
            for (int i = 0; i < array_length(func_args_res.node.children); i++) {
                array_append(node.children, func_args_res.node.children[i]);
            }
            array_free(func_args_res.node.children); // cant call free_ast because its recursive and we only want to free this one
            func_args_res.node.children = NULL;
        }

        return create_parse_result(true, node, idx);
    }

    ParseResult func_arg_res = parse_func_arg(idx);
    if (!func_arg_res.success) return create_parse_result(true, null(ASTNode), idx);
    idx = func_arg_res.endpos;

    ParseResult func_args_res = parse_func_args_seq(idx);
    if (!func_args_res.success) {
        free_ast(func_arg_res.node);
        return create_parse_result(true, null(ASTNode), idx);
    }
    idx = func_args_res.endpos;

    ASTNode node = create_ast_node((Token){.type = FUNC_ARGS_SEQ}, true);
    
    array_append(node.children, func_arg_res.node);
    if (!is_null_ast(func_args_res.node)) {
        for (int i = 0; i < array_length(func_args_res.node.children); i++) {
            array_append(node.children, func_args_res.node.children[i]);
        }
        array_free(func_args_res.node.children); // cant call free_ast because its recursive and we only want to free this one
        func_args_res.node.children = NULL;
    }

    return create_parse_result(true, node, idx);

}

ParseResult parse_func_decl_stmt(int idx) {

    if (get_token(idx++).type != TYPE) return PARSE_FAILED;
    int type_idx = idx - 1;
    if (get_token(idx++).type != NAME) return PARSE_FAILED;
    int name_idx = idx - 1;
    if (get_token(idx++).type != LPAREN) return PARSE_FAILED;

    ParseResult func_args_res = parse_func_args_seq(idx);
    if (!func_args_res.success) return PARSE_FAILED;
    idx = func_args_res.endpos;

    if (get_token(idx++).type != RPAREN) {
        if (!is_null_ast(func_args_res.node)) free_ast(func_args_res.node);
        return PARSE_FAILED;
    }

    ParseResult stmt_res = parse_stmt(idx);

    if (!stmt_res.success) {
        if (!is_null_ast(func_args_res.node)) free_ast(func_args_res.node);
        return PARSE_FAILED;
    }

    idx = stmt_res.endpos;

    // turn one liners into blocks so i can gurantee every function has a block (to make dealing with argument lifetimes easier)
    if (stmt_res.node.token.type != BLOCK) {
        ASTNode block = create_ast_node((Token){.type = BLOCK}, true);
        array_append(block.children, stmt_res.node);
        stmt_res.node = block;
    }

    ASTNode node = create_ast_node((Token){.type = FUNC_DECL_STMT}, true);

    array_append(node.children, create_ast_node(get_token(type_idx), true));

    array_append(node.children, create_ast_node(get_token(name_idx), true));

    if (!is_null_ast(func_args_res.node)) {
        array_append(node.children, func_args_res.node);
    } else {
        // an empty one
        array_append(node.children, create_ast_node((Token){.type = FUNC_ARGS_SEQ}, true));
    }

    array_append(node.children, stmt_res.node);


    return create_parse_result(true, node, idx);

}

ParseResult parse_func_call(int idx) {

    if (get_token(idx).type != NAME) return PARSE_FAILED;
    int name_idx = idx;
    idx += 1;

    if (get_token(idx).type != LPAREN) return PARSE_FAILED;
    idx += 1;

    ParseResult val_seq_res = parse_val_seq(idx);

    if (!val_seq_res.success) {
        if (get_token(idx).type == RPAREN) {
            idx += 1;
            ASTNode node = create_ast_node((Token){.type = FUNC_CALL}, true);
            array_append(node.children, create_ast_node(get_token(name_idx), true));
            array_append(node.children, create_ast_node((Token){.type = VAL_SEQ}, true));
            return create_parse_result(true, node, idx);
        }
        return PARSE_FAILED;
    }
    idx = val_seq_res.endpos;

    if (get_token(idx).type != RPAREN) {
        free_ast(val_seq_res.node);
        return PARSE_FAILED;
    }
    idx += 1;

    ASTNode node = create_ast_node((Token){.type = FUNC_CALL}, true);
    array_append(node.children, create_ast_node(get_token(name_idx), true));
    array_append(node.children, val_seq_res.node);

    return create_parse_result(true, node, idx);
}

ParseResult parse_return_stmt(int idx) {
    if (!check_keyword(get_token(idx), "return")) return PARSE_FAILED;

    idx += 1;

    ParseResult expr_res = parse_expr(idx);
    if (!expr_res.success) {
        if (get_token(idx).type == STMT_END) {
            idx += 1;
            return create_parse_result(true, create_ast_node((Token){.type = RETURN_STMT}, true), idx);
        }
        return PARSE_FAILED;
    }

    idx = expr_res.endpos;

    if (get_token(idx).type != STMT_END) {
        free_ast(expr_res.node);
        return PARSE_FAILED;
    }
    idx += 1;

    ASTNode node = create_ast_node((Token){.type = RETURN_STMT}, true);


    array_append(node.children, expr_res.node);

    return create_parse_result(true, node, idx);
}

ParseResult parse_modify_stmt(int idx) {
    if (get_token(idx).type != NAME) return PARSE_FAILED;
    int name_idx = idx;
    idx += 1;
    if (!in_range(get_token(idx).type, MODIFY_TOKENS_START, MODIFY_TOKENS_END)) return PARSE_FAILED;
    int op_idx = idx;
    idx += 1;

    ParseResult expr_res = parse_expr(idx);

    if (!expr_res.success) return PARSE_FAILED;
    idx = expr_res.endpos;

    if (get_token(idx).type != STMT_END) return PARSE_FAILED;
    idx += 1;


    ASTNode node = create_ast_node((Token){.type = ASSIGN_STMT}, true);

    array_append(node.children, create_ast_node(get_token(name_idx), true));
    TokenType op_type;
    switch (get_token(op_idx).type) {
        case OP_ASSIGN_ADD:
            op_type = OP_ADD;
            break;
        case OP_ASSIGN_SUB:
            op_type = OP_SUB;
            break;
        case OP_ASSIGN_MUL:
            op_type = OP_MUL;
            break;
        case OP_ASSIGN_DIV:
            op_type = OP_DIV;
            break;
        case OP_ASSIGN_MOD:
            op_type = OP_MOD;
            break;
        default:
            print_err("LITERALLY CAN'T HAPPEN. KYS.");
            break;
    }

    ASTNode op_node = create_ast_node((Token){.type = op_type}, true);
    array_append(op_node.children, create_ast_node(get_token(name_idx), true));
    array_append(op_node.children, expr_res.node);
    array_append(node.children, op_node);
    return create_parse_result(true, node, idx);

}

ParseResult parse_defer_stmt(int idx) {
    if (!check_keyword(get_token(idx), "defer")) return PARSE_FAILED;

    idx += 1;

    ParseResult stmt_res = parse_stmt(idx);

    if (!stmt_res.success) return PARSE_FAILED;

    idx = stmt_res.endpos;


    ASTNode node = create_ast_node((Token){.type = DEFER_STMT}, true);
    array_append(node.children, stmt_res.node);

    return create_parse_result(true, node, idx);
}



/*
Rules:
<if-stmt> -> if ( <expr> ) <stmt> | if ( <expr> ) <stmt> else <stmt>

<while-stmt> -> while ( <expr> ) <stmt>
<declare-and-assign-stmt> -> <typename> <name> = <expr>;
<declare-stmt> -> <typename> <name>;

<func-decl> -> <typename> <name>(<func-args>) <stmt>

<func-arg> -> <typename> <name>
<func-args> -> <func-arg> <func-args> | , <func-arg> <func-args> | epsilon 

<func-call> -> <name>(<val-seq>);

<typename> -> one of a list of allowed types
<name> -> sequence of characters which is NOT defined as a variable, doesnt start with [0-9], allowed characters: [a-z][A-Z]_[0-9]
<assign-stmt> -> <variable> = <expr>;

<stmt> -> <if-stmt> | <while-stmt> | ... | <block>
<block> -> { <stmt-seq> }
<stmt-seq> -> <stmt> | <stmt> <stmt-seq>

<val-seq> -> <expr> | <expr>, <val-seq>

<print-stmt> -> print <val-seq>;

<input-stmt> -> input <name>;

<expr> -> <and_rule> <'expr>
<'expr> -> || <and_rule> <'expr> | epsilon
<and_rule> -> <rel_rule> <'and_rule>
<'and_rule> -> && <rel_rule> <'and_rule> | epsilon
<rel_rule> -> <add_rule> <'rel_rule>
<'rel_rule> -> (== | != | > | ...) <add_rule> <'rel_rule> | epsilon
<add_rule> -> <mul_rule> <'add_rule>
<'add_rule> -> + <mul_rule> <'add_rule> | - <mul_rule> <'add_rule> | epsilon
<mul_rule> -> <base_rule> <'mul_rule>
<'mul_rule> -> * <base_rule> <'mul_rule> | / <base_rule> <'mul_rule> | epsilon
<base_rule> -> !<base_rule> | <value> | ( <expr> )
<value> -> <bool> | <literal> | <variable> | <int> | <float> | <func-call>
<bool> -> true | false



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
int get_type_precedence(VarType type) {

    if (type == T_BOOL) return 0;
    if (type == T_INT) return 1;
    if (type == T_FLOAT) return 2;
    if (type == T_STRING) return 3;
    if (type == T_STRUCT) return 4;

    return -1;
}

// anything this function doesn't touch is meant to return void
void typeify_tree(ASTNode *node, HashMap *var_map) {
    
    if (node->token.type == BLOCK) {
        var_map = HashMap_copy(var_map); // make further use be limited to this scope
    }
    
    if (node->token.type == DECL_STMT || node->token.type == DECL_ASSIGN_STMT) {
        String var_name = node->children[1].token.text;
        VarType var_type = node->children[0].token.var_type;
        
        HashMap_put(var_map, var_name, (int)var_type);
    }
    
    if (node->token.type == INTEGER) node->expected_return_type = T_INT;
    if (node->token.type == FLOAT) node->expected_return_type = T_FLOAT;
    if (node->token.type == STRING_LITERAL) node->expected_return_type = T_STRING;
    if (node->token.type == BOOL) node->expected_return_type = T_BOOL;
    if (node->token.type == NAME) {
        node->expected_return_type = (int)HashMap_get(var_map, node->token.text);
    }
    if (node->token.type == FUNC_CALL) {
        node->expected_return_type = (int)HashMap_get(var_map, node->children[0].token.text);
    }
            
    if (in_range(node->token.type, ARITHOPS_START, ARITHOPS_END)) {
        int highest_precedence_type = T_VOID;
        int len = array_length(node->children);
        for (int i = 0; i < len; i++) {
            typeify_tree(&node->children[i], var_map);
            if (get_type_precedence(node->children[i].expected_return_type) > get_type_precedence(highest_precedence_type)) {
                highest_precedence_type = node->children[i].expected_return_type;
            }
        }
        node->expected_return_type = highest_precedence_type;
    } else if (in_range(node->token.type, BOOLOPS_START, BOOLOPS_END)) {

        node->expected_return_type = T_BOOL;

        int len = array_length(node->children);
        for (int i = 0; i < len; i++) {
            typeify_tree(&node->children[i], var_map);
        }
    } else if (node->token.type == FUNC_DECL_STMT) {

        printf("putting func %s of type %s \n", node->children[1].token.text.data, var_type_names[node->children[0].token.var_type]);

        HashMap_put(var_map, node->children[1].token.text, (int)node->children[0].token.var_type);
        
        var_map = HashMap_copy(var_map);

        HashMap_print(var_map);

        printf("copied hashmap \n");

        ASTNode func_args = node->children[2];

        // print_ast(func_args->children[0].children[1], 0);
        int len = array_length(func_args.children);
        for (int i = 0; i < len; i++) {

            StringRef var_name = func_args.children[i].children[1].token.text;

            VarType var_type = func_args.children[i].children[0].token.var_type;

            HashMap_put(var_map, var_name, (int)var_type);
        }

        ASTNode *func_scope = &node->children[3];

        // this ensures we don't copy the hashmap twice because of the scope
        for (int i = 0; i < array_length(func_scope->children); i++) {
            typeify_tree(&func_scope->children[i], var_map);
        }

        HashMap_free(var_map);


    } else {
        int len = array_length(node->children);
        for (int i = 0; i < len; i++) {
            typeify_tree(&node->children[i], var_map);
        }
    } 
    
    
    
    
    
    if (node->token.type == BLOCK) {
        HashMap_free(var_map);
    }
    
}

void typeify_tree_wrapper(ASTNode *node) {
    HashMap *var_map = HashMap(int);
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


void preprocess_ast(ASTNode *ast) {
    typeify_tree_wrapper(ast);
    move_defers_to_end(ast);
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
X(I_NOT_EQUAL) \
X(I_NOT_EQUAL_FLOAT) \
X(I_NOT_EQUAL_BOOL) \
X(I_NOT_EQUAL_STR) \
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
X(I_STORE_STACK_PTR) \
X(I_SET_STACK_PTR) \
X(I_STACK_PTR_ADD) \
X(I_RETURN) \
X(I_CALL) \
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

typedef struct Val {
    VarType type;
    union {
        void *any_val;
        char *s_val;
        double f_val;
        int i_val;
        bool b_val;
    };
} Val;

typedef struct Inst {
    InstType type;
    Val arg1;
    Val arg2; 
} Inst;

typedef struct VarHeader { 
    i32 pos;
    i32 type;
    bool is_func;
}VarHeader;

Inst create_inst(VarType type, Val arg1, Val arg2) {
    return (Inst){.type = type, .arg1 = arg1, .arg2 = arg2};
}

// - 1 + 1
// 1 + - 1
// 1 - 1


void print_val(Val val) {
    printf("(%s, ", var_type_names[val.type]);
    switch (val.type) {
        case T_INT:
            printf("%d", val.i_val);
            break;
        case T_FLOAT:
            printf("%.2f", val.f_val);
            break;
        case T_BOOL:
            printf("%s", val.b_val ? "true" : "false");
            break;
        case T_STRING:
            printf("\"%s\"", val.s_val);
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
            print_val(inst.arg1);
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

static inline int get_vartype_size(VarType t) {
    switch (t) {
        case T_INT:
            return 4;
            break;
        case T_FLOAT:
            return 8;
            break;
        case T_BOOL:
            return 1;
            break;
        case T_STRING:
            return 8;
            break;
        default:
            print_err("i dunno the size!");
            return -1;
            break;
    }
}

InstType get_cvt_inst_type_for_types(VarType from, VarType to) {
    // avert your eyes
    if (from == T_BOOL) {
        if (to == T_INT) return I_CONVERT_BOOL_INT;
        if (to == T_FLOAT) return I_CONVERT_BOOL_FLOAT;
        if (to == T_STRING) return I_CONVERT_BOOL_STR;
    }
    if (from == T_INT) {
        if (to == T_BOOL) return I_CONVERT_INT_BOOL;
        if (to == T_FLOAT) return I_CONVERT_INT_FLOAT;
        if (to == T_STRING) return I_CONVERT_INT_STR;
    }
    if (from == T_FLOAT) {
        if (to == T_BOOL) return I_CONVERT_FLOAT_BOOL;
        if (to == T_INT) return I_CONVERT_FLOAT_INT;
        if (to == T_STRING) return I_CONVERT_FLOAT_STR;
    }
    if (from == T_STRING) {
        if (to == T_BOOL) return I_CONVERT_STR_BOOL;
        if (to == T_INT) return I_CONVERT_STR_INT;
        if (to == T_FLOAT) return I_CONVERT_STR_FLOAT;
    }

    return I_INVALID;
}

InstType get_inst_type_for_op(TokenType op, VarType var_type) {
    if (op == OP_ADD) {
        if (var_type == T_INT) return I_ADD;
        if (var_type == T_FLOAT) return I_ADD_FLOAT;
    }
    if (op == OP_SUB) {
        if (var_type == T_INT) return I_SUB;
        if (var_type == T_FLOAT) return I_SUB_FLOAT;
    }
    if (op == OP_MUL) {
        if (var_type == T_INT) return I_MUL;
        if (var_type == T_FLOAT) return I_MUL_FLOAT;
    }
    if (op == OP_DIV) {
        if (var_type == T_INT) return I_DIV;
        if (var_type == T_FLOAT) return I_DIV_FLOAT;
    }
    if (op == OP_MOD) {
        if (var_type == T_INT) return I_MOD;
        if (var_type == T_FLOAT) return I_MOD_FLOAT;
    }
    if (op == OP_GREATER) {
        if (var_type == T_INT) return I_GREATER;
        if (var_type == T_FLOAT) return I_GREATER_FLOAT;
    }
    if (op == OP_GREATEREQ) {
        if (var_type == T_INT) return I_GREATER_EQUAL;
        if (var_type == T_FLOAT) return I_GREATER_EQUAL_FLOAT;
    }
    if (op == OP_LESS) {
        if (var_type == T_INT) return I_LESS;
        if (var_type == T_FLOAT) return I_LESS_FLOAT;
    }
    if (op == OP_LESSEQ) {
        if (var_type == T_INT) return I_LESS_EQUAL;
        if (var_type == T_FLOAT) return I_LESS_EQUAL_FLOAT;
    }
    if (op == OP_EQ) {
        if (var_type == T_INT) return I_EQUAL;
        if (var_type == T_FLOAT) return I_EQUAL_FLOAT;
        if (var_type == T_BOOL) return I_EQUAL_BOOL;
        if (var_type == T_STRING) return I_EQUAL_STR;
    }
    if (op == OP_NOTEQ) {
        if (var_type == T_INT) return I_NOT_EQUAL;
        if (var_type == T_FLOAT) return I_NOT_EQUAL_FLOAT;
        if (var_type == T_BOOL) return I_NOT_EQUAL_BOOL;
        if (var_type == T_STRING) return I_NOT_EQUAL_STR;
    }
    if (op == OP_AND && var_type == T_BOOL) {
        return I_AND;
    }
    if (op == OP_OR && var_type == T_BOOL) {
        return I_OR;
    }

    return I_INVALID;
}

int gi_stack_pos = 0;
int gi_label_idx = 0;



VarHeader *get_varheader_from_map_list(LinkedList *var_map_list, String name, bool *global) {
    if (global != NULL) *global = false;
    LLNode *current = var_map_list->head;
    while (current != NULL) {
        VarHeader *vh = HashMap_get_safe(current->val, name, NULL);
        if (vh != NULL) {
            if (current->next == NULL && global != NULL) *global = true;
            return vh;
        }
        current = current->next;
    }
    print_err("Identifier '%s' doesn't exist within the current scope!", name.data);
    exit(1);
    return NULL;
}

void add_varheader_to_map_list(LinkedList *var_map_list, String key, VarHeader *vh) {
    HashMap *map = var_map_list->head->val;
    HashMap_put(map, key, vh);
}

int calc_stack_space_for_scope(ASTNode ast) {
    if (ast.token.type == DECL_ASSIGN_STMT || ast.token.type == DECL_STMT) return get_vartype_size(ast.children[0].token.var_type);
    if (ast.token.type == FUNC_DECL_STMT) return 0; // because thats a seperate scope

    int sum = 0;
    int len = array_length(ast.children);
    for (int i = 0; i < len; i++) {
        sum += calc_stack_space_for_scope(ast.children[i]);
    }

    return sum;
}

void generate_instructions_for_node(ASTNode ast, Inst **instructions, LinkedList *var_map_list);

void generate_instructions_for_vardecl(ASTNode ast, Inst **instructions, LinkedList *var_map_list) {
    if (ast.children[0].token.type != TYPE) {
        print_err("Invalid variable declaration!");
        exit(1);
    }
    
    String var_name = ast.children[1].token.text;

    VarType var_type = ast.children[0].token.var_type;
    
    VarHeader vh = (VarHeader){.pos = gi_stack_pos, .type = var_type};

    add_varheader_to_map_list(var_map_list, var_name, &vh);
    
    int size = get_vartype_size(var_type);

    
    if (ast.token.type == DECL_ASSIGN_STMT) {
        generate_instructions_for_node(ast.children[2], instructions, var_map_list);
        
        VarType child_return_type = ast.children[2].expected_return_type;
        
        if (child_return_type != var_type) {
            InstType inst_type = get_cvt_inst_type_for_types(child_return_type, var_type);
            if (inst_type == I_INVALID) {
                print_err("Invalid conversion on variable declaration!");
                printf("Tried to convert from type '%s' to '%s' \n", var_type_names[child_return_type], var_type_names[var_type]);
            } else {
                array_append(*instructions, create_inst(inst_type, null(Val), null(Val)));
            }
        }
    } else {
        Val val = null(Val);
        val.type = var_type;
        array_append(*instructions, create_inst(I_PUSH, val, null(Val)));
    }

    array_append(*instructions, create_inst(I_STACK_STORE, (Val){.type = T_INT, .i_val = get_vartype_size(var_type)}, (Val){.type = T_INT, .i_val = gi_stack_pos}));
    gi_stack_pos += size;
}


void generate_instructions_for_assign(ASTNode ast, Inst **instructions, LinkedList *var_map_list) {
    String var_name = ast.children[0].token.text;

    bool isglobal;

    VarHeader *vh = get_varheader_from_map_list(var_map_list, var_name, &isglobal);

    generate_instructions_for_node(ast.children[1], instructions, var_map_list);
    
    array_append(*instructions, create_inst((isglobal? I_STACK_STORE_GLOBAL : I_STACK_STORE), (Val){.type = T_INT, .i_val = get_vartype_size(vh->type)}, (Val){.type = T_INT, .i_val = vh->pos}));

}


void generate_instructions_for_binop(ASTNode ast, Inst **instructions, LinkedList *var_map_list) {

    int len = array_length(ast.children);

    bool is_bool_op = in_range(ast.token.type, BOOLOPS_START, BOOLOPS_END);

    VarType highest_prec_type = T_VOID;

    if (is_bool_op) {
        for (int i = 0; i < len; i++) {
            if (get_type_precedence(ast.children[i].expected_return_type) > get_type_precedence(highest_prec_type)) {
                highest_prec_type = ast.children[i].expected_return_type;
            }
        }
    }

    VarType goal_type = is_bool_op ? highest_prec_type : ast.expected_return_type;

    for (int i = 0; i < len; i++) {

        generate_instructions_for_node(ast.children[i], instructions, var_map_list);

        if (goal_type != ast.children[i].expected_return_type) {
            InstType inst_type = get_cvt_inst_type_for_types(ast.children[i].expected_return_type, goal_type);
            if (inst_type == I_INVALID) {
                print_err("Invalid conversion!");
                printf("Tried to convert from type '%s' to '%s' \n", var_type_names[ast.children[i].expected_return_type], var_type_names[goal_type]);
            } else {
                array_append(*instructions, create_inst(inst_type, null(Val), null(Val)));
            }
        }
    }

    InstType inst_type = get_inst_type_for_op(ast.token.type, goal_type);
    if (inst_type == I_INVALID) {
        print_err("Invalid operator!");
        printf("Tried to get operator '%s' between '%s' type operands! \n", token_type_names[ast.token.type], var_type_names[goal_type]);
    } else {
        array_append(*instructions, create_inst(inst_type, null(Val), null(Val)));
    }

}

InstType get_print_inst_for_type(VarType type) {
    if (type == T_INT) return I_PRINT_INT;
    if (type == T_BOOL) return I_PRINT_BOOL;
    if (type == T_FLOAT) return I_PRINT_FLOAT;
    if (type == T_STRING) return I_PRINT_STR;

    return I_INVALID;
}

void generate_instructions_for_print(ASTNode ast, Inst **instructions, LinkedList *var_map_list) {

    int len = array_length(ast.children);
    for (int i = 0; i < len; i++) {

        generate_instructions_for_node(ast.children[i], instructions, var_map_list);

        InstType inst_type = get_print_inst_for_type(ast.children[i].expected_return_type);
        if (inst_type == I_INVALID) {
            print_err("Invalid argument for print! (seriously how could you mess this up)");
            printf("argument type: %s \n", var_type_names[ast.children[i].expected_return_type]);
        } else {
            array_append(*instructions, create_inst(inst_type, null(Val), null(Val)));
        }
    }

    array_append(*instructions, create_inst(I_PRINT_NEWLINE, null(Val), null(Val)));
}

void generate_instructions_for_if(ASTNode ast, Inst **instructions, LinkedList *var_map_list) {
    
    int end_label_idx = gi_label_idx++;

    int end_label_true_idx;
    int else_label_true_idx;
    // if (ast.token.type == IF_ELSE_STMT) else_label_idx = gi_label_idx++;

    // condition
    generate_instructions_for_node(ast.children[0], instructions, var_map_list);

    if (ast.children[0].expected_return_type != T_BOOL) {
        InstType inst_type = get_cvt_inst_type_for_types(ast.children[0].expected_return_type, T_BOOL);

        if (inst_type == I_INVALID) {
            print_err("Type is ambigous! Cannot be used as an if condition. (you did badly.)");
            printf("type: %s \n", var_type_names[ast.children[0].expected_return_type]);
        } else {
            array_append(*instructions, create_inst(inst_type, null(Val), null(Val)));
        }
    }
    
    int first_jump_idx = array_length(*instructions);
    array_append(*instructions, create_inst(I_JUMP_NOT, (Val){.type = T_INT, .i_val = -1}, null(Val)));

    // if-body
    generate_instructions_for_node(ast.children[1], instructions, var_map_list);
    
    int if_body_jump_idx = -1;

    if (ast.token.type == IF_ELSE_STMT) {

        if_body_jump_idx = array_length(*instructions);
        array_append(*instructions, create_inst(I_JUMP, (Val){.type = T_INT, .i_val = end_label_idx}, null(Val)));

        else_label_true_idx = array_length(*instructions);
        array_append(*instructions, create_inst(I_LABEL, null(Val), null(Val)));

        // else-body
        generate_instructions_for_node(ast.children[2], instructions, var_map_list);
    }

    end_label_true_idx = array_length(*instructions);
    array_append(*instructions, create_inst(I_LABEL, null(Val), null(Val)));

    if (ast.token.type == IF_STMT) {
        (*instructions)[first_jump_idx].arg1.i_val = end_label_true_idx;
    } else {
        (*instructions)[first_jump_idx].arg1.i_val = else_label_true_idx;
        (*instructions)[if_body_jump_idx].arg1.i_val = end_label_true_idx;
    }

}

void generate_instructions_for_while(ASTNode ast, Inst **instructions, LinkedList *var_map_list) {

    int start_label_idx = array_length(*instructions);

    array_append(*instructions, create_inst(I_LABEL, null(Val), null(Val)));

    generate_instructions_for_node(ast.children[0], instructions, var_map_list);

    if (ast.children[0].expected_return_type != T_BOOL) {
        InstType inst_type = get_cvt_inst_type_for_types(ast.children[0].expected_return_type, T_BOOL);

        if (inst_type == I_INVALID) {
            print_err("Type is ambigous! Cannot be used as an while condition. (you did badly.)");
            printf("type: %s \n", var_type_names[ast.children[0].expected_return_type]);
        } else {
            array_append(*instructions, create_inst(inst_type, null(Val), null(Val)));
        }
    }

    int jump_not_inst_idx = array_length(*instructions);
    array_append(*instructions, create_inst(I_JUMP_NOT, (Val){.type = T_INT, .i_val = -1}, null(Val)));

    // while body
    generate_instructions_for_node(ast.children[1], instructions, var_map_list);

    array_append(*instructions, create_inst(I_JUMP, (Val){.type = T_INT, .i_val = start_label_idx}, null(Val)));

    int end_label_idx = array_length(*instructions);
    array_append(*instructions, create_inst(I_LABEL, null(Val), null(Val)));

    (*instructions)[jump_not_inst_idx].arg1.i_val = end_label_idx;


}

void generate_instructions_for_input(ASTNode ast, Inst **instructions, LinkedList *var_map_list) {
    // {
    //     "hi" : 2,
    //     "ghf", 6
    // }
    
    array_append(*instructions, create_inst(I_INPUT, null(Val), null(Val)));

    VarType goal_type = ast.children[0].expected_return_type;

    if (goal_type != T_STRING) {
        InstType cvt_inst = get_cvt_inst_type_for_types(T_STRING, goal_type);
        if (cvt_inst == I_INVALID) {
            print_err("Can't convert string to this type!");
            exit(1);
        }
        array_append(*instructions, create_inst(cvt_inst, null(Val), null(Val)));
    }

    bool isglobal;

    VarHeader *vh = get_varheader_from_map_list(var_map_list, ast.children[0].token.text, &isglobal);

    array_append(*instructions, create_inst((isglobal? I_STACK_STORE_GLOBAL : I_STACK_STORE), (Val){.type = T_INT, .i_val = get_vartype_size(goal_type)}, (Val){.type = T_INT, .i_val = vh->pos}));

}

void generate_instructions_for_func_decl(ASTNode ast, Inst **instructions, LinkedList *var_map_list) {
    
    array_append(*instructions, create_inst(I_JUMP, (Val){.type = T_INT, .i_val = -1}, null(Val)));
    int jump_inst_idx = array_length(*instructions) - 1;

    array_append(*instructions, create_inst(I_LABEL, null(Val), null(Val)));

    ASTNode var_args = ast.children[2];

    String func_name = ast.children[1].token.text;
    VarHeader vh = {.is_func = true, .pos = array_length(*instructions) - 1};

    add_varheader_to_map_list(var_map_list, func_name, &vh);


    LL_prepend(var_map_list, LLNode_create(HashMap(VarHeader)));
    int prev_gi_stack_pos = gi_stack_pos;
    gi_stack_pos = 0;

    int size = 0;
    ASTNode scope = ast.children[3];
    
    for (int i = array_length(var_args.children) - 1; i >= 0; i--) {
        VarType var_type = var_args.children[i].children[0].token.var_type;
        size += get_vartype_size(var_type);
    }
    size += calc_stack_space_for_scope(scope);
    array_append(*instructions, create_inst(I_STACK_PTR_ADD, (Val){.type = T_INT, .i_val = size}, null(Val)));

    for (int i = array_length(var_args.children) - 1; i >= 0; i--) {
        String var_name = var_args.children[i].children[1].token.text;
        VarType var_type = var_args.children[i].children[0].token.var_type;
        array_append(*instructions, create_inst(I_STACK_STORE, (Val){.type = T_INT, .i_val = get_vartype_size(var_type)}, (Val){.type = T_INT, .i_val = gi_stack_pos}));
        VarHeader vh = (VarHeader){.pos = gi_stack_pos, .type = var_type};
        add_varheader_to_map_list(var_map_list, var_name, &vh);
        gi_stack_pos += get_vartype_size(var_type);
        size += get_vartype_size(var_type);
    }




    int len = array_length(scope.children);

    for (int i = 0; i < len; i++) {
        generate_instructions_for_node(scope.children[i], instructions, var_map_list);
    }
    gi_stack_pos = prev_gi_stack_pos;
    HashMap_free(var_map_list->head->val);
    LL_pop_head(var_map_list);

    array_append(*instructions, create_inst(I_RETURN, null(Val), null(Val)));


    (*instructions)[jump_inst_idx].arg1.i_val = array_length(*instructions);
}

void generate_instructions_for_func_call(ASTNode ast, Inst **instructions, LinkedList *var_map_list) {

    ASTNode func_args = ast.children[1];

    int len = array_length(func_args.children);

    for (int i = 0; i < len; i++) {
        generate_instructions_for_node(func_args.children[i], instructions, var_map_list);
    }

    String func_name = ast.children[0].token.text;

    VarHeader *vh = get_varheader_from_map_list(var_map_list, func_name, NULL);

    array_append(*instructions, create_inst(I_CALL, (Val){.type = T_INT, .i_val = vh->pos}, null(Val)));
}

void generate_instructions_for_node(ASTNode ast, Inst **instructions, LinkedList *var_map_list) {
    
    // independently defined operators
    if (ast.token.type == DECL_ASSIGN_STMT || ast.token.type == DECL_STMT) {
        generate_instructions_for_vardecl(ast, instructions, var_map_list);
        return;
    }
    if (ast.token.type == ASSIGN_STMT) {
        generate_instructions_for_assign(ast, instructions, var_map_list);
        return;
    }
    if (in_range(ast.token.type, BINOPS_START, BINOPS_END)) {
        generate_instructions_for_binop(ast, instructions, var_map_list);
        return;
    }
    if (ast.token.type == PRINT_STMT) {
        generate_instructions_for_print(ast, instructions, var_map_list);
        return;
    }

    if (ast.token.type == IF_STMT || ast.token.type == IF_ELSE_STMT) {
        generate_instructions_for_if(ast, instructions, var_map_list);
        return;
    }

    if (ast.token.type == WHILE_STMT) {
        generate_instructions_for_while(ast, instructions, var_map_list);
        return;
    }

    if (ast.token.type == INPUT_STMT) {
        generate_instructions_for_input(ast, instructions, var_map_list);
        return;
    }

    if (ast.token.type == FUNC_DECL_STMT) {
        generate_instructions_for_func_decl(ast, instructions, var_map_list);
        return;
    }

    if (ast.token.type == FUNC_CALL) {
        generate_instructions_for_func_call(ast, instructions, var_map_list);
        return;
    }

    int temp_stack_ptr;

    // pre children operators
    switch (ast.token.type) {
        case STMT_SEQ: ;
            int size = calc_stack_space_for_scope(ast);
            array_append(*instructions, create_inst(I_STACK_PTR_ADD, (Val){.type = T_INT, .i_val = size}, null(Val)));
            break;
        case BLOCK:
            temp_stack_ptr = gi_stack_pos;
            LL_prepend(var_map_list, LLNode_create(HashMap(VarHeader)));
            // array_append(*instructions, create_inst(I_STORE_STACK_PTR, null(Val)));
            break;
        
        default:
            break;
    }
        
    for (int i = 0; i < array_length(ast.children); i++) {
        generate_instructions_for_node(ast.children[i], instructions, var_map_list);
    }

    Val val;



    // post children operators
    switch (ast.token.type) {
        case RETURN_STMT:
            array_append(*instructions, create_inst(I_RETURN, null(Val), null(Val)));
            break;
        case INTEGER:
            val = (Val){.type = T_INT, .i_val = ast.token.int_val};
            array_append(*instructions, create_inst(I_PUSH, val, null(Val)));
            break;
        case FLOAT:
            val = (Val){.type = T_FLOAT, .f_val = ast.token.double_val};
            array_append(*instructions, create_inst(I_PUSH, val, null(Val)));
            break;
        case BOOL:
            val = (Val){.type = T_BOOL, .b_val = ast.token.int_val};
            if (ast.token.int_val == MAYBE) {
                array_append(*instructions, create_inst(I_PUSH_MAYBE, null(Val), null(Val)));
            } else {
                array_append(*instructions, create_inst(I_PUSH, val, null(Val)));
            }
            break;
        case STRING_LITERAL:
            val = (Val){.type = T_STRING, .s_val = ast.token.text.data};
            array_append(*instructions, create_inst(I_PUSH, val, null(Val)));
            break;
        case OP_ADD:
            array_append(*instructions, create_inst(I_ADD, null(Val), null(Val)));
            break;
        case OP_SUB:
            array_append(*instructions, create_inst(I_SUB, null(Val), null(Val)));
            break;
        case OP_MUL:
            array_append(*instructions, create_inst(I_MUL, null(Val), null(Val)));
            break;
        case OP_DIV:
            array_append(*instructions, create_inst(I_DIV, null(Val), null(Val)));
            break;
        case OP_MOD:
            array_append(*instructions, create_inst(I_MOD, null(Val), null(Val)));
            break;
        case OP_GREATER:
            array_append(*instructions, create_inst(I_GREATER, null(Val), null(Val)));
            break;
        case OP_GREATEREQ:
            array_append(*instructions, create_inst(I_GREATER_EQUAL, null(Val), null(Val)));
            break;
        case OP_LESS:
            array_append(*instructions, create_inst(I_LESS, null(Val), null(Val)));
            break;
        case OP_LESSEQ:
            array_append(*instructions, create_inst(I_LESS_EQUAL, null(Val), null(Val)));
            break;
        case OP_EQ:
            array_append(*instructions, create_inst(I_EQUAL, null(Val), null(Val)));
            break;
        case OP_NOTEQ:
            array_append(*instructions, create_inst(I_NOT_EQUAL, null(Val), null(Val)));
            break;
        case OP_NOT:
            array_append(*instructions, create_inst(I_NOT, null(Val), null(Val)));
            break;
        case NAME: ;
            bool isglobal;
            VarHeader *vh = get_varheader_from_map_list(var_map_list, ast.token.text, &isglobal);
            array_append(*instructions, create_inst(isglobal? I_READ_GLOBAL : I_READ, (Val){.i_val = get_vartype_size(vh->type), .type = T_INT}, (Val){.i_val = vh->pos, .type = vh->type}));
            break;
        case BLOCK:
            HashMap_free(var_map_list->head->val);
            LL_pop_head(var_map_list);
            gi_stack_pos = temp_stack_ptr;
            break;
        case STMT_SEQ:
            break;
        default:
            print_err("Unhandled case!");
            print_token(ast.token, 0);
            break;
    }
}


Inst *generate_instructions(ASTNode ast) {
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




#define STACK_SIZE 8192

// THIS IS HAPPENING

#define EPSILON 0.001
// 10, 3

double trunc(double a) {
    return a > 0 ? (long long)a : -(long long)(-a);
}

double fmod(double a, double b) {
    return a - trunc(a / b) * b;
}

#define lerp(a, b, w) (a + (b - a) * w)

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




#define print_stack_data(...) printf(__VA_ARGS__"stack ptr: %d, frame ptr: %d, inst: %d \n", stack_ptr, frame_ptr, inst_ptr)

// idk why
#define INPUT_BUFFER_SIZE 453
#define LITERALS_MEMORY_SIZE 1024
#define BENCHMARK_ITERS 100
#define TEXT_BUF_SIZE 8192

#define append(ptr, size) memcpy(temp_stack + temp_stack_ptr++, ptr, size)
#define pop(type) (*(type *)&temp_stack[--temp_stack_ptr])
#define dup() temp_stack[temp_stack_ptr++] = temp_stack_ptr[temp_stack_ptr - 1]
#define tuck(ptr, size) {memmove(temp_stack + 1, temp_stack, temp_stack_ptr++ * 8); memcpy(temp_stack, ptr, size);}
#define pop_bottom(type) ({type val = *(type *)temp_stack; memmove(temp_stack, temp_stack + 1, --temp_stack_ptr * 8); val;})

#define execute_instruction() switch (inst.type) { \
    case I_PUSH: \
        append(&inst.arg1.any_val, get_vartype_size(inst.arg1.type)); \
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
        bool b = pop(int) > 0 ? true : false; \
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
        memcpy(var_stack + frame_ptr + inst.arg2.i_val, temp_stack + --temp_stack_ptr, inst.arg1.i_val); \
        break; \
    } \
    case I_STACK_STORE_GLOBAL: { \
        memcpy(var_stack + inst.arg2.i_val, temp_stack + --temp_stack_ptr, inst.arg1.i_val); \
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



u64 temp_stack[STACK_SIZE] = {0};
int temp_stack_ptr = 0;
char var_stack[STACK_SIZE] = {0};
char text_buffer[TEXT_BUF_SIZE] = {0};
int text_buffer_ptr = 0;
int stack_ptr = 0;
int frame_ptr = 0;

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
        if (inst.type == I_PUSH && inst.arg1.type == T_STRING) {
            inst.arg1.s_val = append_to_text_buffer(inst.arg1.s_val, strlen(inst.arg1.s_val));
        }
    }
}

void run_instructions(Inst *instructions) {
    
    memset(temp_stack, 0, STACK_SIZE);
    temp_stack_ptr = 0;
    memset(var_stack, 0, STACK_SIZE);
    memset(text_buffer, 0, TEXT_BUF_SIZE);
    text_buffer_ptr = 0;
    stack_ptr = 0;
    frame_ptr = 0;

    preprocess_string_literals(instructions);

    int len = array_length(instructions);
    
    for (int inst_ptr = 0; inst_ptr < len; inst_ptr++) {
        Inst inst = instructions[inst_ptr];
        // printf("inst %d \n", inst_ptr);
        execute_instruction();
    }
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
    printf("Program output: \n");

    double start = get_current_process_time_seconds();
    run_instructions(instructions);
    double end = get_current_process_time_seconds();

    printf("Program finished successfully after %.2f seconds! \n", end - start);

    print_text_buffer();

}


void run_benchmark(Inst *instructions) {

    printf("Program output: \n");

    double avg = 0;
    for (int lap = 1; lap <= BENCHMARK_ITERS; lap++) {
        double start = get_current_process_time_seconds();
        run_instructions(instructions);
        double end = get_current_process_time_seconds();

        avg += end - start;
        if (lap % 10 == 0) printf("Time of %dth lap: %.3f \n", lap, end - start);
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
//                 if (inst.arg1.type == T_INT) {
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
        printf("|---");
    }

    printf("<%s>", var_type_names[node.expected_return_type]);
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
            printf(", %s", var_type_names[token.var_type]);
            break;

        default:
            break;
    }

    printf("] \n");
}

void print_tokens(Token *tokens) {
    for (int i = 0; i < array_length(tokens); i++) {
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


int main() {

    while (true) {

        bool benchmark = false;

        start_label: printf("File or raw code? ('file' for file, 'code' for code)\n");
        char buf[4096] = {0};
        char answer[20] = {0};
        fgets(answer, sizeof(answer), stdin);

        if (!strncmp(answer, "file", 4)) {

            benchmark = !strncmp(answer + 5, "bench", 5);

            printf("Enter relative path: ");

            char filepath[100] = {0};

            filepath[0] = '.'; filepath[1] = '.'; filepath[2] = '/';

            fgets((char *)filepath + 3, sizeof(filepath) - 3, stdin);

            filepath[strlen(filepath) - 1] = 0;

            printf("Reading from file: '%s' \n", filepath);

            FILE *file = fopen(filepath, "r");

            if (file == NULL) {
                print_err("Couldn't open file!");
                printf("%d \n", errno);
                
                exit(1);
            }

            char *buf_ptr = buf;

            while (fgets(buf_ptr, sizeof(buf) - (buf_ptr - buf), file)) {
                int len = strlen(buf_ptr);
                if (len > 0 && buf_ptr[len - 1] == '\n') {
                    buf_ptr[len - 1] = ' '; // get rid of \n
                    buf_ptr += len;
                }
            }

            buf[strlen(buf)] = 10; // dont ask, it works


            fclose(file);

            printf("result: '%s' \n", buf);

        } else if (!strncmp(answer, "code", 4)) {
            printf("Write the program here:\n>");

            fgets(buf, sizeof(buf), stdin);

        } else {
            goto start_label;
        }


        

        String *parts = lex(StringRef(buf));

        Token *tokens = tokenize_parts(parts);

        set_parse_tokens(tokens);

        ParseResult res = parse_stmt_seq(0);
        
        preprocess_ast(&res.node);

        if (res.endpos < array_length(tokens))res.success = false;
        else {
            printf(">>> RESULT AST <<<\n");
            print_ast(res.node, 0);
        }

        if (!res.success) {
            printf("INVALID EXPRESSION \n");
        }

        Inst *instructions = generate_instructions(res.node);

        print_instructions(instructions);

        // compile_instructions(instructions);

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