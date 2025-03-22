#include <stdio.h>
#include "utils.c"
#include "mystring.c"
#include "array.c"
#include <stdbool.h>
#include "token_stuff.c"
#include "hashmap.c"
#include "inttypes.h"


const char SYMBOLS[] = {
    ' ', ',', ';', '(', ')', '{', '}', '+', '-', '/', '*', '=', '>', '<', '!', '&', '|'
};

char *KEYWORDS[] = {
    "if",
    "while",
    "for",
    "print",
    "else"
};




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
            Token tk = {.type = BOOL, .bool_val = true};
            array_append(tokens, tk);
            continue;
        }
        if (String_equal(parts[i], StringRef("false"))) {
            Token tk = {.type = BOOL, .bool_val = false};
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
    _free_ast(ast); \
    ast = (ASTNode){0}; \
} while (0)

void _free_ast(ASTNode ast) {
    
    if (ast.children == NULL) {
        print_err("Tried to free an AST with null children!");
        return;
    }

    for (int i = 0; i < array_length(ast.children); i++) {
        free_ast(ast.children[i]);
    }

    array_free(ast.children);
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

ParseResult parse_add_rule(int idx);

void print_ast(ASTNode node, int level);


ParseResult parse_value(int idx) {

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

    return null(ParseResult);
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

        return null(ParseResult);

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
            return null(ParseResult);
            
        }
        return null(ParseResult);
        
    }

    return null(ParseResult);
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
            return null(ParseResult);
        }

        return null(ParseResult);
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
        return null(ParseResult);
    }

    return null(ParseResult);
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
            return null(ParseResult);

        }
        return null(ParseResult);
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
        return null(ParseResult);
    }

    return null(ParseResult);
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
            return null(ParseResult);

        }
        return null(ParseResult);
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
        return null(ParseResult);
    }

    return null(ParseResult);
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
            return null(ParseResult);

        }
        return null(ParseResult);
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
        return null(ParseResult);
    }

    return null(ParseResult);
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
            return null(ParseResult);

        }
        return null(ParseResult);
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
        return null(ParseResult);
    }

    return null(ParseResult);
}

#define check_keyword(token, keyword_literal) (token.type == KEYWORD && String_equal(token.text, StringRef(keyword_literal)))


ParseResult parse_stmt(int idx);

ParseResult parse_if_stmt(int idx) {

    if (!check_keyword(get_token(idx), "if")) {
        return null(ParseResult);
    }
    
    idx += 1;

    if (get_token(idx).type != LPAREN) {
        return null(ParseResult);
    }

    idx += 1;

    ParseResult expr_res = parse_expr(idx);

    if (!expr_res.success) {
        return null(ParseResult);
    }
    
    idx = expr_res.endpos;

    if (get_token(idx).type != RPAREN) {
        free_ast(expr_res.node);
        return null(ParseResult);
    }

    idx += 1;

    ParseResult stmt_res = parse_stmt(idx);

    if (!stmt_res.success) {
        free_ast(expr_res.node);
        return null(ParseResult);
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
        return null(ParseResult);

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

    ParseResult vardecl_assign_res = parse_vardecl_assign_stmt(idx);
    if (vardecl_assign_res.success) {
        return create_parse_result(true, vardecl_assign_res.node, vardecl_assign_res.endpos);
    }

    return null(ParseResult);
}

ParseResult parse_stmt_seq(int idx) {
    ParseResult stmt_res = parse_stmt(idx);

    if (!stmt_res.success) {
        return null(ParseResult);
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
        return null(ParseResult);
    }

    idx += 1;

    ParseResult stmt_seq_res = parse_stmt_seq(idx);

    if (!stmt_seq_res.success) {
        return null(ParseResult);
    }

    idx = stmt_seq_res.endpos;

    if (get_token(idx).type != RCURLY) {
        free_ast(stmt_seq_res.node);
        return null(ParseResult);
    }

    idx += 1;

    stmt_seq_res.node.token.type = BLOCK;
    return create_parse_result(true, stmt_seq_res.node, idx);
}

ParseResult parse_val_seq(int idx) {

    ParseResult expr_res = parse_expr(idx);

    if (!expr_res.success) {
        return null(ParseResult);
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
        return null(ParseResult);
    }

    idx += 1;

    ParseResult val_seq = parse_val_seq(idx);

    if (!val_seq.success) {
        printf("couldnt handle the vals!! \n");
        return null(ParseResult);
    }

    idx = val_seq.endpos;

    if (get_token(idx).type != STMT_END) {
        print_err("forgor semi colon on print?");
        return null(ParseResult);
    }

    idx += 1;

    val_seq.node.token.type = PRINT_STMT;

    return create_parse_result(true, val_seq.node, idx);
}

ParseResult parse_while_stmt(int idx) {

    if (!check_keyword(get_token(idx), "while")) {
        return null(ParseResult);
    }

    idx += 1;


    if (get_token(idx).type != LPAREN) {
        return null(ParseResult);
    }

    idx += 1;


    ParseResult expr_res = parse_expr(idx);

    if (!expr_res.success) {
        return null(ParseResult);
    }

    idx = expr_res.endpos;

    if (get_token(idx).type != RPAREN) {
        free_ast(expr_res.node);
        return null(ParseResult);
    }

    idx += 1;

    ParseResult stmt_res = parse_stmt(idx);

    if (!stmt_res.success) {
        free_ast(expr_res.node);
        return null(ParseResult);
    }

    idx = stmt_res.endpos;

    ASTNode node = create_ast_node((Token){.type =  WHILE_STMT}, true);

    array_append(node.children, expr_res.node);

    array_append(node.children, stmt_res.node);

    return create_parse_result(true, node, idx);
}

ParseResult parse_vardecl_stmt(int idx) {

    if (get_token(idx).type != TYPE) {
        return null(ParseResult);
    }

    int type_idx = idx;

    idx += 1;

    if (get_token(idx).type != NAME) {
        return null(ParseResult);
    }

    int name_idx = idx;

    idx += 1;

    if (get_token(idx).type != STMT_END) {
        return null(ParseResult);
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
        return null(ParseResult); \
    } \
} while (0)

ParseResult parse_assign_stmt(int idx) {
    
    if (get_token(idx).type != NAME) {
        return null(ParseResult);
    }
    
    int name_idx = idx;
    
    idx += 1;
    
    if (get_token(idx).type != OP_ASSIGN) {
        return null(ParseResult);
    }
    
    idx += 1;
    
    ParseResult expr_res = parse_expr(idx);
    
    if (!expr_res.success) {
        return null(ParseResult);
    }
    
    idx = expr_res.endpos;
    
    
    if (get_token(idx).type != STMT_END) {
        free_ast(expr_res.node);
        print_err("forgot semicolon!");
        return null(ParseResult);
    }
    
    idx += 1;
    
    ASTNode node = create_ast_node((Token){.type = ASSIGN_STMT}, true);
    
    array_append(node.children, create_ast_node(get_token(name_idx), true));
    
    array_append(node.children, expr_res.node);
    
    return create_parse_result(true, node, idx);
}

ParseResult parse_vardecl_assign_stmt(int idx) {
    if (get_token(idx).type != TYPE) {
        return null(ParseResult);
    }
    
    
    
    int type_idx = idx;
    
    idx += 1;
    
    if (get_token(idx).type != NAME) {
        return null(ParseResult);
    }
    
    int name_idx = idx;
    
    idx += 1;
    
    if (get_token(idx).type != OP_ASSIGN) {
        return null(ParseResult);
    }
    
    idx += 1;
    
    ParseResult expr_res = parse_expr(idx);
    
    if (!expr_res.success) {
        return null(ParseResult);
    }
    
    idx = expr_res.endpos;
    
    
    if (get_token(idx).type != STMT_END) {
        free_ast(expr_res.node);
        print_err("forgot semicolon!");
        return null(ParseResult);
    }
    
    idx += 1;
    
    ASTNode node = create_ast_node((Token){.type = DECL_ASSIGN_STMT}, true);
    
    array_append(node.children, create_ast_node(get_token(type_idx), true));
    
    array_append(node.children, create_ast_node(get_token(name_idx), true));
    
    array_append(node.children, expr_res.node);
    
    return create_parse_result(true, node, idx);
}

/*
Rules:
<if-stmt> -> if ( <expr> ) <stmt> | if ( <expr> ) <stmt> else <stmt>

<while-stmt> -> while ( <expr> ) <stmt>
<declare-and-assign-stmt> -> <typename> <name> = <expr>;
<declare-stmt> -> <typename> <name>;

<typename> -> one of a list of allowed types
<name> -> sequence of characters which is NOT defined as a variable, doesnt start with [0-9], allowed characters: [a-z][A-Z]_[0-9]
<assign-stmt> -> <variable> = <expr>;

<stmt> -> <if-stmt> | <while-stmt> | ... | <block>
<block> -> { <stmt-seq> }
<stmt-seq> -> <stmt> | <stmt> <stmt-seq>

<val-seq> -> <expr> | <expr>, <val-seq>

<print-stmt> -> print <val-seq>;


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
<value> -> <bool> | <literal> | <variable> | <int> | <float>
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


#define in_range(a, b, c) ((a >= b) && (a <= c))


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
    
    if (node->token.type == BLOCK || node->token.type == STMT_SEQ) {
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
        printf("read variable: %s, got type: %d \n", node->token.text.data, node->expected_return_type);
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


// #INST
#define INSTRUCTIONS \
X(I_INVALID) \
X(I_PUSH) \
X(I_READ) \
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
X(I_NOT_EQUAL) \
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
X(I_CONVERT_END) \
X(I_PRINT_START) \
X(I_PRINT_INT) \
X(I_PRINT_FLOAT) \
X(I_PRINT_STR) \
X(I_PRINT_BOOL) \
X(I_PRINT_END) \
X(I_STACK_STORE) \
X(I_STORE_STACK_PTR) \
X(I_SET_STACK_PTR)

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
    Val arg;
    
} Inst;

typedef struct VarHeader { 
    i32 pos;
    i32 type;
}VarHeader;

Inst create_inst(VarType type, Val arg) {
    return (Inst){.type = type, .arg = arg};
}

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
        print_val(inst.arg);
        break;
        case I_READ:
        case I_JUMP:
        case I_JUMP_IF:
        case I_JUMP_NOT:
        printf(", %d", inst.arg.i_val);
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

int get_vartype_size(VarType t) {
    switch (t) {
        case T_INT:
        return 4;
        break;
        case T_FLOAT:
        return 4;
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
    if (from == T_STRING && to == T_BOOL) {
        return I_CONVERT_STR_BOOL;
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

    return I_INVALID;
}

int gi_stack_pos = 0;
int gi_label_idx = 0;

void generate_instructions_for_node(ASTNode ast, Inst **instructions, HashMap *var_map);

void generate_instructions_for_vardecl(ASTNode ast, Inst **instructions, HashMap *var_map) {
    if (ast.children[0].token.type != TYPE) {
        print_err("Invalid variable declaration!");
        exit(1);
    }
    
    String var_name = ast.children[1].token.text;

    if (HashMap_contains(var_map, var_name)) {
        print_err("I'm too lazy for variable shadowing! (for now, atleast)");
        exit(1);
    }

    VarType var_type = ast.children[0].token.var_type;
    
    VarHeader vh = (VarHeader){.pos = gi_stack_pos, .type = var_type};

    HashMap_put(var_map, var_name, &vh);
    
    int size = get_vartype_size(var_type);

    array_append(*instructions, create_inst(I_PUSH, (Val){.type = T_INT, .i_val = gi_stack_pos}));
    gi_stack_pos += size;

    if (ast.token.type == DECL_ASSIGN_STMT) {
        generate_instructions_for_node(ast.children[2], instructions, var_map);

        VarType child_return_type = ast.children[2].expected_return_type;

        if (child_return_type != var_type) {
            InstType inst_type = get_cvt_inst_type_for_types(child_return_type, var_type);
            if (inst_type == I_INVALID) {
                print_err("Invalid conversion on variable declaration!");
                printf("Tried to convert from type '%s' to '%s' \n", var_type_names[child_return_type], var_type_names[var_type]);
            } else {
                array_append(*instructions, create_inst(inst_type, null(Val)));
            }
        }
    } else {
        Val val = null(Val);
        val.type = var_type;
        array_append(*instructions, create_inst(I_PUSH, val));
    }
    array_append(*instructions, create_inst(I_STACK_STORE, null(Val)));
}


void generate_instructions_for_assign(ASTNode ast, Inst **instructions, HashMap *var_map) {
    String var_name = ast.children[0].token.text;

    VarHeader *vh = HashMap_get(var_map, var_name);

    array_append(*instructions, create_inst(I_PUSH, (Val){.type = T_INT, .i_val = vh->pos}));

    generate_instructions_for_node(ast.children[1], instructions, var_map);

    array_append(*instructions, create_inst(I_STACK_STORE, null(Val)));

}


void generate_instructions_for_binop(ASTNode ast, Inst **instructions, HashMap *var_map) {

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

        generate_instructions_for_node(ast.children[i], instructions, var_map);

        if (goal_type != ast.children[i].expected_return_type) {
            InstType inst_type = get_cvt_inst_type_for_types(ast.children[i].expected_return_type, goal_type);
            if (inst_type == I_INVALID) {
                print_err("Invalid conversion!");
                printf("Tried to convert from type '%s' to '%s' \n", var_type_names[ast.children[i].expected_return_type], var_type_names[goal_type]);
            } else {
                array_append(*instructions, create_inst(inst_type, null(Val)));
            }
        }
    }

    InstType inst_type = get_inst_type_for_op(ast.token.type, goal_type);
    if (inst_type == I_INVALID) {
        print_err("Invalid operator!");
        printf("Tried to get operator '%s' between '%s' type operands! \n", token_type_names[ast.token.type], var_type_names[goal_type]);
    } else {
        array_append(*instructions, create_inst(inst_type, null(Val)));
    }

}

InstType get_print_inst_for_type(VarType type) {
    if (type == T_INT) return I_PRINT_INT;
    if (type == T_BOOL) return I_PRINT_BOOL;
    if (type == T_FLOAT) return I_PRINT_FLOAT;
    if (type == T_STRING) return I_PRINT_STR;

    return I_INVALID;
}

void generate_instructions_for_print(ASTNode ast, Inst **instructions, HashMap *var_map) {

    int len = array_length(ast.children);
    for (int i = 0; i < len; i++) {

        generate_instructions_for_node(ast.children[i], instructions, var_map);

        InstType inst_type = get_print_inst_for_type(ast.children[i].expected_return_type);
        if (inst_type == I_INVALID) {
            print_err("Invalid argument for print! (seriously how could you mess this up)");
            printf("argument type: %s \n", var_type_names[ast.children[i].expected_return_type]);
        } else {
            array_append(*instructions, create_inst(inst_type, null(Val)));
        }
    }
}

void generate_instructions_for_if(ASTNode ast, Inst **instructions, HashMap *var_map) {
    
    int end_label_idx = gi_label_idx++;
    int else_label_idx;

    int end_label_true_idx;
    int else_label_true_idx;
    if (ast.token.type == IF_ELSE_STMT) else_label_idx = gi_label_idx++;

    // condition
    generate_instructions_for_node(ast.children[0], instructions, var_map);

    if (ast.children[0].expected_return_type != T_BOOL) {
        InstType inst_type = get_cvt_inst_type_for_types(ast.children[0].expected_return_type, T_BOOL);

        if (inst_type == I_INVALID) {
            print_err("Type is ambigous! Cannot be used as an if condition. (you did badly.)");
            printf("type: %s \n", var_type_names[ast.children[0].expected_return_type]);
        } else {
            array_append(*instructions, create_inst(inst_type, null(Val)));
        }
    }
    
    int first_jump_idx = array_length(*instructions);
    array_append(*instructions, create_inst(I_JUMP_NOT, (Val){.type = T_INT, .i_val = -1}));

    // if-body
    generate_instructions_for_node(ast.children[1], instructions, var_map);
    
    int if_body_jump_idx = -1;

    if (ast.token.type == IF_ELSE_STMT) {

        if_body_jump_idx = array_length(*instructions);
        array_append(*instructions, create_inst(I_JUMP, (Val){.type = T_INT, .i_val = end_label_idx}));

        else_label_true_idx = array_length(*instructions);
        array_append(*instructions, create_inst(I_LABEL, (Val){.type = T_INT, .i_val = else_label_idx}));

        // else-body
        generate_instructions_for_node(ast.children[2], instructions, var_map);
    }

    end_label_true_idx = array_length(*instructions);
    array_append(*instructions, create_inst(I_LABEL, (Val){.type = T_INT, .i_val = end_label_idx}));

    if (ast.token.type == IF_STMT) {
        (*instructions)[first_jump_idx].arg.i_val = end_label_true_idx;
    } else {
        (*instructions)[first_jump_idx].arg.i_val = else_label_true_idx;
        (*instructions)[if_body_jump_idx].arg.i_val = end_label_true_idx;
    }

}

void generate_instructions_for_while(ASTNode ast, Inst **instructions, HashMap *var_map) {

    int start_label_idx = array_length(*instructions);

    array_append(*instructions, create_inst(I_LABEL, null(Val)));

    generate_instructions_for_node(ast.children[0], instructions, var_map);

    if (ast.children[0].expected_return_type != T_BOOL) {
        InstType inst_type = get_cvt_inst_type_for_types(ast.children[0].expected_return_type, T_BOOL);

        if (inst_type == I_INVALID) {
            print_err("Type is ambigous! Cannot be used as an while condition. (you did badly.)");
            printf("type: %s \n", var_type_names[ast.children[0].expected_return_type]);
        } else {
            array_append(*instructions, create_inst(inst_type, null(Val)));
        }
    }

    int jump_not_inst_idx = array_length(*instructions);
    array_append(*instructions, create_inst(I_JUMP_NOT, (Val){.type = T_INT, .i_val = -1}));

    // while body
    generate_instructions_for_node(ast.children[1], instructions, var_map);

    array_append(*instructions, create_inst(I_JUMP, (Val){.type = T_INT, .i_val = start_label_idx}));

    int end_label_idx = array_length(*instructions);
    array_append(*instructions, create_inst(I_LABEL, null(Val)));

    (*instructions)[jump_not_inst_idx].arg.i_val = end_label_idx;


}

void generate_instructions_for_node(ASTNode ast, Inst **instructions, HashMap *var_map) {
    
    // independently defined operators
    if (ast.token.type == DECL_ASSIGN_STMT || ast.token.type == DECL_STMT) {
        generate_instructions_for_vardecl(ast, instructions, var_map);
        return;
    }
    if (ast.token.type == ASSIGN_STMT) {
        generate_instructions_for_assign(ast, instructions, var_map);
        return;
    }
    if (in_range(ast.token.type, BINOPS_START, BINOPS_END)) {
        generate_instructions_for_binop(ast, instructions, var_map);
        return;
    }
    if (ast.token.type == PRINT_STMT) {
        generate_instructions_for_print(ast, instructions, var_map);
        return;
    }

    if (ast.token.type == IF_STMT || ast.token.type == IF_ELSE_STMT) {
        generate_instructions_for_if(ast, instructions, var_map);
        return;
    }

    if (ast.token.type == WHILE_STMT) {
        generate_instructions_for_while(ast, instructions, var_map);
        return;
    }



    int temp_stack_ptr;
    HashMap *temp_var_map = NULL;

    // pre children operators
    switch (ast.token.type) {
        case STMT_SEQ:
        case BLOCK:
            temp_stack_ptr = gi_stack_pos;
            temp_var_map = HashMap_copy(var_map);
            array_append(*instructions, create_inst(I_STORE_STACK_PTR, null(Val)));
            break;
        
        default:
            break;
    }
        
    for (int i = 0; i < array_length(ast.children); i++) {
        generate_instructions_for_node(ast.children[i], instructions, temp_var_map != NULL ? temp_var_map : var_map);
    }

    Val val;


    // post children operators
    switch (ast.token.type) {
        case INTEGER:
            val = (Val){.type = T_INT, .i_val = ast.token.int_val};
            array_append(*instructions, create_inst(I_PUSH, val));
            break;
        case FLOAT:
            val = (Val){.type = T_FLOAT, .f_val = ast.token.double_val};
            array_append(*instructions, create_inst(I_PUSH, val));
            break;
        case BOOL:
            val = (Val){.type = T_BOOL, .b_val = ast.token.bool_val};
            array_append(*instructions, create_inst(I_PUSH, val));
            break;
        case STRING_LITERAL:
            val = (Val){.type = T_STRING, .s_val = ast.token.text.data};
            array_append(*instructions, create_inst(I_PUSH, val));
            break;
        case OP_ADD:
            array_append(*instructions, create_inst(I_ADD, null(Val)));
            break;
        case OP_SUB:
            array_append(*instructions, create_inst(I_SUB, null(Val)));
            break;
        case OP_MUL:
            array_append(*instructions, create_inst(I_MUL, null(Val)));
            break;
        case OP_DIV:
            array_append(*instructions, create_inst(I_DIV, null(Val)));
            break;
        case OP_MOD:
            array_append(*instructions, create_inst(I_MOD, null(Val)));
            break;
        case OP_GREATER:
            array_append(*instructions, create_inst(I_GREATER, null(Val)));
            break;
        case OP_GREATEREQ:
            array_append(*instructions, create_inst(I_GREATER_EQUAL, null(Val)));
            break;
        case OP_LESS:
            array_append(*instructions, create_inst(I_LESS, null(Val)));
            break;
        case OP_LESSEQ:
            array_append(*instructions, create_inst(I_LESS_EQUAL, null(Val)));
            break;
        case OP_EQ:
            array_append(*instructions, create_inst(I_EQUAL, null(Val)));
            break;
        case OP_NOTEQ:
            array_append(*instructions, create_inst(I_NOT_EQUAL, null(Val)));
            break;
        case NAME:
            if (!HashMap_contains(var_map, ast.token.text)) {
                print_err("Unknown identifier!");
                printf("'%s' \n", ast.token.text.data);
                exit(1);
            }
            VarHeader *vh = HashMap_get(var_map, ast.token.text);
            array_append(*instructions, create_inst(I_PUSH, (Val){.i_val = get_vartype_size(vh->type), .type = T_INT}));
            array_append(*instructions, create_inst(I_READ, (Val){.i_val = vh->pos, .type = vh->type}));
            break;
        case STMT_SEQ:
        case BLOCK:
            HashMap_free(temp_var_map);
            gi_stack_pos = temp_stack_ptr;
            array_append(*instructions, create_inst(I_SET_STACK_PTR, null(Val)));
            break;
    
        default:
            break;
            //     print_err("Unhandled case!");
            //     print_token(ast.token, 0);
    }
}


Inst *generate_instructions(ASTNode ast) {
    Inst *res = array(Inst, 20);
    HashMap *var_map = HashMap(VarHeader); // I KNOW ABOUT THIS.

    gi_stack_pos = 0;
    gi_label_idx = 0;
    generate_instructions_for_node(ast, &res, var_map);

    HashMap_free(var_map);

    return res;
}


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

        bool part_of_double_symbol = (stop_char && i + 1 < text.len) && (
            (c == '=' && text.data[i + 1] == '=')
            || (c == '>' && text.data[i + 1] == '=')
            || (c == '<' && text.data[i + 1] == '=')
            || (c == '!' && text.data[i + 1] == '=')
            || (c == '&' && text.data[i + 1] == '&')
            || (c == '|' && text.data[i + 1] == '|')
        );

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
            printf(", %s", token.bool_val ? "true" : "false");
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

        start_label: printf("File or raw code? ('file' for file, 'code' for code)\n");
        char buf[4096] = {0};
        char answer[20] = {0};
        fgets(answer, sizeof(answer), stdin);

        if (!strncmp(answer, "file", 4)) {

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
        typeify_tree_wrapper(&res.node);
        if (res.endpos < array_length(tokens)) {
            res.success = false;
        } else {
            printf(">>> RESULT AST <<<\n");
            print_ast(res.node, 0);
        }

        if (!res.success) {
            printf("INVALID EXPRESSION \n");
        }

        Inst *instructions = generate_instructions(res.node);

        print_instructions(instructions);

        array_free(instructions);

        free_tokens(tokens);

        free_parts(parts);
    }

    

    return 0;
}