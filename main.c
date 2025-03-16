#include <stdio.h>
#include "utils.c"
#include "mystring.c"
#include "array.c"
#include <stdbool.h>
#include "token_stuff.c"



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

#define INSTRUCTIONS\
    X(I_PUSH) \
    X(I_READ) \
    X(I_ADD) \
    X(I_SUB) \
    X(I_MUL) \
    X(I_DIV) \
    X(I_MOD) \
    X(I_GREATER) \
    X(I_GREATER_EQUAL) \
    X(I_LESS) \
    X(I_LESS_EQUAL) \
    X(I_EQUAL) \
    X(I_NOT_EQUAL) \
    X(I_LABEL) \
    X(I_JUMP) \
    X(I_JUMP_IF) \
    X(I_JUMP_NOT) \
    X(I_CONVERT) \
    X(I_STACK_ALLOC) \
    X(I_STACK_STORE) \
    X(I_STORE_STACK_PTR) \
    X(I_SET_STACK_PTR) \

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
        int i_val;
        double f_val;
        bool b_val;
        char *s_val;
    };
} Val;

typedef struct Inst {
    InstType type;
    Val arg;

} Inst;


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
        case I_STACK_ALLOC:
        case I_READ:
            printf(", %d", inst.arg.i_val);
            break;
        case I_CONVERT:
            printf(", ");
            print_val(inst.arg);
            break;
        case I_LABEL:
        case I_JUMP:
        case I_JUMP_IF:
        case I_JUMP_NOT:
            printf(", %s", inst.arg.s_val);
            break;


        default:
            break;
    }
    printf("] \n");
}

void print_instructions(Inst *arr) {
    for (int i = 0; i < array_length(arr); i++) {
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

int gi_stack_pos = 0;
int gi_stack_size = 0;

void generate_instructions_for_node(ASTNode ast, Inst *instructions);

void generate_instructions_for_vardecl(ASTNode ast, Inst *instructions) {
    if (ast.children[0].token.type != TYPE) {
        print_err("Invalid variable declaration!");
        exit(1);
    }

    int size = get_vartype_size(ast.children[0].token.var_type);

    array_append(instructions, create_inst(I_STACK_ALLOC, (Val){.type = T_INT, .i_val = size}));
    gi_stack_size += size;
    array_append(instructions, create_inst(I_PUSH, (Val){.type = T_INT, .i_val = gi_stack_pos}));
    gi_stack_pos += size;
    if (gi_stack_pos > gi_stack_size) {
        print_err("exceeded current stack!");
    }
    generate_instructions_for_node(ast.children[2], instructions);
    array_append(instructions, create_inst(I_STACK_STORE, null(Val)));
}

void generate_instructions_for_node(ASTNode ast, Inst *instructions) {
    
    // independently defined operators
    if (ast.token.type == DECL_ASSIGN_STMT) {
        generate_instructions_for_vardecl(ast, instructions);
        return;
    }

    int temp_stack_ptr;

    // pre children operators
    switch (ast.token.type) {
        case STMT_SEQ:
        case BLOCK:
            temp_stack_ptr = gi_stack_pos;
            array_append(instructions, create_inst(I_STORE_STACK_PTR, null(Val)));
            break;
        
        default:
            break;
    }
    
    
    
    for (int i = 0; i < array_length(ast.children); i++) {
        generate_instructions_for_node(ast.children[i], instructions);
    }

    Val val;


    // post children operators
    switch (ast.token.type) {
        case INTEGER:
            val = (Val){.type = T_INT, .i_val = ast.token.int_val};
            array_append(instructions, create_inst(I_PUSH, val));
            break;
        case FLOAT:
            val = (Val){.type = T_FLOAT, .f_val = ast.token.double_val};
            array_append(instructions, create_inst(I_PUSH, val));
            break;
        case BOOL:
            val = (Val){.type = T_BOOL, .b_val = ast.token.bool_val};
            array_append(instructions, create_inst(I_PUSH, val));
            break;
        case OP_ADD:
            array_append(instructions, create_inst(I_ADD, null(Val)));
            break;
        case OP_SUB:
            array_append(instructions, create_inst(I_SUB, null(Val)));
            break;
        case OP_MUL:
            array_append(instructions, create_inst(I_MUL, null(Val)));
            break;
        case OP_DIV:
            array_append(instructions, create_inst(I_DIV, null(Val)));
            break;
        case OP_MOD:
            array_append(instructions, create_inst(I_MOD, null(Val)));
            break;
        case OP_GREATER:
            array_append(instructions, create_inst(I_GREATER, null(Val)));
            break;
        case OP_GREATEREQ:
            array_append(instructions, create_inst(I_GREATER_EQUAL, null(Val)));
            break;
        case OP_LESS:
            array_append(instructions, create_inst(I_LESS, null(Val)));
            break;
        case OP_LESSEQ:
            array_append(instructions, create_inst(I_LESS_EQUAL, null(Val)));
            break;
        case OP_EQ:
            array_append(instructions, create_inst(I_EQUAL, null(Val)));
            break;
        case OP_NOTEQ:
            array_append(instructions, create_inst(I_NOT_EQUAL, null(Val)));
            break;
        case NAME:
            array_append(instructions, create_inst(I_READ, (Val){.s_val = ast.token.text.data}));
            break;
        case STMT_SEQ:
        case BLOCK:
            gi_stack_pos = temp_stack_ptr;
            array_append(instructions, create_inst(I_SET_STACK_PTR, null(Val)));
            break;
    
        default:
            break;
            //     print_err("Unhandled case!");
            //     print_token(ast.token, 0);
    }
}

Inst *generate_instructions(ASTNode ast) {
    Inst *res = array(Inst, 20);

    gi_stack_pos = 0;
    gi_stack_size = 0;
    generate_instructions_for_node(ast, res);

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