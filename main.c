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
    "print"
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
            printf("bruh \n");
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
            printf("%s \n", tk.text.data);
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

#define create_parse_result(s, n, e) ((ParseResult){.success = s, .node = n, .endpos = e})

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
        //|| get_token(idx).type == NAME
    ) { 
        int token_idx = idx;
        idx += 1;
        return create_parse_result(true, create_ast_node(get_token(token_idx), true), idx);
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
                print_ast(expr_res.node, 0);
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


/*
Rules:
<if-stmt> -> if ( <cond> ) <stmt> | if ( <cond> ) <stmt> else <stmt>
<while-stmt> -> while ( <cond> ) <stmt>

<declare-stmt> -> <typename> <name>
<typename> -> one of a list of allowed types
<name> -> sequence of characters which is NOT defined as a variable, doesnt start with [0-9], allowed characters: [a-z][A-Z]_[0-9]
<assign-stmt> -> <variable> = <expr>
<declare-and-assign-stmt> -> <typename> <name> = <expr>

<stmt> -> <if-stmt> | <while-stmt> | ... | <block>
<block> -> { <stmt-seq> }
<stmt-seq> -> <stmt> <'stmt-seq>
<'stmt-seq> -> <stmt> <'stmt-seq> | epsilon
<cond> -> <bool> <'cond> | !<bool> <'cond>
<'cond> -> && <bool> <'cond> | || <bool> <'cond> | epsilon


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
// ASTNode create_ast_from_tokens(Token *tokens) {
//     ASTNode global = create_ast_node((Token){.type = SCOPE});
    
//     // TokenNode *unresolved_tokens = NULL;
    
//     // int scope_level = 0;

//     // for (int i = 0; i < array_length(tokens); i++) {

//     //     if (tokens[i].type == SYMBOL) {

//     //         if (tokens[i].symbol == ';' && scope_level == 0) {
//     //             if (unresolved_tokens != NULL) {
//     //                 Token tk = {.type = UNRESOLVED, .unresolved_tokens = unresolved_tokens};
//     //                 array_append(global.children, create_ast_node(tk));
//     //                 unresolved_tokens = NULL;
//     //             }
//     //             continue;
//     //         } else if (tokens[i].symbol == '{') {
//     //             scope_level += 1;
//     //             printf("scope level: %d \n", scope_level);
                
//     //         } else if (tokens[i].symbol == '}') {
//     //             scope_level -= 1;
//     //             printf("scope level: %d \n", scope_level);
//     //             if (scope_level < 0) {
//     //                 print_err("Closing curly bracket doesn't have an opening curly bracket!");
//     //             }
//     //             if (scope_level == 0) {
//     //                 if (unresolved_tokens != NULL) {
//     //                     list_append(unresolved_tokens, TokenNode_create(tokens[i]));
//     //                     Token tk = {.type = UNRESOLVED, .unresolved_tokens = unresolved_tokens};
//     //                     array_append(global.children, create_ast_node(tk));
//     //                     unresolved_tokens = NULL;
//     //                     continue;
//     //                 }
//     //             }
                
//     //         } 
//     //     }

        
//     //     if (unresolved_tokens == NULL) {
//     //         printf("UT was null but here anyways: ");
//     //         print_token(tokens[i], 0);
//     //         unresolved_tokens = TokenNode_create(tokens[i]);
//     //     } else {
//     //         print_token(tokens[i], 0);
//     //         list_append(unresolved_tokens, TokenNode_create(tokens[i]));
//     //     }
//     // }

//     // if (scope_level > 0) {
//     //     print_err("Opening curly bracket doesn't have a closing curly bracket!");
//     // }


//     // return global;
// }

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
        
        if (c == ' ') continue;

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
        case STRING_LITERAL:
            printf(", \"%s\"", token.text.data);
            break;
        case NAME:
            printf(", %s", token.text.data);
            break;
        case KEYWORD:
            printf(", %s", token.text.data);
            break;
        case UNRESOLVED:
            printf(":\n");
            for (TokenNode *node = token.unresolved_tokens; node != NULL; node = node->next) {

                print_token(node->token, level + 1);
            }
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
        printf("Paste the program here:\n>");
        char buf[1024] = {0};

        fgets(buf, sizeof(buf), stdin);

        String *parts = lex(StringRef(buf));

        Token *tokens = tokenize_parts(parts);

        print_tokens(tokens);

        set_parse_tokens(tokens);
        ParseResult expr_res = parse_expr(0);
        if (expr_res.endpos < array_length(tokens)) {
            expr_res.success = false;
        } else {
            printf("final ast: \n");
            print_ast(expr_res.node, 0);
        }
        printf("Is valid expression: %s \n", expr_res.success ? "true" : "false");


        // ParseResult term_res = parse_mul_rule(0);
        // printf("Term parsing result: \n");
        // print_ast(term_res.node, 0);

        // ParseResult value_res = parse_value(0);
        // printf("Value parsing result: \n");
        // print_ast(value_res.node, 0);

        free_tokens(tokens);

        print_str_parts(parts);

        free_parts(parts);
    }

    

    return 0;
}