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

int get_token_priority(Token tk) {
    // arithmetic -> 3,4,5
    // assignment -> 0
    




    return 999999;
}

Token *tokenize_parts(String *parts) {
    Token *tokens = array(Token, 10);

    int len = array_length(parts);

    for (int i = 0; i < len; i++) {
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

ASTNode create_ast_node(Token tk) {
    ASTNode node = {0};
    node.children = array(ASTNode, 2);
    node.token = tk;
    
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
    ASTNode node;
} ParseResult;

#define PARSE_FAILED ((ParseResult){0})

int parse_idx = 0;
Token *parse_tokens = NULL;

void set_parse_tokens(Token *tokens) {
    parse_tokens = tokens;
    parse_idx = 0;
}

bool match_token(Token token, bool verbose) {
    if (parse_tokens == NULL) {
        print_err("Tried to match tokens, but there aren't any!");
    }
    if (parse_idx >= array_length(parse_tokens)) {
        print_err("Tried to match token, but reachd end of tokens!");
    }

    Token curr_token = parse_tokens[parse_idx];
    if (memcmp(&curr_token, &token, sizeof(Token)) == 0) {
        parse_idx += 1;
        return true;
    } else {
        if (verbose) {
            print_err("Failed to match token!");
            printf("Expected:\n\t");
            print_token(curr_token, 0);
            printf("Instead got: \n\t");
            print_token(token, 0);
        }
        
        return false;
    }
}

void parse_value() {
    
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
<bool> -> <expr> == <expr> | <expr> != <expr> ...
<expr> -> <term> <'expr>
<'expr> -> + <term> <'expr> | - <term> <'expr> | epsilon
<term> -> <factor> <'term>
<'term> -> * <factor> <'term> | / <factor> <'term> | epsilon
<factor> -> <value> | ( <expr> )
<value> -> true | false | <literal> | <variable> | <int> | <float>

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
    if (level) {
        printf("----AST---- %d\n", level);
    } else {
        printf("----AST----\n");
    }
    print_token(node.token, level);
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

    for (int i = 0; i < text.len - 1; i++) {
        
        char c = text.data[i];
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
        array_append(parts, String_ncopy_from_literal(buf, pos));
        pos = 0;
    }

    return parts;

}

void print_token(Token token, int level) {
    for (int i = 0; i < level; i++) {
        printf("\t");
    }
    printf("[%s", token_type_names[token.type]);
    switch (token.type) {
        case INTEGER:
            printf(", %d", token.int_val);
            break;
        case FLOAT:
            printf(", %.2f", token.double_val);
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

    printf("] Priority: %d\n", get_token_priority(token));
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

        free_tokens(tokens);

        print_str_parts(parts);

        free_parts(parts);
    }

    

    return 0;
}