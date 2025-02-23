#include <stdio.h>
#include "utils.c"
#include "mystring.c"
#include "array.c"
#include <stdbool.h>
#include "token_stuff.c"



const char SYMBOLS[] = {
    ' ', ',', ';', '(', ')', '{', '}', '+', '-', '/', '*', '=', '>', '<', '!'
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
    if (tk.type == SYMBOL) {
        if (tk.symbol == '+' || tk.symbol == '-') return 10;
        if (tk.symbol == '*' || tk.symbol == '/') return 20;
        if (tk.symbol == '=') return 0;
    }




    return 999999;
}

Token *tokenize_parts(String *parts) {
    Token *tokens = array(Token, 10);

    for (int i = 0; i < array_length(parts); i++) {
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
        if (is_symbol(parts[i])) {
            Token tk = {.type = SYMBOL, .symbol = parts[i].data[0]};
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

ASTNode create_ast_from_tokens(Token *tokens) {
    ASTNode global = create_ast_node((Token){.type = SCOPE});
    
    TokenNode *unresolved_tokens = NULL;
    
    int scope_level = 0;

    for (int i = 0; i < array_length(tokens); i++) {

        if (tokens[i].type == SYMBOL) {

            if (tokens[i].symbol == ';' && scope_level == 0) {
                if (unresolved_tokens != NULL) {
                    Token tk = {.type = UNRESOLVED, .unresolved_tokens = unresolved_tokens};
                    array_append(global.children, create_ast_node(tk));
                    unresolved_tokens = NULL;
                }
                continue;
            } else if (tokens[i].symbol == '{') {
                scope_level += 1;
                printf("scope level: %d \n", scope_level);
                
            } else if (tokens[i].symbol == '}') {
                scope_level -= 1;
                printf("scope level: %d \n", scope_level);
                if (scope_level < 0) {
                    print_err("Closing curly bracket doesn't have an opening curly bracket!");
                }
                if (scope_level == 0) {
                    if (unresolved_tokens != NULL) {
                        list_append(unresolved_tokens, TokenNode_create(tokens[i]));
                        Token tk = {.type = UNRESOLVED, .unresolved_tokens = unresolved_tokens};
                        array_append(global.children, create_ast_node(tk));
                        unresolved_tokens = NULL;
                        continue;
                    }
                }
                
            } 
        }

        
        if (unresolved_tokens == NULL) {
            printf("UT was null but here anyways: ");
            print_token(tokens[i], 0);
            unresolved_tokens = TokenNode_create(tokens[i]);
        } else {
            print_token(tokens[i], 0);
            list_append(unresolved_tokens, TokenNode_create(tokens[i]));
        }
    }

    if (scope_level > 0) {
        print_err("Opening curly bracket doesn't have a closing curly bracket!");
    }


    return global;
}

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

    for (int i = 0; i < text.len; i++) {
        
        char c = text.data[i];
        bool stop_char = is_stop_char(c);

        if (stop_char && pos > 0) {
            array_append(parts, String_ncopy_from_literal(buf, pos));
            pos = 0;

        }
        
        if (c == ' ') continue;

        buf[pos++] = c;

        if (stop_char) {
            array_append(parts, String_ncopy_from_literal(buf, pos));
            pos = 0;
        }
    }


    return parts;

}

void print_token(Token token, int level) {
    for (int i = 0; i < level; i++) {
        printf("\t");
    }
    printf("[");
    switch (token.type) {
        case INTEGER:
            printf("INTEGER, %d", token.int_val);
            break;
        case FLOAT:
            printf("FLOAT, %.2f", token.double_val);
            break;
        case NAME:
            printf("NAME, %s", token.text.data);
            break;
        case SYMBOL:
            printf("SYMBOL, %c", token.symbol);
            break;
        case KEYWORD:
            printf("KEYWORD, %s", token.text.data);
            break;
        case UNRESOLVED:
            printf("UNRESOLVED:\n");
            for (TokenNode *node = token.unresolved_tokens; node != NULL; node = node->next) {

                print_token(node->token, level + 1);
            }
        case SCOPE:
            printf("SCOPE");
            break;

        default:
            printf("INVALID");
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
        printf("> %s \n", parts[i].data);
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

        ASTNode ast = create_ast_from_tokens(tokens);

        print_tokens(tokens);
        
        print_ast(ast, 0);

        free_tokens(tokens);

        free_parts(parts);
    }

    

    return 0;
}