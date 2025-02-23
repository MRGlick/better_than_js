#include <stdio.h>
#include "utils.c"
#include "mystring.c"
#include "array.c"
#include <stdbool.h>


typedef enum TokenType {
    KEYWORD,
    NAME, // Either a variable or a function (which is maybe also a variable)
    STRING_LITERAL,
    INTEGER,
    FLOAT,
    EXPRESSION,
    SYMBOL,
    UNRESOLVED,
    INVALID
} TokenType;

const char SYMBOLS[] = {
    ' ', ',', ';', '(', ')', '{', '}', '+', '-', '/', '*', '=', '>', '<', '!'
};

char *KEYWORDS[] = {
    "var",
    "func",
    "if",
    "while",
    "for",
    "print"
};


typedef struct Token {
    TokenType type;
    union {
        String text;
        struct Token *unresolved_tokens;
        double double_val;
        int int_val;
        char symbol;
    };
} Token;

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
        if (tk.symbol == '+' || tk.symbol == '-') return 3;
        if (tk.symbol == '*' || tk.symbol == '/') return 4;
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

void print_token(Token token) {
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
        default:
            printf("INVALID");
            break;
    }

    printf("] Priority: %d\n", get_token_priority(token));
}

void print_tokens(Token *tokens) {
    for (int i = 0; i < array_length(tokens); i++) {
        print_token(tokens[i]);
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

        print_tokens(tokens);

        free_tokens(tokens);

        free_parts(parts);
    }

    

    return 0;
}