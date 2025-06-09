#include <stdio.h>

int main() {
    // ANSI escape codes for colors
    // Format: \033[<code>m
    printf("\033[31mThis text is red!\033[0m\n");      // 31 = red, 0 = reset
    printf("\033[32mThis text is green!\033[0m\n");    // 32 = green
    printf("\033[33mThis text is yellow!\033[0m\n");   // 33 = yellow
    printf("\033[34mThis text is blue!\033[0m\n");     // 34 = blue
    printf("\033[35mThis text is magenta!\033[0m\n");  // 35 = magenta
    printf("\033[1;36mThis text is cyan!\033[0m\n");     // 36 = cyan

    // You can also combine codes, e.g. bold + color
    printf("\033[1;31mThis text is bold red!\033[0m\n");

    return 0;
}