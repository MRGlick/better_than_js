#include <stdio.h>
#include <time.h>
#include <stdbool.h>

int main() {
    printf("Enter number: ");

    int num;

    scanf("%d", &num);

    int i = 2;

    bool is_prime = true;

    while (i < num / 2) {
        if (num % i == 0) {
            is_prime = false;
        }
        i = i + 1;
    }

    printf("%d \n", is_prime);
}