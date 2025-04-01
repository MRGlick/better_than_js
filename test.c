#include <stdio.h>
#include <stdint.h>

int main() {
    uint64_t value = 3;
    uint8_t* p_char = (uint8_t*)&value;
    uint32_t* p_int = (uint32_t*)&value;

    printf("char value: %u\n", *p_char);
    printf("int value: %u\n", *p_int);

    return 0;
}