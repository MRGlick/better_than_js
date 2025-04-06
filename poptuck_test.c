#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define append(ptr, size) memcpy(temp_stack + temp_stack_ptr++, ptr, size)
#define pop(type) (*(type *)&temp_stack[--temp_stack_ptr])
#define dup() temp_stack[temp_stack_ptr++] = temp_stack_ptr[temp_stack_ptr - 1]
#define tuck(ptr, size) {memmove(temp_stack + 1, temp_stack, temp_stack_ptr++ * 8); memcpy(temp_stack, ptr, size);}
#define pop_bottom(type) ({type val = *(type *)temp_stack; memmove(temp_stack, temp_stack + 1, --temp_stack_ptr * 8); val;})

int main() {
    uint64_t temp_stack[1024];
    int temp_stack_ptr = 0;

    int x = 1;
    memmove(temp_stack + 1, temp_stack, temp_stack_ptr * 8);
    temp_stack_ptr += 1;
    *(int *)temp_stack = x;
    int y = 2;
    for (int i = 0; i < 4; i++) {
        printf("%d ", *(int *)(temp_stack + i));
    }
    printf("temp_stack_ptr: %d \n", temp_stack_ptr);
    memmove(temp_stack + 1, temp_stack, temp_stack_ptr * 8);
    temp_stack_ptr += 1;
    *(int *)temp_stack = y;
    int z = 3;
    for (int i = 0; i < 4; i++) {
        printf("%d ", *(int *)(temp_stack + i));
    }
    printf("temp_stack_ptr: %d \n", temp_stack_ptr);
    memmove(temp_stack + 1, temp_stack, temp_stack_ptr * 8);
    temp_stack_ptr += 1;
    *(int *)temp_stack = z;
    for (int i = 0; i < 4; i++) {
        printf("%d ", *(int *)(temp_stack + i));
    }
    printf("temp_stack_ptr: %d \n", temp_stack_ptr);
    int w = 4;
    memmove(temp_stack + 1, temp_stack, temp_stack_ptr * 8);
    temp_stack_ptr += 1;
    *(int *)temp_stack = w;

    for (int i = 0; i < 4; i++) {
        printf("%d ", *(int *)(temp_stack + i));
    }
    printf("temp_stack_ptr: %d \n", temp_stack_ptr);


    int p1 = pop_bottom(int);
    int p2 = pop_bottom(int);
    int p3 = pop_bottom(int);
    int p4 = pop_bottom(int);

    printf("x: %d, y: %d, z: %d, w: %d\n p1: %d, p2: %d, p3: %d, p4: %d \n", x, y, z, w, p1, p2, p3, p4);
}