

#include <stdio.h>



int main() {

    
    void *addr = &&label2;

    goto *addr;

    label:
    printf("hi");
    goto end;
    label2:
    printf("bye");
    goto end;

    end:
    return 0;
}