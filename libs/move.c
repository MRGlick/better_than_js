

// Transfer ownership of the pointer to the caller
#define move(ptr) ({void *p = ptr; ptr = NULL; p;})