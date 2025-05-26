

// transfer ownership of the ptr to the user
#define move(ptr) ({void *p = ptr; ptr = NULL; p;})