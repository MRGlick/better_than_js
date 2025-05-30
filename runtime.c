
#include "inttypes.h"
#include "utils.c"
#include <stdio.h>
#include "debug.h"
#include "mystring.c"

#define COUNTER_BITMASK 0x00FFFFFF
#define STRUCT_METADATA_BITMASK 0xFF000000
#define STRUCT_METADATA_BIT_OFFSET 24
#define STACK_SIZE 8192
#define print_stack_data(...) printf(__VA_ARGS__"stack ptr: %d, frame ptr: %d, inst: %d \n", stack_ptr, frame_ptr, inst_ptr)

#define INPUT_BUFFER_SIZE 453
#define LITERALS_MEMORY_SIZE 1024
#define BENCHMARK_ITERS 100
#define TEXT_BUF_SIZE 8192
#define REFCOUNTER_START_VALUE 1

typedef struct StructMetadata {
    u32 size;
    u32 offset_count;
    u32 *offsets;
    String struct_name;
} StructMetadata;

StructMetadata struct_metadata[256] = {0};
int struct_metadata_ptr = 0;
u64 temp_stack[STACK_SIZE] = {0};
int temp_stack_ptr = 0;
char var_stack[STACK_SIZE] = {0};
char text_buffer[TEXT_BUF_SIZE] = {0};
int text_buffer_ptr = 0;
int stack_ptr = 0;
int frame_ptr = 0;
int runtime_mallocs = 0;
int runtime_frees = 0;

// most significant byte is metadata_idx, the rest are the refcounter
typedef struct ObjectHeader {
    u32 data;
} ObjectHeader;

void print_struct_meta(StructMetadata sm) {
    printf("Struct metadata: \n");
    printf("    Name: %s \n", sm.struct_name.data);
    printf("    Offset count: %d \n", sm.offset_count);
    printf("    offsets: ");
    for (size_t i = 0; i < sm.offset_count; i++) printf("%d ", sm.offsets[i]);
    printf("\n");
    printf("    Size: %d bytes\n ", sm.size);
} 

static inline int _object_get_refcount(void *obj) {

    assert(obj != NULL);

    ObjectHeader *header = obj;
    return header->data & COUNTER_BITMASK;
}

static inline void object_dec_ref(void *obj);

static inline void *alloc_object(u32 size) {
    runtime_mallocs++;
    return calloc(size, 1);
}

// #TODO figure out optimizations for this because expensive
static inline void free_object(void *obj) {
    
    assert(obj != NULL); // a null shouldnt be able to reach this function

    ObjectHeader *header = obj;

    int sm_idx = header->data >> STRUCT_METADATA_BIT_OFFSET;

    StructMetadata sm = struct_metadata[sm_idx];

    for (u32 i = 0; i < sm.offset_count; i++) {

        void *child_obj = *(void **)((char *)obj + sm.offsets[i]);
        object_dec_ref(child_obj);
    }

    runtime_frees++;
    free(obj);
}


static inline void object_inc_ref(void *obj) {

    if (obj == NULL) return; // this is fine

    ObjectHeader *header = obj;
    header->data = ((header->data + 1) & COUNTER_BITMASK) | (header->data & STRUCT_METADATA_BITMASK);
    // debug printf("<%p>: inc refcount to %d \n", obj, _object_get_refcount(obj));
}

static inline void object_dec_ref(void *obj) {

    if (obj == NULL) return; // this is also fine? yeah its fine

    assert(_object_get_refcount(obj) != 0); // is it worth the performance? yes

    ObjectHeader *header = obj;
    header->data = ((header->data - 1) & COUNTER_BITMASK) | (header->data & STRUCT_METADATA_BITMASK);
    // debug printf("<%p>: dec refcount to %d \n", obj, _object_get_refcount(obj));

    if ((header->data & COUNTER_BITMASK) == 0) {
        free_object(obj);
    }
}


static inline void object_init_header(void *obj, int meta_idx) {

    assert(obj != NULL); // this is NOT fine

    ObjectHeader *header = obj;
    //   VV meta_idx
    // 0x__000000
    header->data = (meta_idx << STRUCT_METADATA_BIT_OFFSET) | REFCOUNTER_START_VALUE; 
}

void clear_struct_metadata() {
    for (int i = 0; i < struct_metadata_ptr; i++) {
        array_free(struct_metadata[i].offsets);
    }

    memset(struct_metadata, 0, struct_metadata_ptr);
    struct_metadata_ptr = 0;
}
