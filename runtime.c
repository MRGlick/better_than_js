
#include "inttypes.h"

#define COUNTER_BITMASK 0x00FFFFFF
#define STRUCT_METADATA_BITMASK 0xFF000000
#define STRUCT_METADATA_BIT_OFFSET 24
#define STACK_SIZE 8192
#define print_stack_data(...) printf(__VA_ARGS__"stack ptr: %d, frame ptr: %d, inst: %d \n", stack_ptr, frame_ptr, inst_ptr)

#define INPUT_BUFFER_SIZE 453
#define LITERALS_MEMORY_SIZE 1024
#define BENCHMARK_ITERS 100
#define TEXT_BUF_SIZE 8192
#define debug if (1)

typedef struct StructMetadata {
    u32 size;
    u32 offset_count;
    u32 *offsets;
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

static inline void object_dec_ref(void *obj);

static inline void *alloc_object(u32 size) {
    runtime_mallocs++;
    return calloc(size, 0);
}

// #TODO figure out optimizations for this because expensive
static inline void free_object(void *obj) {
    
    ObjectHeader *header = obj;

    int sm_idx = header->data >> STRUCT_METADATA_BIT_OFFSET;

    StructMetadata sm = struct_metadata[sm_idx];

    for (u32 i = 0; i < sm.offset_count; i++) {
        object_dec_ref((char *)obj + sm.offsets[i]);
    }

    runtime_frees++;
    free(obj);
}

static inline int _object_get_refcount(void *obj) {
    ObjectHeader *header = obj;
    return header->data & COUNTER_BITMASK;
}

static inline void object_inc_ref(void *obj) {
    ObjectHeader *header = obj;
    header->data = ((header->data + 1) & COUNTER_BITMASK) | (header->data & STRUCT_METADATA_BITMASK);
    debug printf("Incremented refcounter to %d \n", _object_get_refcount(obj));
}

static inline void object_dec_ref(void *obj) {
    ObjectHeader *header = obj;
    header->data = ((header->data - 1) & COUNTER_BITMASK) | (header->data & STRUCT_METADATA_BITMASK);
    if ((header->data & COUNTER_BITMASK) == 0) {
        debug printf("Deleted! \n");
        free_object(obj);
    } else {
        debug printf("Not deleted, RC = %d \n", _object_get_refcount(obj));
    }
}

#define REFCOUNTER_START_VALUE 1

static inline void object_init_header(void *obj, int meta_idx) {
    ObjectHeader *header = obj;
    //   VV meta_idx
    // 0x__000000
    header->data = meta_idx << 24 | REFCOUNTER_START_VALUE; 
}

void clear_struct_metadata() {
    for (int i = 0; i < struct_metadata_ptr; i++) {
        array_free(struct_metadata[i].offsets);
    }

    memset(struct_metadata, 0, struct_metadata_ptr);
    struct_metadata_ptr = 0;
}
