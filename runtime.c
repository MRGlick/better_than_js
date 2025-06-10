
#include "inttypes.h"
#include "utils.c"
#include <stdio.h>
#include "debug.h"
#include "mystring.c"
#include "globals.c"
#include "stdbool.h"

#define COUNTER_BITMASK 0x00FFFFFF
#define METADATA_BITMASK 0xFF000000
#define METADATA_BIT_OFFSET 24
#define STACK_SIZE 8192
#define print_stack_data(...) printf(__VA_ARGS__"stack ptr: %d, frame ptr: %d, inst: %d \n", stack_ptr, frame_ptr, inst_ptr)

#define INPUT_BUFFER_SIZE 453
#define LITERALS_MEMORY_SIZE 1024
#define BENCHMARK_ITERS 100
#define TEXT_BUF_SIZE 8192
#define REFCOUNTER_START_VALUE 1

typedef struct RefOffset {
    u16 offset;
    bool is_array;
} RefOffset;

typedef struct StructMetadata {
    u32 size;
    u32 offset_count;
    RefOffset *offsets;
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
    for (size_t i = 0; i < sm.offset_count; i++)
        printf("offset: %d is_arr: %s", sm.offsets[i].offset, sm.offsets[i].is_array ? "true" : "false");
    printf("\n");
    printf("    Size: %d bytes\n ", sm.size);
} 

void *tracked_malloc(size_t size) {
    runtime_mallocs++;
    return calloc(size, 1); // this works stop hating
}

void tracked_free(void *ptr) {
    runtime_frees++;
    free(ptr);
}

static inline int _object_get_refcount(void *obj) {

    assert(obj != NULL);

    ObjectHeader *header = obj;
    return header->data & COUNTER_BITMASK;
}

static inline void object_dec_ref(void *obj, bool is_array);

static inline void *alloc_object(u32 size) {
    return tracked_malloc(size);
}

// test

// #TODO figure out optimizations for this because expensive
static inline void free_object(void *obj) {
    
    assert(obj != NULL); // a null shouldnt be able to reach this function

    ObjectHeader *header = obj;

    int sm_idx = header->data >> METADATA_BIT_OFFSET;

    StructMetadata sm = struct_metadata[sm_idx];

    for (u32 i = 0; i < sm.offset_count; i++) {

        void *child_obj = *(void **)((char *)obj + sm.offsets[i].offset);
        object_dec_ref(child_obj, sm.offsets[i].is_array);
    }

    tracked_free(obj);
}

static inline bool is_reference_typekind(TypeKind);

static inline void free_object_array(void *obj) {
    assert(obj != NULL); 

    ObjectHeader *header = obj;

    TypeKind subtype_kind = header->data >> METADATA_BIT_OFFSET;
    
    if (is_reference_typekind(subtype_kind)) {
        int elem_size = get_typekind_size(subtype_kind);
        int len = *(int *)(obj + sizeof(ObjectHeader));

        for (int i = 0; i < len; i++) {
            void *child_obj = *(void **)(obj + calculate_array_offset(i, elem_size));

            object_dec_ref(child_obj, subtype_kind == TYPE_array);
        }
    }

    tracked_free(obj);
}


static inline void object_inc_ref(void *obj) {

    if (obj == NULL) return; // this is fine

    ObjectHeader *header = obj;
    header->data = ((header->data + 1) & COUNTER_BITMASK) | (header->data & METADATA_BITMASK);
    // debug printf("<%p>: inc refcount to %d \n", obj, _object_get_refcount(obj));
}

static inline void object_dec_ref(void *obj, bool is_array) {

    if (obj == NULL) return; // this is also fine? yeah its fine

    assert(_object_get_refcount(obj) != 0); // is it worth the performance? yes

    ObjectHeader *header = obj;
    header->data = ((header->data - 1) & COUNTER_BITMASK) | (header->data & METADATA_BITMASK);
    // debug printf("<%p>: dec refcount to %d \n", obj, _object_get_refcount(obj));

    if ((header->data & COUNTER_BITMASK) == 0) {
        if (is_array) free_object_array(obj);
        else free_object(obj);
    }
}


static inline void object_init_header(void *obj, int meta_idx) {

    assert(obj != NULL); // this is NOT fine

    ObjectHeader *header = obj;
    //   VV meta_idx
    // 0x__000000
    header->data = (meta_idx << METADATA_BIT_OFFSET) | REFCOUNTER_START_VALUE; 
}


void *_init_n_dim_array(TypeKind final_subtype_kind, int n, int *dims, int dims_idx) {

    assert(dims_idx < n);

    // base case
    if (dims_idx == n - 1) {

        int elem_size = get_typekind_size(final_subtype_kind);

        int len = dims[0];

        void *arr = tracked_malloc(calculate_array_offset(elem_size, len));
        object_init_header(arr, final_subtype_kind);
        int *length = (arr + sizeof(ObjectHeader));
        *length = dims[dims_idx];
        return arr;

    }

    int elem_size = 8;

    int len = dims[dims_idx];

    void *arr = tracked_malloc(calculate_array_offset(len, elem_size));

    object_init_header(arr, TYPE_array);

    int *length = (arr + sizeof(ObjectHeader));
    *length = len;

    for (int i = 0; i < len; i++) {
        void **child = (void **)(arr + calculate_array_offset(i, elem_size));

        assert((*child) == NULL); // if its not then we're in trouble

        void *thing = _init_n_dim_array(final_subtype_kind, n, dims, dims_idx + 1);

        *child = thing;
    }

    return arr;

}

// elem size is the size of the final subtype
void *init_n_dim_array(TypeKind final_subtype_kind, int n, int *dims) {

    // i am simply not dealing with these cases
    assert(n > 0);
    assert(dims != NULL);
    assert(final_subtype_kind >= 0 && final_subtype_kind < TYPE_count_);

    void *retval = _init_n_dim_array(final_subtype_kind, n, dims, 0);

    return retval;
}

void clear_struct_metadata() {
    for (int i = 0; i < struct_metadata_ptr; i++) {
        array_free(struct_metadata[i].offsets);
    }

    memset(struct_metadata, 0, struct_metadata_ptr);
    struct_metadata_ptr = 0;
}


// Thanks Gemini
void clear_n_lines(int n) {
    if (n <= 0) {
        return; // Nothing to clear
    }
    printf("\x1b[%dA", n);

    for (int i = 0; i < n; i++) {
        printf("\x1b[2K");
        if (i < n - 1) {
            printf("\x1b[1B");
        }
    }

    printf("\x1b[%dA", n - 1);
    printf("\r");
    
    // fflush(stdout); // not sure?
}
