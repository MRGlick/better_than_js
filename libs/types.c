
#include "inttypes.h"
#include <stdlib.h>
#include "mystring.c"
#include "match.h"

// 'void' has to be first! Some code in 'main.c' relies on that.
#define TYPE_KINDS \
    X(void) \
    X(bool) \
    X(char) \
    X(int) \
    X(float) \
    X(str) \
    X(named_end_) \
    X(struct) \
    X(array) \
    X(null_ref) \
    X(func) \
    X(count_)

#define named_types_start() (0)
#define named_types_end() (TYPE_named_end_)

typedef enum TypeKind {
    #define X(a) TYPE_##a, 
    TYPE_KINDS
    #undef X
} TypeKind;

char *type_kind_names[] = {
    #define X(a) #a, 
    TYPE_KINDS
    #undef X
};


typedef struct Type {
    TypeKind kind;
    union {
        struct {struct Type *type; } array_data;
        struct {StringRef name; } struct_data;
        struct {struct Type **arg_types; struct Type *return_type; } func_data;
    };
} Type;

// CONSTANTS FOR ALL PRIMITIVES
Type _const_types[] = {
    #define X(a) (Type){.kind = TYPE_##a},
    TYPE_KINDS
    #undef X
};

Type *make_type(TypeKind kind) {
    Type *thing = malloc(sizeof(Type));
    *thing = (Type){.kind = kind}; // which zeros out the other fields BY THE WAY?
    return thing;
}

Type *make_struct_type(String name) {
    Type *thing = malloc(sizeof(Type));
    *thing = (Type){.kind = TYPE_struct, .struct_data.name = name};

    return thing;
}

Type *make_array_type(Type *child_type) {
    Type *thing = malloc(sizeof(Type));
    *thing = (Type){.kind = TYPE_array, .array_data.type = child_type};

    return thing;
}

Type *make_func_type(Type *return_type, Type **arg_types) {
    Type *thing = malloc(sizeof(Type));

    *thing = (Type){
        .kind = TYPE_func, 
        .func_data.return_type = return_type,
        .func_data.arg_types = arg_types
    };

    return thing;
}

Type *copy_type(Type *type) {

    assert(type != NULL);

    Type *cop = make_type(type->kind);

    if (type->kind == TYPE_struct) cop->struct_data.name = type->struct_data.name;
    if (type->kind == TYPE_array) cop->array_data.type = copy_type(type->array_data.type);
    if (type->kind == TYPE_func) {
        cop->func_data.return_type = copy_type(type->func_data.return_type);
        
        int len = array_length(type->func_data.arg_types);
        
        cop->func_data.arg_types = array(Type *, len);

        for (int i = 0; i < len; i++) {
            array_append(cop->func_data.arg_types, copy_type(type->func_data.arg_types[i]));
        }
    }

    return cop;
}

bool types_are_equal(Type *t1, Type *t2);

bool type_arrays_are_equal(Type **arr1, Type **arr2) {

    int len;
    if ((len = array_length(arr1)) != array_length(arr2)) return false;

    for (int i = 0; i < len; i++) {
        if (!types_are_equal(arr1[i], arr2[i])) return false;
    }

    return true;
}

bool types_are_equal(Type *t1, Type *t2) {
    if (t1->kind != t2->kind) return false;
    TypeKind kind = t1->kind;

    match (kind) {
        case (TYPE_array) 
            return types_are_equal(t1->array_data.type, t2->array_data.type);

        case (TYPE_struct) 
            return String_equal(t1->struct_data.name, t2->struct_data.name);
        
        case (TYPE_func) {
            return type_arrays_are_equal(t1->func_data.arg_types, t2->func_data.arg_types)
                && types_are_equal(t1->func_data.return_type, t2->func_data.return_type);
        }

        default ()
            return true;

    }
}
#define SCRATCH_BUF_SIZE 1024

StringRef _type_get_name(Type *type, char *buf, int *buf_idx_ptr) {
    
    #define alloc_str(string) \
        ({ \
            String __s = string; \
            if ((*buf_idx_ptr) + __s.len > SCRATCH_BUF_SIZE) { \
                print_err("type_get_name() scratch buffer max size exceeded!"); \
                return StringRef("<type_get_name overflow placeholder>"); \
            } \
            char *res = buf + (*buf_idx_ptr); \
            memcpy(buf + (*buf_idx_ptr), __s.data, __s.len); \
            (*buf_idx_ptr) += __s.len; \
            StringRef(res); \
        }) \


    match (type->kind) {
        case (TYPE_array) {
            String res = _type_get_name(type->array_data.type, buf, buf_idx_ptr);
            alloc_str(StringRef("[]"));
            return res;
        }
            

        case (TYPE_struct) {
            return alloc_str(type->struct_data.name);
        }
            
        
        case (TYPE_func) {

            String res = _type_get_name(type->func_data.return_type, buf, buf_idx_ptr);

            alloc_str(StringRef("("));

            for (int i = 0; i < array_length(type->func_data.arg_types); i++) {
                if (i > 0) alloc_str(StringRef(", "));

                _type_get_name(type->func_data.arg_types[i], buf, buf_idx_ptr);
            }

            alloc_str(StringRef(")"));

            return res;
        }

        default ()
            return alloc_str(StringRef(type_kind_names[type->kind]));

    }

    #undef alloc_str
}


StringRef type_get_name(Type *type) {
    
    static char buf[SCRATCH_BUF_SIZE] = {0};
    static int buf_idx = 0;
    
    StringRef res = _type_get_name(type, buf, &buf_idx);

    buf_idx++; // So the null terminator doesn't get overriden
    
    return res;
}

#undef SCRATCH_BUF_SIZE

void print_type(Type *type) {

    if (type == NULL) {
        printf("(null_type)");
        return;
    }

    match (type->kind) {
        case (TYPE_array) 
            print_type(type->array_data.type);
            printf("[]");

        case (TYPE_struct) 
            printf("%s", type->struct_data.name.data);
        
        case (TYPE_func) {
            print_type(type->func_data.return_type);
            printf("(");
            int len = array_length(type->func_data.arg_types);
            for (int i = 0; i < len; i++) {
                if (i > 0) printf(", ");
                print_type(type->func_data.arg_types[i]);
            }
            printf(")");
        }

        default ()
            printf("%s", type_kind_names[type->kind]);

    }
}

void _free_type(Type *type) {

    if (type >= _const_types && type <= _const_types + sizeof(_const_types)) {
        print_err("Tried to free a const type! Type: %s", type_get_name(type));
        return;
    }
    if (type == NULL) { // This is fine because recursive types
        return;
    }

    if (type->kind == TYPE_array) _free_type(type->array_data.type);
    if (type->kind == TYPE_func) {
        _free_type(type->func_data.return_type);
        int len = array_length(type->func_data.arg_types);
        for (int i = 0; i < len; i++) {
            _free_type(type->func_data.arg_types[i]);
        }
        array_free(type->func_data.arg_types);
    }
    free(type);
}

// Expects an lvalue. If you wanna pass an rvalue, use _free_type().
#define free_type(type) do { \
    _free_type(type); \
    type = NULL; \
} while (0)
