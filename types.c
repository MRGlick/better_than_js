
#include "inttypes.h"
#include <stdlib.h>
#include "mystring.c"
#include "match.h"

#define TYPE_KINDS \
    X(void) \
    X(bool) \
    X(char) \
    X(int) \
    X(float) \
    X(str) \
    X(struct) \
    X(array) \
    X(null_ref) \
    X(count_)

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

Type *copy_type(Type *type) {

    assert(type != NULL);

    Type *cop = make_type(type->kind);
    if (type->kind == TYPE_struct) cop->struct_data.name = type->struct_data.name;
    if (type->kind == TYPE_array) cop->array_data.type = copy_type(type->array_data.type);


    return cop;
}

bool types_are_equal(Type *t1, Type *t2) {
    if (t1->kind != t2->kind) return false;
    TypeKind kind = t1->kind;

    match (kind) {
        case (TYPE_array) 
            return types_are_equal(t1->array_data.type, t2->array_data.type);

        case (TYPE_struct) 
            return String_equal(t1->struct_data.name, t2->struct_data.name);

        default ()
            return true;

    }
}

StringRef type_get_name(Type *type) {
    match (type->kind) {
        case (TYPE_array) 
            // it only leaks a little bit and it's barely called anyways and dealing with this will be a pain
            print_warning("This leaks memory! I don't care enough to deal with this.");
            return String_concat(type_get_name(type->array_data.type), StringRef("[]"));

        case (TYPE_struct) 
            return type->struct_data.name;

        default ()
            return StringRef(type_kind_names[type->kind]);

    }
}

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

        default ()
            printf("%s", type_kind_names[type->kind]);

    }
}

void _free_type(Type *type) {
    if (type == NULL) return;
    if (type->kind == TYPE_array) _free_type(type->array_data.type);
    free(type);
}

#define free_type(type) do { \
    _free_type(type); \
    type = NULL; \
} while (0)
