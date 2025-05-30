
#include <stdlib.h>
#include <stdio.h>
#include "array.c"
#include "move.c"

Rc<Refcell<Option<String>>> b = Rc::new(Refcell::new(Some(String::from("Hello, world!"))));

void naive_append_stuff(int *arr) {
    array_append(arr, 5);
    array_append(arr, 3);
    array_append(arr, 8);
}

void naive() {
    int *arr = array(int, 5);

    append_stuff(arr); // bad, unless ownership?
}

int *ownership_append_stuff(int *arr) {
    array_append(arr, 5);
    array_append(arr, 3);
    array_append(arr, 8);
    return arr;
}

void ownership() {
    int *arr = array(int, 5);

    arr = ownership_append_stuff(move(arr)); // safe, but kinda easy to forget to call move() or return the ptr back in the function
}