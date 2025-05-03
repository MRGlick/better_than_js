# Yet another high level, statically typed and (currently) interpreted programming language

## Description

This is a personal project developing a stack-based interpreter for a custom programming language, written entirely in C. It currently processes code through lexing, parsing, bytecode compilation, and execution via a custom virtual machine (VM).
This project is mainly for learning about how compilers and interpreters work. I have not followed any specific tutorial or book, and I mainly figured everything out with research (also no AI codegen!)

## Current Language Features

The language currently supports:

* Basic data types: `int`, `float`, `bool` (which can be `true`, `false` or `maybe`, obviously.), `string`
* Variable declaration and assignment (`=`, `+=`, `-=`, etc.) as well as implicit type coercion.
* Control flow: `if`/`else`, `while` (didnt add `for` yet because lazy).
* Functions: Declaration and calls similar to C or Java, with the only difference being support for one line functions (e.g. `int square(int a) return a * a`).
* Structs: Definition (`struct Foo {int x; float y;}`), heap allocation (`Foo f = new Foo(x = 2); // unmentioned members are default allocated (y = 0.0)`), member access (`print f.x;`), deallocation (`delete foo;`).
* Input/Output: `print`, `write`, `input`.
* Deferred execution with `defer` (scuffed).
* Standard arithmetic, relational, and logical operators.
* Constant folding optimization during compilation.
* Basic runtime memory leak detection.

Example code:

```
float square(float a) return a * a;

void print_nums(int max) {
    int i = 0;
    while (i < max) {

        print i + 1, ": ", square(i + 1);

        i += 1;
    }
}

struct Vec2 {
    float x;
    float y;
}

float length_squared(Vec2 v) {
    return square(v.x) + square(v.y);
}

int compare(Vec2 v1, Vec2 v2) {
    float l1 = length_squared(v1);
    float l2 = length_squared(v2);
    if (l1 > l2) return 1;
    else if (l1 < l2) return 2;
    return 0;
}

Vec2 v1 = new Vec2(x = 5, y = 6);
defer delete v1;

Vec2 v2 = new Vec2(x = 4, y = 7);
defer delete v2;

int result = compare(v1, v2);

if (result == 1) print "The first vector is bigger!";
else if (result == 2) print "The second vector is bigger!";
else print "The vectors have the same magnitude!";


print_nums(10);
```

Program output:
```
The second vector is bigger!
1: 1.0
2: 4.0
3: 9.0
4: 16.0
5: 25.0
6: 36.0
7: 49.0
8: 64.0
9: 81.0
10: 100.0
No memory leaks.
Program finished successfully after 0.00 seconds!
```

## Benchmarks:

Will update this in the future.
Took ~2.5s to check whether 78,953,803 is prime (which, the same unoptimized code ran on python took ~3.5s)

## Future Goals

* **Explicit Type coercion** Self explanatory.
* **Arrays:** Introduce array data structures.
* **File Support:** Allow programs to read from and write to files.
* **Generic Types:** I might add them later on.
* **Graphics Support:** Add capabilities for graphical output.
* **Input Handling:** Allow real time input hazndling (mainly for graphical applications).
* **Web support** (Maybe) compile the language to WASM (if it's not on the web, does it even exist?).
* **Compilation:** Potentially compile the language to x86_64 Assembly.
* **Garbage Collection:** Not likely. Mostly because of a lack of skill (But if I do add it, it will be a togglable option and not [enforced](https://en.wikipedia.org/wiki/Java_(programming_language))).

## How to Build and Run

1.  **Prerequisites:** You need a C compiler (like GCC or Clang) and the standard C library. The code uses `math.h`, so you might need to link the math library.
2.  **Compilation:** Open a terminal in the project directory and run:
    ```bash
    gcc main.c -o interpreter -O3
    ```
3.  **Running:** Execute the compiled program:
    ```bash
    ./interpreter
    ```
    The program will prompt you to enter code directly or provide a file path.

## Contributing

This is currently a personal project. Suggestions and feedback are welcome!

## License

MIT License or something.
