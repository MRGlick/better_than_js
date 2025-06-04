# Yet another high level, statically typed and (currently) interpreted programming language

## Description

This is a personal project of mine - a stack-based interpreter for a custom programming language, written entirely in C.
This project is mainly for learning about how compilers and interpreters work. I haven't followed any specific tutorial or book, so I mainly figured everything out with research (also no AI codegen!)

## Current Language Features

The language currently supports:

* Basic data types: `int`, `float`, `bool` (which can be `true`, `false` or `maybe`, obviously.), `string` (no `char` yet)
* Variable declaration and assignment (`=`, `+=`, `-=`, etc.) as well as implicit type coercion
* Control flow: `if`/`else`, `while`, `for`
* Functions: Declaration and calls similar to C or Java, with the only difference being support for one line functions (e.g. `int square(int a) return a * a`).
* Structs: Definition (`struct Foo {int x; float y;}`), heap allocation (`Foo f = new Foo(x = 2); // unmentioned members are default initialized (y = 0.0)`), attribute access (`print f.x;`) automatic deallocation via Reference Counting
* Arrays: Support for heap allocated arrays with type inference for literals. Initialization: `int[] arr = [1, 2, 3]` access: `int val = arr[0] * arr[1];`
* I/O: `print`, `write`, `input`.
* `defer` for deferred execution of statements (scuffed).
* Standard arithmetic, relational, and logical expressions and operations.
* Constant folding optimization during compilation.
* Runtime memory leak detection.

## Benchmarks:

Will update this in the future.
Took ~2.5s to check whether 78,953,803 is prime (which, the same unoptimized code ran on python took ~3.5s)

## Future Goals

* **Explicit Type coercion** Self explanatory.
* **Better error reporting** Errors are pretty cool right now, but they could be better, especially non-parsing errors.
* **File Support:** Allow programs to read from and write to files.
* **Generic Types:** I might add them later on.
* **Graphics Support:** Add capabilities for graphical output.
* **Input Handling:** Allow real time input handling (mainly for graphical applications).
* **Web support** (Maybe) compile the language to WASM (if it's not on the web, does it even exist?).
* **Compilation:** Potentially compile the language to x86_64 Assembly.
* **Garbage Collection:** Automatic Reference Counting. ([Cyclic References](https://en.wikipedia.org/wiki/Reference_counting#Advantages_and_disadvantages)? What are those?)

## How to Build and Run

1.  **Prerequisites:** Any C compiler.
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
