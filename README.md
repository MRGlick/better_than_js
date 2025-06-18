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
  
## Future Goals

* **Explicit Type coercion** Self explanatory.
* **Better error reporting** Errors are pretty cool right now, but they could be better, especially non-parsing errors.
* **File Support:** Allow programs to read from and write to files.
* **Generic Types:** I might add them later on.
* **Graphics Support:** Add capabilities for graphical output.
* **Input Handling:** Allow real time input handling (mainly for graphical applications).
* **Web support** (Very unlikely) compile the language to WASM.
* **Compilation:** Potentially compile the language to x86_64 Assembly.
* **Garbage Collection:** Automatic Reference Counting. ([Cyclic References](https://en.wikipedia.org/wiki/Reference_counting#Advantages_and_disadvantages)? What are those?)

## Examples:

# mandlebrot_ultra_hd.fanta

A script to render an ascii representation of the mandlebrot set.
```

struct Complex {
    float real;
    float imag;
}

void c_write(Complex a)
    write a.real, " + ", a.imag, "i";

Complex c_new(float r, float i) return new Complex (
    real = r,
    imag = i
);

void c_add_inplace(Complex a, Complex b) {
    a.real = a.real + b.real;
    a.imag = a.imag + b.imag;
}

void c_mul_inplace(Complex a, Complex b) {
    float real = a.real * b.real - a.imag * b.imag;
    float imag = a.real * b.imag + b.real * a.imag;
    a.real = real;
    a.imag = imag;
}


float length_squared(Complex a) return a.real * a.real + a.imag * a.imag;


bool is_mandlebrot(Complex c) {

    float LENGTH_THRESHOLD = 2;
    int ITER_COUNT = 300; 

    # z(0) = 0
    # z(n) = z(n-1)^2 + c
    Complex result = c_new(0, 0);

    for (int i = 0; i < ITER_COUNT; i += 1; ) {
        c_mul_inplace(result, result);
        c_add_inplace(result, c);
        float l = length_squared(result);
        if (l > LENGTH_THRESHOLD * LENGTH_THRESHOLD) return false;
    }

    return true;
}

void test_MB(Complex a) {
    c_write(a);
    print ": ", is_mandlebrot(a);
}

char[][] render_mandlebrot(Complex top_left, Complex size, int width, int height) {
    
    # ts lang is ass
    char NEWLINE = 10;
    
    char[][] screen = [height; width + 1; ]; # width + 1 to include newline

    Complex coords;

    for (int r = 0; r < height; r += 1; ) {
        for (int c = 0; c < width; c += 1; ) {
            coords = c_new(size.real / width * c, -size.imag / height * r);
            c_add_inplace(coords, top_left);

            bool m = is_mandlebrot(coords);

            if (m) screen[r][c] = '@';
            else screen[r][c] = '.';
        }
        screen[r][width] = NEWLINE;
    }

    return screen;
}

int size = 400;

char[][] render = render_mandlebrot(
    c_new(-1.75, 1.5),
    c_new(2.5, 2.5),
    size,
    size
);

for (int i = 0; i < render.length; i += 1; ) {
    for (int j = 0; j < render[i].length; j += 1; ) {
        write render[i][j], "  ";
    }
}


```
Output (Rendered in ~20s): 

![{2139B469-72E0-4C8C-9960-47305A655E8D}](https://github.com/user-attachments/assets/8825dbf5-a55d-4986-b13a-60075c0fcd56)



## How to Build and Run

1.  **Prerequisites:** Any C compiler.
2.  **Compilation:** Open a terminal in the project directory and run:
    ```bash
    gcc main.c -o interpreter
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
