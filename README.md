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
* Arrays: Support for heap allocated arrays with type inference for literals. Initialization: `int[] arr = [1, 2, 3]` `int[][] board = [8; 8]; ` access: `int val = arr[0] * arr[1];`
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

# mandlebrot4.fanta

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


int iters_to_escape_mandlebrot(Complex c, int max_iters) {

    float LENGTH_THRESHOLD = 2;

    # z(0) = 0
    # z(n) = z(n-1)^2 + c
    Complex result = c_new(0, 0);

    for (int i = 0; i < max_iters; i += 1; ) {
        c_mul_inplace(result, result);
        c_add_inplace(result, c);
        float l = length_squared(result);
        if (l > LENGTH_THRESHOLD * LENGTH_THRESHOLD) return i + 1;
    }

    return 99999;
}

int[][] get_mandlebrot_points(Complex top_left, Complex size, int width, int height, int max_iters) {
    
    int[][] screen = [height; width];

    Complex coords;

    for (int r = 0; r < height; r += 1; ) {
        for (int c = 0; c < width; c += 1; ) {
            coords = c_new(size.real / width * c, -size.imag / height * r);
            c_add_inplace(coords, top_left);

            screen[r][c] = iters_to_escape_mandlebrot(coords, max_iters);
        }
    }

    return screen;
}
str NUM_ITERS_TO_CHAR = ":^!?~Y7JP5G&&BB##@@ ";

char[] mandlebrot_points_to_string(int[][] points) {

    int width = points[0].length * 4; # Triple character plus newline
    int height = points.length;

    char[] res = [width * height + 1; ];
    int res_idx = 0; # Too lazy

    for (int r = 0; r < points.length; r += 1; ) {
        for (int c = 0; c < points[r].length; c += 1; ) {
            int idx = points[r][c];
            if (idx > NUM_ITERS_TO_CHAR.length) idx = NUM_ITERS_TO_CHAR.length;
            char ch = NUM_ITERS_TO_CHAR[idx - 1];
            
            for (int i = 0; i < 3; i += 1; ) {
                res[res_idx] = ch;
                res_idx += 1;
            }
        }
        res[res_idx] = 10; # newline
        res_idx += 1;
    }

    return res;
}

while (true) {
    int iters;
    write "Enter number of iterations: ";
    input iters;

    int size = 400;

    char[] res = mandlebrot_points_to_string(
        get_mandlebrot_points(
            c_new(-1.75, 1.5),
            c_new(2.5, 2.5),
            size,
            size,
            iters
        )
    );

    print res;
}
```
Output (Rendered in ~20s, inputted 400 iterations bc at this low resolution there isn't much difference): 

![{440CDBEA-A0FC-44E0-B87B-2A577C948C0F}](https://github.com/user-attachments/assets/e23ede6c-b931-4b8a-91ea-f3503c126e49)





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
