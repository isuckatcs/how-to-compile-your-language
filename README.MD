<div align="center">
<img src="https://github.com/isuckatcs/how-to-compile-your-language/actions/workflows/pipeline.yml/badge.svg" />
<img src="https://github.com/isuckatcs/how-to-compile-your-language/actions/workflows/deploy.yml/badge.svg" />
<a href="https://codecov.io/github/isuckatcs/how-to-compile-your-language" target="_blank"><img src="https://codecov.io/github/isuckatcs/how-to-compile-your-language/branch/main/graph/badge.svg?token=SLOD84JALB" /></a>
<br>
<br>
</div>

![cover](https://isuckatcs.github.io/how-to-compile-your-language/img/cover.png)

**🚧 The guide is currently behind the main branch. Updates are coming soon. 🚧**

A simple programming language implementation with an educational guide that displays modern compiler techniques. 

[isuckatcs.github.io/how-to-compile-your-language](https://isuckatcs.github.io/how-to-compile-your-language/)

## Getting Started

This guide assumes that the project is being built on Linux* but equivalent steps can be performed on any other operating system.

1. Install CMake 3.20 or newer
   - `apt-get install cmake`

2. Install LLVM and Clang
   - The project was originally built for **LLVM 20.1.0**. Other versions are not guaranteed to work.
   - `apt-get install llvm clang`

3. Create a build directory and enter that directory
   - `mkdir build && cd build`

4. Configure CMake and build the project
   - `cmake path/to/repo/root && cmake --build .`

To run the tests, proceed with these following optional steps.

5. Install Python 3.10 or newer
   - `apt-get install python`

6. Install the required dependencies
   - `pip install -r ./test/requirements.txt`

7. Run the `check` target with CMake
   - `cmake --build . --target check`

\* tested on Ubuntu 22.04.3 LTS

## Features
### Clean Syntax

Unambiguous, modern and easy to parse syntax with elements inspired by C, Kotlin, Rust and Swift.

```
fn main() {
  println(0);
}
```

### Hindley-Milner Style Type System

Simple Hindley-Milner style static type system currently supporting the `number`, `unit`, `function` and user-defined `struct` types with type inference capabilities. 

The `number` type is implemented as a 64-bit floating point value.

```
fn foo<T>(x: (T) -> T, y: T): T {
  return x(y);
}

fn bar<T>(x: T): T {
  return x;
}

fn main() {
  println(foo(bar, 0));
}
```

### Heap Allocated Closures

Heap allocated closures to support seamless function and lambda interchangeability.

```
fn returnCapturingLambda(): () -> number {
  let x = 123;
  return ->(){ return x; };
}

fn consumeFunction(f: () -> number) {
  println(f());
}

fn main() {
  consumeFunction(returnCapturingLambda());
}
```

### Tracing Garbage Collection

A stop-the-world mark-and-sweep garbage collector built on top of the LLVM shadow stack strategy to allow access to the heap.

```
struct S {
  ptr: *number
}

fn main() {
  mut s = gcMut(S { ptr: gc(123) });
  s.ptr = gc(456);

  gcCollect(); // manual gc trigger point

  println(*s.ptr);
}
// automatic gc trigger point
```

Defining the `YL_GC_DUMP` environment variable when running the compiled executable enables extensive garbage collector logging. This helps visualizing how the collector behaves for debugging and learning purposes.

```
$ compiler gc_example.yl -o gc_example
$ YL_GC_DUMP=1 ./gc_example
alloc @(123.header), data: (123.data) (8 B) {heap: 32 B, threshold: 96 B}
alloc @(S.header), data: (S.data) (8 B) offsets: {0 @(nil)} {heap: 64 B, threshold: 96 B}
alloc @(456.header), data: (456.data) (8 B) {heap: 96 B, threshold: 96 B}
[0] mark @(S.header), data: (S.data) (8 B) offsets: {0 @(456.data)} {heap: 96 B, threshold: 96 B}
[0] mark @(456.header), data: (456.data) (8 B) {heap: 96 B, threshold: 96 B}
[0] sweep @(123.header), data: (123.data) (8 B) {heap: 64 B, threshold: 96 B}
[0] sweep {heap: 64 B, threshold: 128 B}
456
[1] sweep @(456.header), data: (456.data) (8 B) {heap: 32 B, threshold: 128 B}
[1] sweep @(S.header), data: (S.data) (8 B) offsets: {0 @(456.data)} {heap: 0 B, threshold: 128 B}
[1] sweep {heap: 0 B, threshold: 96 B}
```

### Generics

Generic `struct`, and `function` types instantiated during LLVM IR generation with zero-sized values optimized away.

```
struct S<T, U> {
  x: T,
  y: U,
}

fn wrapper<T, U>(x: T, y: U): S<T, U> {
  return S{ x: x, y: y };
}

fn main() {
  wrapper(0, unit);
}
```

### Lazy Initialization

Data-flow analysis based support for detecting lazily initialized and uninitialized variables.

```
fn dataFlowAnalysis(n: number) {
  let uninit;

  if n > 3 {
    uninit = 1;
  }

  println(uninit);
}
```
```
main.yl:8:11: error: 'uninit' is not initialized
8 |   println(uninit);
  |           ^
```

### Smart Return Check

Flow-sensitive return value analysis made smarter with compile time expression evaluation.

```
fn maybeReturns(n: number): number {
  if n > 2 {
    return 1;
  } else if n < -2 {
    return -1;
  }

  // missing return 'else' branch
}

fn alwaysReturns(n: number): number {
  if 0 && n > 2 {
    // unreachable
  } else {
    return 0;
  }
}
```
```
main.yl:1:1: error: expected function to return a value on every path
1 | fn maybeReturns(n: number): number {
  | ^
```

### Immutability

Immutable and mutable variables declared with the `let` and `mut` keywords.

```
fn main() {
  let immutable = 1;
  mut mutable = 2;

  while mutable > 2 {
    immutable = 0;
    mutable = mutable - 1;
  }
}
```
```
main.yl:6:15: error: 'immutable' cannot be mutated
6 |     immutable = 0;
  |               ^
```

### Compile Time Expression Evaluation

Expressions are evaluated by a tree-walk interpreter during compilation if possible.

```
fn main() {
  let x = (1 + 2) * 3 - -4;
  println(x);
}
```
```
$ compiler main.yl -res-dump

FunctionDecl @(main.addr) main {() -> unit}
  Block
    ...
    CallExpr {unit}
      DeclRefExpr @(println.addr) println {(number) -> unit}
      DeclRefExpr @(x.addr) x {number}
      | value: 13
```

### Native Code Generation

A source file is first compiled to LLVM IR, which is then passed to the host platform specific LLVM backend to generate a native executable.

```
fn main() {
  println(1.23);
}
```
```
$ compiler main.yl -o main.out
$ ./main.out 
1.23
```

### Accessible Internals

Capability to print the Abstract Syntax Tree before and after resolution, the Control-Flow Graph and the generated LLVM IR module.
```
fn main() {
  println(1.23);
}
```
```
$ compiler main.yl -ast-dump

FunctionDecl: main
  Block
    CallExpr:
      DeclRefExpr: println
      NumberLiteral: '1.23'
```
```
$ compiler main.yl -res-dump

FunctionDecl @(main.addr) main {() -> unit}
  Block
    CallExpr {unit}
      DeclRefExpr @(println.addr) println {(number) -> unit}
      NumberLiteral '1.23' {number}
      | value: 1.23
```
```
$ compiler main.yl -cfg-dump

main:
[2 (entry)]
  preds: 
  succs: 1 

[1]
  preds: 2 
  succs: 0 
  1: 1.23
  2: println
  3: [1.2] ([1.1])

[0 (exit)]
  preds: 1 
  succs: 
```
```
$ compiler main.yl -llvm-dump

define void @__builtin_main() {
entry:
  call void @println(double 1.230000e+00)
  ret void
}
```
