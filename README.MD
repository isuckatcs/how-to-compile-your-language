<div align="center">
<img src="https://github.com/isuckatcs/how-to-compile-your-language/actions/workflows/pipeline.yml/badge.svg" />
<img src="https://github.com/isuckatcs/how-to-compile-your-language/actions/workflows/deploy.yml/badge.svg" />
<a href="https://codecov.io/github/isuckatcs/how-to-compile-your-language" target="_blank"><img src="https://codecov.io/github/isuckatcs/how-to-compile-your-language/branch/main/graph/badge.svg?token=SLOD84JALB" /></a>
<br>
<br>
</div>

![cover](https://isuckatcs.github.io/how-to-compile-your-language/img/cover.png)

A simple programming language that displays modern compiler techniques for education purposes.

## Getting Started

This guide assumes that the project is being built on Linux* but equivalent steps can be performed on any other operating system.

1. Install CMake 3.20 or newer
   - `apt-get install cmake`

2. Install LLVM and Clang
   - The project was originally built for **LLVM 14.0.0**. Other versions are not guaranteed to work.
   - `apt-get install llvm clang`

3. Create a build directory and enter that directory
   - `mkdir build && cd build`

4. Configure CMake and build the project
   - `cmake path/to/repo/root && cmake --build .`

To run the tests, proceed with these following optional steps.

5. Install Python 3.10 or newer
   - `apt-get install python`

6. Install the python port of LLVM LIT and LLVM FileCheck.
   - `pip install -r ./test/requirements.txt`

7. Run the `check` target with CMake
   - `cmake --build . --target check`

\* tested on Ubuntu 22.04.3 LTS

## Features
### Clean Syntax

Unambigous, modern and easy to parse syntax with elements inspired by C, Kotlin, Rust and Swift.

```
fn main(): void {
    println(0);
}
```

### Static Type System

Simple type system currently supporting the `number` and `void` types with the capability to be expanded by user-defined types. 

The `number` type is implemented as a 64-bit floating point value.

```
fn main(): void {
    let x: number = 1234;
    let y: number = 12.34;
}
```

### Lazy Initialization

Data-flow analysis based support for detecting lazily initialized and uninitialized variables.

```
fn dataFlowAnalysis(n: number): void {
    let uninit: number;

    if n > 3 {
        uninit = 1;
    }

    println(uninit);
}
```
```
main.yl:8:13: error: 'uninit' is not initialized
```

### Naive Type Inference
When a variable is initialized, the type specifier can be omitted as the type is inferred from the initializer.

```
fn main(): void {
    let typeInference = 1; 
}
```

### Immutability

Immutable and mutable variables declared with the `let` and `var` keywords.

```
fn main(): void {
    let immutable = 1;
    var mutable = 2;

    while mutable > 2 {
      immutable = 0;
      mutable = mutable - 1;
    }
}
```
```
main.yl:6:7: error: 'immutable' cannot be mutated
```

### Compile Time Expression Evaluation

Expressions are evaluated by a tree-walk interpreter during compilation if possible.

```
fn main(): void {
    let x = (1 + 2) * 3 - -4;
    println(x);
}
```
```
$ compiler main.yl -res-dump

ResolvedFunctionDecl: @(main.addr) main:
  ResolvedBlock
    ...
    ResolvedCallExpr: @(println.addr) println
      ResolvedDeclRefExpr: @(x.addr) x
      | value: 13
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
main.yl:1:1: error: non-void function doesn't return a value on every path
```

### Native Code Generation

A source file is first compiled to LLVM IR, which is then passed to the host platform specific LLVM backend to generate a native executable.

```
fn main(): void {
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
fn main(): void {
    println(1.23);
}
```
```
$ compiler main.yl -ast-dump

FunctionDecl: main:void
  Block
    CallExpr:
      DeclRefExpr: println
      NumberLiteral: '1.23'
```
```
$ compiler main.yl -res-dump

ResolvedFunctionDecl: @(main.addr) main:
  ResolvedBlock
    ResolvedCallExpr: @(println.addr) println
      ResolvedNumberLiteral: '1.23'
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
  ResolvedNumberLiteral: '1.23'
  | value: 1.23
  ResolvedCallExpr: @(println.addr) println
    ResolvedNumberLiteral: '1.23'
    | value: 1.23

[0 (exit)]
  preds: 1 
  succs: 
```
```
$ compiler main.yl -llvm-dump

define void @__builtin_main() {
  call void @println(double 1.230000e+00)
  ret void
}
```
