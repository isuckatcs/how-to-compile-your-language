![example workflow](https://github.com/isuckatcs/how-to-compile-your-language/actions/workflows/pipeline.yml/badge.svg)
[![codecov](https://codecov.io/github/isuckatcs/how-to-compile-your-language/branch/main/graph/badge.svg?token=SLOD84JALB)](https://codecov.io/github/isuckatcs/how-to-compile-your-language)

# How to Compile Your Language
![cover](https://isuckatcs.github.io/how-to-compile-your-language/img/httyd.jpg)

# Modern Syntax
A clean, easy to write and parse unambigous C-style syntax resembling modern languages with elements inspired by C++, Kotlin, Rust and JavaScript.
```
fn main(): void {
    println(0);
}
```

# Static Typing

The language comes with a small static type system, currently supporting the `number` and `void` types with the capability to be expanded for user-defined types. The `number` type can be used to represent arbitrary real numbers.

When an initializer is provided for the variable, the type is automatically inferred from the initializer.
```
fn main(): void {
    let x: number;
    let y: userDefinedType;

    // number is inferred
    let typeInference = 1; 
}
```

# Mutability Support

Support for both immutable and mutable variables in the form of `let` and `var`.
```
fn main(): void {
    let immutable = 1;
    var mutable = 2;

    immutable = 3;
    mutable = 4;
}
```
```
main.yl:5:5: error: 'immutable' cannot be mutated
```

# Advanced Semantic Analyzer

A feature rich semantic analyzer with flow-sensitive checks and constant expression folding, where possible.

```
fn main(): void {
    return;
    
    let x = (1 + 2) * 3 - -4;
    println(x);
}
```
```
$ compiler main.yl -res-dump

../main.yl:4:5: warning: unreachable statement
ResolvedFunctionDecl: @(main.addr) main:
  ResolvedBlock
    ResolvedReturnStmt
    ResolvedDeclStmt:
      ...
    ResolvedCallExpr: @(println.addr) println
      ResolvedDeclRefExpr: @(x.addr) x
      | value: 13
```

# Data-Flow Analysis

The semantic analyzer is enchanced by data-flow analysis to reason about late variable initialization and mutation of immutable variables.

```
// main.yl
fn dataFlowAnalysis(n: number): void {
    let x: number;

    while n > 3 {
        x = 1;
    }

    println(x);
}
```
```
main.yl:6:9: error: 'x' cannot be mutated
main.yl:9:13: error: 'x' is not initialized
```

# Accessible Internals

Capability to print the AST before and after resolution, the CFG and the generated LLVM IR module.
```
fn main(): void {
    let x = 1.23;
    
    println(x);
}
```
```
$ compiler main.yl -ast-dump

FunctionDecl: main:void
  Block
    DeclStmt:
      VarDecl: x
        NumberLiteral: '1.23'
    CallExpr:
      DeclRefExpr: println
      DeclRefExpr: x
```
```
$ compiler main.yl -llvm-dump

define void @__builtin_main() {
  %x = alloca double, align 8
  store double 1.230000e+00, double* %x, align 8
  call void @println(double 1.230000e+00)
  ret void
}

define i32 @main() {
  call void @__builtin_main()
  ret i32 0
}
```

# Native Code Generation

The language is built on top of LLVM and compiles to native executable.

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
