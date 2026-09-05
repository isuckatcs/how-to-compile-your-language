<div align="center">
<img src="https://github.com/isuckatcs/how-to-compile-your-language/actions/workflows/pipeline.yml/badge.svg" />
<img src="https://github.com/isuckatcs/how-to-compile-your-language/actions/workflows/deploy.yml/badge.svg" />
<a href="https://codecov.io/github/isuckatcs/how-to-compile-your-language" target="_blank"><img src="https://codecov.io/github/isuckatcs/how-to-compile-your-language/branch/main/graph/badge.svg?token=SLOD84JALB" /></a>
<br>
<br>
</div>

![cover](https://isuckatcs.github.io/how-to-compile-your-language/img/cover.png)

__Build a compiler from scratch for [_Your Language_](#your-language)__, a small programming language designed to explore how modern compilers work.

### What You'll Learn

- 🚀 [Native code generation with LLVM](#-native-code-generation)
- 🧠 [Hindley–Milner type inference](#-hindley-milner-type-inference)
- 🏷️ [Traits and type extensions](#️-traits-and-type-extensions)
- ♻️ [Tracing garbage collection](#️-tracing-garbage-collection)
- 📸 [Heap-allocated closures](#-heap-allocated-closures)
- 🖨️ [Monomorphized generics](#️-monomorphized-generics)
- 🗺️ [Virtual dispatch](#️-virtual-dispatch)
- 🌊 [Data-flow analysis](#-data-flow-analysis)
- 💻 [Compile time expression evaluation](#-compile-time-expression-evaluation)
- 🔍 [Visualized internals](#-visualized-internals)

📖 [Read the Guide](https://isuckatcs.github.io/how-to-compile-your-language/), or [Get Started](#getting-started)

**🚧 The guide is currently being updated to match the main branch. 🚧**

## Your Language

A small example combining traits, type extensions, generics, closures, and type inference:

```
trait Shape {
  fn area(self: &Self): number;
}

struct Rectangle {
  width: number,
  length: number,
}

extension Rectangle : Shape {
  fn area(self: &Self): number {
    return self.width * self.length;
  }
}

fn map<T, U>(value: T, transform: (T) -> U): U {
  return transform(value);
}

fn main() {
  let r = Rectangle { width: 4.0, length: 5.0 };

  let areaSquared = map(r.area(), ->(area) { 
    return area * area;
  });

  println(areaSquared);
}
```
```
$ compiler example.yl -o example
$ ./example
400
```
## Getting Started

_Your Language_ currently only supports POSIX systems. The steps below will get the compiler up and running on Ubuntu Linux*. Equivalent commands are available for other POSIX systems as well.

1. Clone the repository:
   ```sh
   git clone https://github.com/isuckatcs/how-to-compile-your-language.git
   cd how-to-compile-your-language
   ```

2. Install CMake 3.20 or newer:
   ```sh
   sudo apt install cmake
   ```

3. Install LLVM and Clang. For Ubuntu Linux it is recommended to use the [`llvm.sh`](https://apt.llvm.org/#llvmsh) script from [apt.llvm.org](https://apt.llvm.org).
   ```sh
   wget https://apt.llvm.org/llvm.sh
   chmod +x llvm.sh
   sudo ./llvm.sh 20
   ```
   > 📝 The project was originally built for **LLVM 20.1.0**. Other versions are not guaranteed to work.

4. Create and enter the build directory:
   ```sh
   mkdir build
   cd build
   ```

5. Configure CMake and build the project:
   ```sh
   cmake ..
   cmake --build .
   ```

6. Verify that the build was successful:
   ```sh
   ./bin/compiler -h
   ```

To run the tests, install the additional dependencies and run the `check` target.

1. Install Python 3.10 or newer:
   ```sh
   sudo apt install python3
   ```

2. Install the required dependencies:
   ```sh
   python3 -m pip install -r ./test/requirements.txt
   ```

3. Run the `check` target with CMake:
   ```sh
   cmake --build . --target check
   ```

\* Tested on Ubuntu 22.04.5 LTS

## Topics Covered
### 🚀 Native Code Generation

__Run your program directly on your machine.__ The source file is first compiled to LLVM IR, which is then passed to the host-platform-specific LLVM backend to generate a native executable.

```
fn main() {
  println(123);
}
```
```
$ compiler main.yl -o main
$ ./main
123
```

### 🧠 Hindley-Milner Type Inference

__Write generic code without annotating every type.__ The compiler infers types using a Hindley–Milner style type system and Algorithm J.

```
fn map<T, U>(value: T, transform: (T) -> U): U {
  return transform(value);
}

fn boolToNumber(b: bool): number {
  if b { return 1; }

  return 0;
}

fn main() {
  println(map(true, boolToNumber));
}
```
```
$ compiler type_inference.yl -o type_inference
$ ./type_inference
1
```

### 🏷️ Traits and Type Extensions

__Extend types without modifying their definitions.__ Traits provide shared behavior, while type extensions attach implementations to existing types.

```
trait Explorable {
  fn explore(self: &Self, year: number);
}

struct Planet {}

extension Planet : Explorable {
  fn explore(self: &Self, year: number) {
    println(year);
  }
}

fn main() {
  let mars = Planet {};
  mars.explore(2148);
}
```
```
$ compiler traits_and_type_extensions.yl -o traits_and_type_extensions
$ ./traits_and_type_extensions
2148
```

### ♻️ Tracing Garbage Collection

__Allocate objects on the heap without manually managing their lifetime.__ A stop-the-world mark-and-sweep garbage collector, built on LLVM's shadow stack strategy, manages those allocations automatically.

```
struct Box<T> {
  ptr: *T
}

fn main() {
  let box = Box { ptr: gc 123 };

  gcCollect();

  println(*box.ptr);
}
```
>💡 Use the `YL_GC_DUMP` environment variable to enable logging the actions the collector performs.
```
$ compiler gc.yl -o gc
$ YL_GC_DUMP=1 ./gc
[0] alloc (123.header) data: (123.data) (8 B) {heap: 32 B, threshold: 96 B}
[0] mark (123.header) data: (123.data) (8 B) {heap: 32 B, threshold: 96 B}
[0] sweep {heap: 32 B, threshold: 96 B}
123
[1] sweep (123.header) data: (123.data) (8 B) {heap: 0 B, threshold: 96 B}
[1] sweep {heap: 0 B, threshold: 96 B}
```

<details>
<summary>Understanding the GC log format</summary>

```
[0] alloc (123.header) data: (123.data) (8 B) {heap: 32 B, threshold: 96 B}

- [0]: GC cycle, incremented after sweeping
- alloc: operation the GC performed
- (123.header): address of the allocation header
- (123.data): address of the storage returned to the program
- (8 B): size of the requested storage
- {heap: 32 B, threshold: 96 B}: total heap usage with the limit before the next cycle
```
</details>

### 📸 Heap-Allocated Closures

__Work with higher-order functions seamlessly.__ Heap-allocated closures allow functions and lambdas to be passed around interchangeably.

```
fn apply<T, U, V>(t: T, u: U, f: (T, U) -> V): V {
  return f(t, u);
}

fn main() {
  let sum = apply(1, 2, ->(a, b) { 
    return a + b; 
  });

  println(sum);
}
```
```
$ compiler heap_allocated_closures.yl -o heap_allocated_closures
$ ./heap_allocated_closures
3
```

### 🖨️ Monomorphized Generics

__See how generics become concrete at compile time.__ In _Your Language_ generics are instantiated lazily during LLVM IR generation.

```
fn generic<T>() {}

fn main() {
  generic@<unit>();
  generic@<number>();
  generic@<() -> unit>();
}
```
>💡 Use the `-mono-dump` flag to see how each instance was created.
```
$ compiler main.yl -mono-dump

(generic, [T = unit]) -> _Y7genericGuE
(generic, [T = number]) -> _Y7genericGnE
(generic, [T = () -> unit]) -> _Y7genericGFRuE
```

### 🗺️ Virtual Dispatch

__Call trait methods without knowing the concrete type.__ Associated functions on trait objects are looked up from a vtable when called.

```
trait Printable {
  fn print(self: &Self);
}

extension number : Printable {
  fn print(self: &Self) {
    println(self);
  }
}

fn virtualDispatch(p: &any Printable) {
  p.print();
}

fn main() {
  let n = 123;
  virtualDispatch(n);
}
```
```
$ compiler virtual_dispatch.yl -o virtual_dispatch
$ ./virtual_dispatch
123
```

### 🌊 Data-Flow Analysis

__Catch uninitialized variables before your program runs.__ Data-flow analysis tracks variable initialization across control-flow paths.

```
fn dataFlowAnalysis(n: number) {
  let uninit;

  if n > 3 {
    uninit = n;
  }

  println(uninit);
}

fn main() {
  dataFlowAnalysis(1);
}
```
```
$ compiler data_flow_analysis.yl
data_flow_analysis.yl:8:11: error: 'uninit' is not initialized
8 |   println(uninit);
  |           ^
```

### 💻 Compile-Time Expression Evaluation

__Implement an interpreter inside the compiler.__ Expressions are evaluated by a tree-walk interpreter during compilation if possible.

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

### 🔍 Visualized Internals

__See what the compiler sees.__ Dump the AST, resolved AST, CFG and the generated LLVM IR module on demand.
```
fn main() {
  println(123);
}
```
```
$ compiler main.yl -ast-dump

FunctionDecl 'main'
  Block
    CallExpr
      PathExpr
        DeclRefExpr 'println'
      NumberLiteral '123'
```
```
$ compiler main.yl -res-dump

FunctionDecl @(main.addr) main {() -> unit}
  Block
    CallExpr {unit}
      DeclRefExpr @(println.addr) println {(number) -> unit}
      NumberLiteral '123' {number}
      | value: 123
```
```
$ compiler main.yl -cfg-dump

fn main(...) {
bb2:
  preds: 
  succs: 1 

bb1:
  preds: 2 
  $1 = 123
  $2 = println
  $3 = $2($1)
  succs: 0 

bb0:
  preds: 1 
  succs: 
}
```
```
$ compiler main.yl -mono-dump

(println, []) -> _Y7println
(main, []) -> _Y4main
```
```
$ compiler main.yl -llvm-dump

define void @_Y4main(ptr %closure) {
entry:
  call void @_Y7println(double 1.230000e+02, ptr null)
  ret void
}
```
> 📝 The `number` type in _Your Language_ is represented as a `double`.
