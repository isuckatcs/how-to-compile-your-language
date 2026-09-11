# Your Language

Programming languages are usually created with a specific purpose in mind.

In the 1970s, C was created as a systems programming language for Unix, addressing many of the limitations of the B language. Later, C++ extended C with classes and other features to make it easier to build and organize large programs without giving up the performance of a systems language. More recently, Rust focused on addressing the memory-safety problems that have historically affected C and C++

Portability has also been a motivation for creating new languages. Java was designed around the idea of running the same program across different platforms without requiring developers to deal with platform-specific details. Kotlin aimed to improve productivity when working with the JVM while remaining interoperable with Java. Swift served a similar purpose in Apple's ecosystem, providing a modern alternative to Objective-C.

The latest language in our list, Mojo, focuses on the demands of the AI era. It aims to provide a unified programming model across increasingly heterogeneous hardware such as CPUs, GPUs, and custom accelerators, while remaining interoperable with Python, the language most widely used for AI-related work.

_Your Language_ has a simpler goal. It is a small language designed to showcase features found in modern compilers, so you can learn how they work behind the scenes.

## Clean Syntax

The syntax of _Your Language_ draws inspiration from C, Kotlin, Rust, and Swift.

```
fn main() {
  println(123);
}
```

We’ll see how a language’s syntax affects both how easy it is to read and how difficult it is to compile. Along the way, we’ll compare how different languages tackle the same problems at the syntax level and examine the trade-offs behind each approach.

## Native Code Generation

_Your Language_ uses LLVM as its backend for generating native executables. This is the same approach used by production compilers such as Rust, Swift, Mojo, and Kotlin/Native. LLVM even includes its own C, C++, and Objective-C compilers.

```
$ compiler main.yl -o main
$ ./main
123
```

Generating native code also introduces a new class of problems. Interacting with system libraries, following platform-specific calling conventions, and correctly passing values to functions.

In _Your Language_, function parameters are passed by value by default, which becomes particularly interesting when those values are structs, functions, or trait objects.

## Static Type System

Types are checked at compile time rather than at runtime.

```
fn square(n: number): number {
  let result: number = n * n;

  return result;
}
```

Invalid operations, such as multiplying a `number` by a `bool`, are detected during compilation, eliminating the need for runtime type checks.

Static typing also enables type inference, allowing the compiler to determine types where they can be inferred from context.

## Structs

User-defined `struct`s group related data into a single type. Extension functions can be used to associate behavior with them.

```
struct Vec3 {
  x: number,
  y: number,
  z: number,
}

extension Vec3 {
  fn dot(self: &Self, other: &Self): number {
    return self.x * other.x + self.y * other.y + self.z * other.z;
  }
}

fn main() {
  let v: Vec3 = Vec3 { x: 0, y: 1, z: 2 };

  println(v.dot(v));
}
```

The syntax ensures that `struct`s are fully initialized when instantiated. The language also favors composition over inheritance, so `struct`s cannot inherit from one another. This avoids complexities such as multiple inheritance and object slicing.

## Generics

Generic constructs allow the same logic to be reused with different types. Combined with type inference, they provide a smooth programming experience.

```
struct S<T> {
  t: T
}

fn id<T>(s: S<T>): S<T> {
  return s;
}

fn main() {
  id(S { t: unit });
  id(S { t: true });
  id(S { t: 1234 });
}
```

We’ll explore how compilers can implement generics, including type erasure and monomorphisation. We’ll also examine variance and its role in type safety.

## Type Inference

Generic type parameters don't usually need to be written explicitly because the compiler can infer them from how a generic function is used.

```
struct S {}

extension S {
  fn generic<T>(t: T) {}
}

fn main() {
  let f = S::generic;
  f(123);
}
```

While type inference makes generic code more concise, it also has limitations, particularly when features such as function overloading or inheritance are involved.

## Traits

Although _Your Language_ doesn't support inheritance, it provides a way to share behavior through traits.

```
trait Printable {
  fn print(self: &Self);
}

extension number : Printable {
  fn print(self: &Self) {
    println(self);
  }
}

fn consumePrintable<T : Printable>(t: T) {
  t.print();
}

fn main() {
  consumePrintable(123);
}
```

Traits can also constrain generic parameters, giving us compile-time polymorphism. We'll see how other languages approach the same idea, how trait conflicts are handled, and how a solver can enforce trait constraints.

## Virtual Dispatch

Through trait objects, we'll see how to call methods when the receiver's concrete type isn't known at compile time.

```
trait Printable {
  fn print(self: &Self);
}

fn consumeAnyPrintable(p: &any Printable) {
  p.print();
}
```

_Your Language_ implements trait objects using vtables, but we'll also explore an alternative approach called dictionary passing.

## Garbage Collection

In _Your Language_, values are allocated on the stack by default. To support heap allocation, the language uses a tracing garbage collector to manage allocated memory.

```
struct Box<T> {
  tPtr: *T
}

fn main() {
  let b = Box { tPtr: gc 123 };
}
```

The `gc` keyword allocates the following expression on the heap. We'll look at the two common approaches to automatic memory management, reference counting and tracing garbage collection, and see why tracing collectors handle cyclic references better.

## Heap-Allocated Closures

In _Your Language_, closures are allocated on the heap, allowing lambdas and functions to be used interchangeably.

```
fn returnLambda(): (number) -> number {
  let rhs = 3;
  return ->(lhs) { return lhs + rhs; };
}

fn takeFunction(f: (number) -> number): number {
  return f(0);
}

fn main() {
  takeFunction(returnLambda());
}
```

We'll explore how other languages solve the same problem and compare the trade-offs of each approach. 

## Data-Flow Analysis

_Your Language_ doesn't allow reading from uninitialized variables. The compiler uses dataflow analysis to ensure that every variable is initialized before it is read.

```
fn definiteInitialization(n: number) {
  let uninit;

  if n > 3 {
    uninit = n;
  }

  println(uninit);
}
```

Dataflow analysis is a common static analysis technique for checking whether an invariant holds along every execution path.

## Mutability

Mutability is explicit in _Your Language_ for both stack and heap allocated values. An immutable storage cannot be mutated, regardless of how it is accessed.

```
fn main() {
  let immutableStackValue: number = 0;
  mut mutableStackValue: number = 0;

  let immutableHeapValue: *number = gc 0;
  let mutableHeapValue: *mut number = gc mut 0;

  // immutableStackValue = 1
  mutableStackValue = 1

  // *immutableHeapValue = 1
  *mutableHeapValue = 1
}
```

Immutable pointers `*` and references `&` cannot be converted to their mutable counterparts `*mut` and `&mut`. Mutable pointers and references can be safely promoted to their immutable counterparts.

## Compile-Time Expression Evaluation

While this book focuses on native targets, we'll also explore how virtual machines can evaluate expressions at compile time.

```
fn main() {
  let x = (1 + 2) * 3 - -4;
}
```

_Your Language_ uses a tree-walk interpreter for this, but we'll also discuss stack-based virtual machines.

## Intermediate Representations

In _Your Language_, every intermediate representation used by the compiler can be visualized. We'll explore the role of each IR and see how structured and control-flow-based representations influence code generation for different targets.
