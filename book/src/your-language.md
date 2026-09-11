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
