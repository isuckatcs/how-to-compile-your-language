# Your Language

Programming languages are usually created with a specific purpose in mind. C was designed for systems programming, Java for portable execution across platforms, Rust for memory safety, and Kotlin and Swift for improving the experience of programming in their respective ecosystems.

_Your Language_, on the other hand, isn't designed to solve one particular programming problem. It is a small language designed to explore the techniques used to implement statically typed languages with generics, type inference, traits, closures, garbage collection, and native code generation.

Throughout the following chapters, you’ll write a compiler from scratch for _Your Language_ and see how these features are implemented.

## Clean Syntax

The syntax of _Your Language_ draws inspiration from C++, Kotlin, Rust, Swift, and Ruby.

```
fn main() {
  println(123);
}
```

At the surface, syntax affects how easy a language is to read and write. Careful syntax design can also avoid ambiguities that parsers must otherwise resolve, such as the classic dangling `else` problem.

The syntax of _Your Language_ is therefore intentionally designed to make parsing straightforward and avoid common sources of ambiguity.

## Native Code Generation

The _Your Language_ compiler uses LLVM to generate native executables, as many production compilers do.

```
$ compiler main.yl -o main
$ ./main
123
```

LLVM is also used for code generation by Rust, Swift, Mojo, Kotlin/Native, and some C and C++ compilers.

It also makes it easy for _Your Language_ to interact with system libraries and reuse existing logic to print values or allocate memory.

## Static Type System

In _Your Language_, types are checked at compile time rather than at runtime.

```
fn square(n: number): number {
  let result: number = n * n;
  return result;
}
```

_Your Language_'s static type system lets invalid operations be rejected before the program runs and allows the compiler to infer types when they are not explicitly specified.

Type inference reduces the need for repetitive type annotations and is particularly useful when working with generic types.

## Structs

User-defined `struct`s group related data into a single named type.

```
struct Vec3 {
  x: number,
  y: number,
  z: number,
}

fn main() {
  Vec3 { x: 0, y: 1, z: 2 };
}
```

A `struct` contains only data and cannot inherit from another `struct`. _Your Language_ favors composition over inheritance, avoiding complexities such as multiple inheritance and object slicing.

Instantiating a `struct` requires every field to be initialized, preventing partially initialized `struct` values.

## Type Extensions

Types can also be extended with new functionality.

```
extension Vec3 {
  fn zero(): Vec3 {
    return Vec3 { x: 0, y: 0, z: 0 };
  }

  fn dot(self: &Self, other: &Self): number {
    return self.x * other.x + self.y * other.y + self.z * other.z;
  }
}

fn main() {
  let v: Vec3 = Vec3::zero();
  v.dot(v);
}
```

Both type-level and instance-level functions can be associated with a type. Instance-level functions take an explicit `self` argument whose type must be `&Self` or `&mut Self`.

This provides a clear separation between a type's data and its functionality, while giving developers control over how associated functions are defined.

## Generics

Generic constructs allow the same logic to be used with different types.

```
struct S<T> {
  t: T
}

fn id<T>(t: T): T {
  return t;
}

fn main() {
  id@<S<unit>>(S@<unit>{ t: unit });
  id@<S<bool>>(S@<bool>{ t: true });
}
```

In _Your Language_, every generic type is invariant. Since the language does not support subtyping, covariant and contravariant generic types would not provide any practical benefit to developers. Furthermore, unlike covariance, invariant generic types cannot lead to situations where a runtime type check is still required to ensure type safety.

Generic functions are monomorphized by the compiler upon instantiation. This means that each instance of a generic function receives its own unique implementation.

## Type Inference

_Your Language_ allows generic parameters to be omitted and inferred from their context instead.

```
fn main() {
  id(S { t: unit });
  id(S { t: true });
  id(S { t: 1234 });
}
```

Inference is implemented using a Hindley–Milner-style type system and Algorithm J. 

While this makes generic code more convenient to use, it also complicates function overloading. To keep type inference predictable, _Your Language_ does not support this feature.

## Traits

Traits provide a way to share behavior without inheritance.

```
trait Printable {
  fn print(self: &Self);
}

extension number : Printable {
  fn print(self: &Self) {
    println(self);
  }
}

fn print<T : Printable>(t: T) {
  t.print();
}

fn main() {
  print(123);
}
```

Traits can require other traits as prerequisites. They can define both type-level and instance-level functions, and provide default implementations for them. Trait functions without a default implementation must be implemented when the type is extended with the trait.

When multiple traits provide functions with the same name, each extension remains separate, so the trait from which a function is dispatched must be specified explicitly.

## Virtual Dispatch

Trait objects allow non-generic functions to accept values that implement a specific trait.

```
fn consumeAnyPrintable(p: &any Printable) {
  p.print();
}
```

Since the concrete type of a trait object is not known, only `&` and `*` types can be used as trait objects. The function to dispatch is determined at runtime through a vtable lookup.

## Garbage Collection

Values in _Your Language_ are allocated on the stack by default. To support heap allocation, the language provides a runtime that manages allocated memory.

```
struct Box<T> {
  tPtr: *T
}

fn main() {
  let b = Box { tPtr: gc 123 };
}
```

Prefixing an expression with `gc` or `gc mut` allocates its result on the heap and returns a pointer to it, represented by `*` or `*mut`. A `*` type always denotes a pointer to a heap-allocated value. 

A `*mut` can be promoted to `*`, but not the other way around. If a value is allocated on the heap as immutable, it remains immutable for its entire lifetime.

The implementation of _Your Language_ uses a tracing garbage collector to manage memory. Unlike reference counting, another popular memory-management technique, a tracing garbage collector automatically handles cyclic references.

## Heap-Allocated Closures

Closures make it possible to create higher-order functions and capture values from their enclosing scopes.

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

Working with closures on native targets can be difficult, especially when the language tries to preserve their interchangeability with regular functions.

In _Your Language_, closures are allocated on the heap, and every function is represented as a closure, giving ordinary functions and capturing lambdas the same runtime representation.

## Definite Assignment

_Your Language_ guarantees that every variable is initialized before it is read.

```
fn definiteAssignment(n: number) {
  let uninit;

  if n > 3 {
    uninit = n;
  } else {
    uninit = n * 2;
  }

  println(uninit);
}
```

The compiler enforces this guarantee using dataflow analysis, a common static analysis technique for checking whether an invariant holds along every execution path.

## Mutability

Mutability is explicit in _Your Language_ for both stack- and heap-allocated values.

```
fn main() {
  let stackValue: number = 0;
  mut mutableStackValue: number = 0;

  let heapValuePointer: *number = gc 0;
  let mutableHeapValuePointer: *mut number = gc mut 0;

  // stackValue = 1;
  mutableStackValue = 1;

  // *heapValuePointer = 1;
  *mutableHeapValuePointer = 1;
}
```

An immutable storage location cannot be modified, regardless of how it is accessed. 

Mutable pointers and references, `*mut` and `&mut`, can be promoted to `*` and `&`, but immutable pointers and references cannot be used as mutable ones.

## Reference Parameters

Function parameters can be references, allowing a function to modify an existing value or avoid copying a potentially large value.

```
fn assign(lhs: &mut number, rhs: &number) {
  lhs = rhs;
}
```

By restricting references to function parameters, _Your Language_ avoids dangling references without requiring a full lifetime system. References are therefore temporary views into values supplied by the caller.

Assigning through a mutable reference modifies the referenced value. It does not change which value the reference refers to.

## Compile-Time Expression Evaluation

Expressions with compile-time-known values are evaluated during compilation and replaced with their results.

```
fn main() {
  let x = (1 + 2) * 3 - -4;
}
```

The compiler evaluates these expressions using a simple tree-walk interpreter. During code generation, the resulting value is emitted instead of the expression itself.
