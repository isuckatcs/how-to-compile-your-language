# Introduction

Welcome to the guide for the [How to Compiler Your Language](https://github.com/isuckatcs/how-to-compile-your-language) repository. This book guides you through building a compiler for _Your Language_, a small programming language designed to explore how modern compilers work.

The following chapters cover both the necessary theory and its practical implementation. You can follow along and write every line of code yourself.

## Why learn about compilers?

Compilers are complex systems that solve problems across many interesting domains, like graphs, type theory, static analysis, or optimization. Implementing a compiler is a great exercise for improving your software design skills and gaining a better understanding of what the developer tools integrated into IDEs do to make writing code easier.

Many of the problems you will encounter are open-ended. There is often no single best solution, instead, there are multiple approaches, each with its own trade-offs. Language design involves constantly balancing these trade-offs, and learning how to make these decisions can be useful in both research and product-oriented work.

## What you will learn

_Your Language_ and its compiler are heavily influenced by modern production languages such as C++, Kotlin, Rust, and Swift. You will learn how features found in these languages are implemented behind the scenes. You will also learn how their compilers generate native executables with the help of LLVM.

The first chapter will help you understand how source code is translated into a native executable. It covers the core compilation pipeline and explains the role of each step.

The second chapter builds on this knowledge and explores how compiler architecture and language design influence each other. To understand the subtle differences in the syntax of Kotlin and Rust, or the generics of C++ and Java, it is first essential to understand how a compiler works.

The third chapter focuses on advanced topics that require specialized handling, including memory management, runtime function dispatch, and the challenges that lambdas pose when compiling to a native target.

## Ready to get started?

If any of the above caught your interest, let's proceed to the next section to learn more about the language you'll be working with for the rest of the book.
