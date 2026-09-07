# Introduction

Welcome to the guide for the [How to Compiler Your Language](https://github.com/isuckatcs/how-to-compile-your-language) repository. This book guides you through building a compiler for _Your Language_, a small programming language designed to explore how modern compilers work.

The following chapters cover both the necessary theory and its practical implementation. You can follow along and write every line of the compiler yourself.

## Why learn about compilers?

Compilers are complex systems that solve problems across many different domains, including programming languages and type theory, computer architecture, graph theory, static analysis, and more. Implementing a compiler is a great exercise for improving your software design skills and gaining a better understanding of how the tools that software engineers use every day work.

Many of the problems you will encounter are open-ended. There is often no single best solution, instead, there are multiple approaches, each with its own trade-offs. Language design involves constantly balancing these trade-offs, and learning how to make these decisions can be useful in both research and product-oriented work.

## What you will learn

_Your Language_ and its compiler are heavily influenced by modern production languages such as C++, Kotlin, Rust, and Swift. You will learn how features found in these languages are implemented behind the scenes. You will also learn how their compilers generate native executables with the help of LLVM.

The first chapter will help you understand how source code is translated into a native executable. It covers the core compilation pipeline and explains the role of each step.

The second chapter builds on this knowledge and explores how compiler architecture and language design influence each other. To understand why the syntax of C++ and Rust, or the generics of C++ and Java, are so different, it is first essential to understand how a compiler works.

The third chapter focuses on advanced topics that require specialized handling, including memory management, determining which function to call at runtime, and the challenges that lambdas pose when compiling to a native target.

## Ready to get started?

If any of the above caught your interest, let's proceed to the next section, where you will take the first steps on your compiler-building journey.
