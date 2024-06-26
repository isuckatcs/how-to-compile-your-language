// RUN: compiler %s 2>&1 | filecheck %s
fn main(): void {}

// CHECK: [[# @LINE + 1 ]]:1: error: redeclaration of 'main'
fn main(): void {}
