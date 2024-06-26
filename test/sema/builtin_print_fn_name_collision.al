// RUN: compiler %s 2>&1 | filecheck %s
fn main(): void {}

// CHECK: [[# @LINE + 1 ]]:1: error: redeclaration of 'print'
fn print(x: number): number {
    return 0.0;
}
