// RUN: compiler %s -res-dump 2>&1 | filecheck %s
fn main(): void {
    let x: number = 1.0;
    // CHECK: [[# @LINE + 1 ]]:9: error: redeclaration of 'x'
    let x: number = 2.0;
}
