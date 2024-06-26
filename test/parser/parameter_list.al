// RUN: compiler %s -ast-dump 2>&1 | filecheck %s
// CHECK: [[# @LINE + 1 ]]:6: error: expected parameter declaration
fn f(: void {}

// CHECK: [[# @LINE + 1 ]]:7: error: expected ':'
fn f(x): void {}

// CHECK: [[# @LINE + 1 ]]:8: error: expected type specifier
fn f(x:): void {}

// CHECK: [[# @LINE + 1 ]]:9: error: expected type specifier
fn f(x: 1.0): void {}

// CHECK: [[# @LINE + 1 ]]:15: error: expected ')'
fn f(x: number: void {}

// CHECK: [[# @LINE + 1 ]]:16: error: expected parameter declaration
fn f(x: number,): void {}

fn f(x: number, y: number): void {}
// CHECK: FunctionDecl: f:void
// CHECK-NEXT:   ParamDecl: x:number
// CHECK-NEXT:   ParamDecl: y:number
// CHECK-NEXT:   Block

fn main(): void {}
