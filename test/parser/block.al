// RUN: compiler %s -ast-dump 2>&1 | filecheck %s

// CHECK: [[# @LINE + 8 ]]:1: error: expected '}' at the end of a block
// CHECK: [[# @LINE + 8 ]]:1: error: expected '}' at the end of a block

fn foo(): void {}
// CHECK: FunctionDecl: foo:void
// CHECK-NEXT:   Block

fn bar(): void {
fn main(): void {
