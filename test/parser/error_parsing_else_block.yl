// RUN: compiler %s -ast-dump 2>&1 | filecheck %s
fn main(): void {
    if 0.0 {}
    else {
// CHECK: [[# @LINE + 2 ]]:1: error: expected '}' at the end of a block
// CHECK: [[# @LINE + 1 ]]:1: error: expected '}' at the end of a block
