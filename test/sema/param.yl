// RUN: compiler %s -res-dump 2>&1 | filecheck %s

// CHECK: [[# @LINE + 1 ]]:8: error: parameter 'x' has invalid 'void' type
fn foo(x: void, y: customType): void {}

// CHECK: [[# @LINE + 1 ]]:19: error: parameter 'y' has invalid 'customType' type
fn bar(x: number, y: customType): void {}

fn main(): void {}
