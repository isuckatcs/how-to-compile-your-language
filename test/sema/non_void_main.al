// RUN: compiler %s 2>&1 | filecheck %s
// CHECK: [[# @LINE + 1 ]]:1: error: 'main' function is expected to have 'void' type
fn main(): number {}
