// RUN: compiler %s 2>&1 | filecheck %s
// CHECK: [[# @LINE + 1 ]]:13: error: expected type specifier
fn error(): {
