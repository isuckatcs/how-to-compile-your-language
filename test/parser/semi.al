// RUN: compiler %s -ast-dump 2>&1 | filecheck %s

fn main(): void {
    // CHECK: [[# @LINE + 3 ]]:5: error: expected expression
    // CHECK: [[# @LINE + 2 ]]:6: error: expected expression
    // CHECK: [[# @LINE + 1 ]]:7: error: expected expression
    ;;;
}
