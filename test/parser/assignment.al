// RUN: compiler %s -ast-dump 2>&1 | filecheck %s
fn bar(): number { return 1.0; }

fn main(): void {
    // CHECK: [[# @LINE + 1 ]]:11: error: expected variable on the LHS of an assignment
    bar() = 3.0;
}
