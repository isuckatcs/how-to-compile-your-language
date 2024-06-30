// RUN: compiler %s -res-dump 2>&1 | filecheck %s
fn foo(): number {
    if 0.0 {
        return 1.0;
    }

    // CHECK: [[# @LINE + 1 ]]:5: error: expected a return value
    return;

    // CHECK: [[# @LINE + 1 ]]:16: error: unexpected return type
    return main();
}

fn bar(): number {
    // CHECK: [[# @LINE + 1 ]]:5: error: expected a return value
    return;
}

fn main(): void {
    if 1.0 {
        return;
    }

    // CHECK: [[# @LINE + 1 ]]:5: error: unexpected return value in void function
    return 1.0;
}
