// RUN: compiler %s -res-dump 2>&1 | filecheck %s
fn foo(): void {}

fn bar(x: number, y: number): void {}

fn main(): void {
    let x: number = 1.0;

    // CHECK: [[# @LINE + 1 ]]:6: error: calling non-function symbol
    x();

    // CHECK: [[# @LINE + 1 ]]:5: error: symbol 'y' not found
    y();

    // CHECK: [[# @LINE + 1 ]]:8: error: argument count missmatch in function call
    foo(x);

    // CHECK: [[# @LINE + 1 ]]:12: error: unexpected type of argument
    bar(foo(), foo());

    // CHECK: [[# @LINE + 1 ]]:17: error: unexpected type of argument
    bar(1.0, foo());
    
    // CHECK: [[# @LINE + 1 ]]:8: error: argument count missmatch in function call
    bar();

    // CHECK: [[# @LINE + 1 ]]:8: error: argument count missmatch in function call
    bar(1.0, 2.0, 3.0);

    bar(1.0, 2.0);
    foo();
}
