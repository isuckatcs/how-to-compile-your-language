// RUN: compiler %s -ast-dump 2>&1 | filecheck %s
fn bar(): number { return 1.0; }

fn main(): void {
    // CHECK: [[# @LINE + 1 ]]:11: error: expected variable on the LHS of an assignment
    bar() = 3.0;
    
    // CHECK: [[# @LINE + 1 ]]:11: error: expected ';' at the end of statement
    a = b = 4.0;

    a = 4.0;
    // CHECK: Assignment:
    // CHECK-NEXT:   DeclRefExpr: a
    // CHECK-NEXT:   NumberLiteral: '4.0'
}
