// RUN: compiler %s -ast-dump 2>&1 | filecheck %s
fn error(x: number): void {
    
    // CHECK: [[# @LINE + 1 ]]:13: error: expected expression
    if x == {
        if x == 2.0 {

        } else {

        }
    // CHECK: [[# @LINE + 1 ]]:7: error: expected expression
    } else {
        1.0;
    }

    2.0;
}

// CHECK: FunctionDecl: error:void
// CHECK-NEXT:   ParamDecl: x:number
// CHECK-NEXT:   Block
// CHECK-NEXT:     NumberLiteral: '2.0'
