// RUN: compiler %s -ast-dump 2>&1 | filecheck %s
fn error(): void {
    // CHECK: [[# @LINE + 1 ]]:20: error: expected expression
    let x: number =;

    let y: number = 1.0 + 2.0;
    
    // CHECK: [[# @LINE + 1 ]]:18: error: expected expression
    let z: void =;
}

// CHECK: FunctionDecl: error:void
// CHECK-NEXT:   Block
// CHECK-NEXT:    DeclStmt:
// CHECK-NEXT:      VarDecl: y:number
// CHECK-NEXT:        BinaryOperator: '+'
// CHECK-NEXT:          NumberLiteral: '1.0'
// CHECK-NEXT:          NumberLiteral: '2.0'
