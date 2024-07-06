// RUN: compiler %s -ast-dump 2>&1 | filecheck %s
fn error(x: number): void {
    
    if x == 0.0 {
        if x == 2.0 {
            0.0;
            let x
        // CHECK: [[# @LINE + 1 ]]:9: error: expected ':'
        } else {
            1.0;
        }
    }

    2.0;
}

// CHECK: FunctionDecl: error:void
// CHECK-NEXT:   ParamDecl: x:number
// CHECK-NEXT:   Block
// CHECK-NEXT:     IfStmt
// CHECK-NEXT:       BinaryOperator: '=='
// CHECK-NEXT:         DeclRefExpr: x
// CHECK-NEXT:         NumberLiteral: '0.0'
// CHECK-NEXT:       Block
// CHECK-NEXT:         IfStmt
// CHECK-NEXT:           BinaryOperator: '=='
// CHECK-NEXT:             DeclRefExpr: x
// CHECK-NEXT:             NumberLiteral: '2.0
// CHECK-NEXT:           Block
// CHECK-NEXT:             NumberLiteral: '0.0'
// CHECK-NEXT:           Block
// CHECK-NEXT:             NumberLiteral: '1.0'
// CHECK-NEXT:     NumberLiteral: '2.0'
