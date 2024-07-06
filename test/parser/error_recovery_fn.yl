// RUN: compiler %s -ast-dump 2>&1 | filecheck %s
// CHECK: [[# @LINE + 1 ]]:13: error: expected type specifier
fn error(): {
    let y: number = 1.0 + 2.0;
}

fn main(): void {
    let y: number = 1.0 + 2.0;
}

// CHECK: [[# @LINE + 1 ]]:12: error: expected parameter declaration
fn error2( {
    let y: number = 1.0 + 2.0;
}

fn error3(): void {
    let y: number =

// CHECK: [[# @LINE + 2 ]]:1: error: expected expression
// CHECK: [[# @LINE + 1 ]]:1: error: expected '}' at the end of a block
fn pass(): number {
    let y: number = 1.0 + 2.0;
}

// CHECK: FunctionDecl: main:void
// CHECK-NEXT:   Block
// CHECK-NEXT:     DeclStmt:
// CHECK-NEXT:       VarDecl: y:number
// CHECK-NEXT:         BinaryOperator: '+'
// CHECK-NEXT:           NumberLiteral: '1.0'
// CHECK-NEXT:           NumberLiteral: '2.0'
// CHECK-NEXT: FunctionDecl: pass:number
// CHECK-NEXT:   Block
// CHECK-NEXT:     DeclStmt:
// CHECK-NEXT:       VarDecl: y:number
// CHECK-NEXT:         BinaryOperator: '+'
// CHECK-NEXT:           NumberLiteral: '1.0'
// CHECK-NEXT:           NumberLiteral: '2.0'
