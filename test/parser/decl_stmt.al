// RUN: compiler %s -ast-dump 2>&1 | filecheck %s
fn main(): void {
    // CHECK: [[# @LINE + 1 ]]:25: error: expected ';' at the end of statement
    let x: number = 0.0 |;

    // CHECK: [[# @LINE + 1 ]]:9: error: expected identifier
    var ;

    // CHECK: [[# @LINE + 1 ]]:10: error: expected ':'
    var x;

    // CHECK: [[# @LINE + 1 ]]:11: error: expected type specifier
    let x:;

    // CHECK: [[# @LINE + 1 ]]:20: error: expected expression
    let x: number =;

    let x: number;
    // CHECK: DeclStmt:
    // CHECK-NEXT:   VarDecl: x:number
    
    var x: number = 1.0;
    // CHECK: DeclStmt:
    // CHECK-NEXT:   VarDecl: x:number
    // CHECK-NEXT:     NumberLiteral: '1.0'
}
