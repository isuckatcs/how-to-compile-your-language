// RUN: compiler %s -res-dump 2>&1 | filecheck %s
fn pass(): number {
    let x: number = 2.0;
    return x * 10.0;
}
// CHECK:     ResolvedReturnStmt
// CHECK-NEXT:       ResolvedBinaryOperator: '*'
// CHECK-NEXT:       | value: 20
// CHECK-NEXT:         ResolvedDeclRefExpr: @({{.*}}) x
// CHECK-NEXT:         | value: 2
// CHECK-NEXT:         NumberLiteral: '10'
// CHECK-NEXT:         | value: 10

fn err(): number {
    let x: number;
    x = 2.0;
    return x * 10.0;
}
// CHECK:     ResolvedReturnStmt
// CHECK-NEXT:       ResolvedBinaryOperator: '*'
// CHECK-NEXT:         ResolvedDeclRefExpr: @({{.*}}) x
// CHECK-NEXT:         NumberLiteral: '10'
// CHECK-NEXT:         | value: 10

fn err2(): number {
    var x: number = 2.0;
    return x * 10.0;
}
// CHECK:    ResolvedReturnStmt
// CHECK-NEXT:      ResolvedBinaryOperator: '*'
// CHECK-NEXT:        ResolvedDeclRefExpr: @({{.*}}) x
// CHECK-NEXT:        NumberLiteral: '10'
// CHECK-NEXT:        | value: 10

fn main(): void {}
