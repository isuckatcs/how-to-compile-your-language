// RUN: compiler %s -res-dump 2>&1 | filecheck %s
fn foo(): number {
    let x: number = 2.1;
    let y: number = 5.3;

    return (10.0 * (x + 4.0)) && (!(y == x) || x < y);
}
// CHECK:    ResolvedReturnStmt
// CHECK-NEXT:      ResolvedBinaryOperator: '&&'
// CHECK-NEXT:      | value: 1
// CHECK-NEXT:        ResolvedGroupingExpr:
// CHECK-NEXT:        | value: 61
// CHECK-NEXT:          ResolvedBinaryOperator: '*'
// CHECK-NEXT:          | value: 61
// CHECK-NEXT:            NumberLiteral: '10'
// CHECK-NEXT:            | value: 10
// CHECK-NEXT:            ResolvedGroupingExpr:
// CHECK-NEXT:            | value: 6.1
// CHECK-NEXT:              ResolvedBinaryOperator: '+'
// CHECK-NEXT:              | value: 6.1
// CHECK-NEXT:                ResolvedDeclRefExpr: @({{.*}}) x
// CHECK-NEXT:                | value: 2.1
// CHECK-NEXT:                NumberLiteral: '4'
// CHECK-NEXT:                | value: 4
// CHECK-NEXT:        ResolvedGroupingExpr:
// CHECK-NEXT:        | value: 1
// CHECK-NEXT:          ResolvedBinaryOperator: '||'
// CHECK-NEXT:          | value: 1
// CHECK-NEXT:            ResolvedUnaryOperator: '!'
// CHECK-NEXT:            | value: 1
// CHECK-NEXT:              ResolvedGroupingExpr:
// CHECK-NEXT:              | value: 0
// CHECK-NEXT:                ResolvedBinaryOperator: '=='
// CHECK-NEXT:                | value: 0
// CHECK-NEXT:                  ResolvedDeclRefExpr: @({{.*}}) y
// CHECK-NEXT:                  | value: 5.3
// CHECK-NEXT:                  ResolvedDeclRefExpr: @({{.*}}) x
// CHECK-NEXT:                  | value: 2.1
// CHECK-NEXT:            ResolvedBinaryOperator: '<'
// CHECK-NEXT:            | value: 1
// CHECK-NEXT:              ResolvedDeclRefExpr: @({{.*}}) x
// CHECK-NEXT:              | value: 2.1
// CHECK-NEXT:              ResolvedDeclRefExpr: @({{.*}}) y
// CHECK-NEXT:              | value: 5.3

fn main(): void {}
