// RUN: compiler %s -res-dump 2>&1 | filecheck %s
fn binaryLhsKnown(): number {
    var x: number = 2.1;
    let y: number;

    return (0.0 && y) + (1.0 || x);
}
// CHECK:       ResolvedBinaryOperator: '+'
// CHECK-NEXT:       | value: 1
// CHECK-NEXT:         ResolvedGroupingExpr:
// CHECK-NEXT:         | value: 0
// CHECK-NEXT:           ResolvedBinaryOperator: '&&'
// CHECK-NEXT:           | value: 0
// CHECK-NEXT:             NumberLiteral: '0'
// CHECK-NEXT:             | value: 0
// CHECK-NEXT:             ResolvedDeclRefExpr: @({{.*}}) y
// CHECK-NEXT:         ResolvedGroupingExpr:
// CHECK-NEXT:         | value: 1
// CHECK-NEXT:           ResolvedBinaryOperator: '||'
// CHECK-NEXT:           | value: 1
// CHECK-NEXT:             NumberLiteral: '1'
// CHECK-NEXT:             | value: 1
// CHECK-NEXT:             ResolvedDeclRefExpr: @({{.*}}) x

fn unaryNonConst(): number {
    var x: number = 2.1;

    return !x;
}
// CHECK:    ResolvedReturnStmt
// CHECK-NEXT:      ResolvedUnaryOperator: '!'
// CHECK-NEXT:        ResolvedDeclRefExpr: @({{.*}}) x

fn ret(): number {
    return 1.0;
}

fn call(): void {
    if !ret() {
        return;
    }
}
// CHECK:    ResolvedIfStmt
// CHECK-NEXT:      ResolvedUnaryOperator: '!'
// CHECK-NEXT:        ResolvedCallExpr: @({{.*}}) ret
// CHECK-NEXT:      ResolvedBlock
// CHECK-NEXT:        ResolvedReturnStmt

fn lhsKnownRhsNot(): number {
    let y: number;

    return 1.0 && y;
}
// CHECK:    ResolvedReturnStmt
// CHECK-NEXT:      ResolvedBinaryOperator: '&&'
// CHECK-NEXT:        NumberLiteral: '1'
// CHECK-NEXT:        | value: 1
// CHECK-NEXT:        ResolvedDeclRefExpr: @({{.*}}) y

fn main(): void {}
