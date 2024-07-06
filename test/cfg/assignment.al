// RUN: compiler %s -cfg-dump 2>&1 | filecheck %s
fn main(): void {
    var x: number;

    x = 2.0;

    x = 3.0;

    x = x + 1.0;
}
// CHECK: main:
// CHECK-NEXT: [2 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   ResolvedDeclStmt:
// CHECK-NEXT:     ResolvedVarDecl: @({{.*}}) x:
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT:   ResolvedAssignment:
// CHECK-NEXT:     ResolvedDeclRefExpr: @({{.*}}) x
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT:   ResolvedAssignment:
// CHECK-NEXT:     ResolvedDeclRefExpr: @({{.*}}) x
// CHECK-NEXT:     NumberLiteral: '3'
// CHECK-NEXT:     | value: 3
// CHECK-NEXT:   ResolvedDeclRefExpr: @({{.*}}) x
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:   ResolvedBinaryOperator: '+'
// CHECK-NEXT:     ResolvedDeclRefExpr: @({{.*}}) x
// CHECK-NEXT:     NumberLiteral: '1'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:   ResolvedAssignment:
// CHECK-NEXT:     ResolvedDeclRefExpr: @({{.*}}) x
// CHECK-NEXT:     ResolvedBinaryOperator: '+'
// CHECK-NEXT:       ResolvedDeclRefExpr: @({{.*}}) x
// CHECK-NEXT:       NumberLiteral: '1'
// CHECK-NEXT:       | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs:
