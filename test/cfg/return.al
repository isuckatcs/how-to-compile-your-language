// RUN: compiler %s -cfg-dump 2>&1 | filecheck %s
fn foo(): number {
    3.0;
    return 3.0;

    2.0;
    return 2.0;

    return 1.0;
}
// CHECK: foo:
// CHECK-NEXT: [4 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 3 
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT:   ResolvedReturnStmt
// CHECK-NEXT:     NumberLiteral: '3'
// CHECK-NEXT:     | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT:   ResolvedReturnStmt
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:   ResolvedReturnStmt
// CHECK-NEXT:     NumberLiteral: '1'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 2 3 
// CHECK-NEXT:   succs: 

fn main(): void {}
