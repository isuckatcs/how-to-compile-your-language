// RUN: compiler %s -cfg-dump 2>&1 | filecheck %s
fn foo(): number {
    if 0.0 {

    } else {
        return 2.0;
    }
}
// CHECK: foo
// CHECK-NEXT: ----------
// CHECK-NEXT: [3 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 2 
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3 
// CHECK-NEXT:   succs: 0(U) 1 
// CHECK-NEXT:   NumberLiteral: '0'
// CHECK-NEXT:   | value: 0
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     NumberLiteral: '0'
// CHECK-NEXT:     | value: 0
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       ResolvedReturnStmt
// CHECK-NEXT:         NumberLiteral: '2'
// CHECK-NEXT:         | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT:   ResolvedReturnStmt
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 2(U) 
// CHECK-NEXT:   succs:

fn main(): void {}
