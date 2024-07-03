// RUN: compiler %s -cfg-dump 2>&1 | filecheck %s
fn main(): void {
    3.0 || 2.0;
    1.0;
}
// CHECK: main
// CHECK-NEXT: ----------
// CHECK-NEXT: [4 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 3 
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 1 2(U) 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '3'
// CHECK-NEXT:     | value: 3
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3(U) 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 3 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '3'
// CHECK-NEXT:     | value: 3
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs: 

fn and(): void {
    3.0 && 2.0;
    1.0;
}
// CHECK: and
// CHECK-NEXT: ----------
// CHECK-NEXT: [4 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 3 
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 1(U) 2 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT:   ResolvedBinaryOperator: '&&'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '3'
// CHECK-NEXT:     | value: 3
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 3(U) 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   ResolvedBinaryOperator: '&&'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '3'
// CHECK-NEXT:     | value: 3
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs: 

fn multipleOr(): void {
    4.0 || 3.0 || 2.0;
    1.0;
}
// CHECK: multipleOr
// CHECK-NEXT: ----------
// CHECK-NEXT: [5 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 4 
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 5 
// CHECK-NEXT:   succs: 1 3 
// CHECK-NEXT:   NumberLiteral: '4'
// CHECK-NEXT:   | value: 4
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '4'
// CHECK-NEXT:     | value: 4
// CHECK-NEXT:     NumberLiteral: '3'
// CHECK-NEXT:     | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 1 2 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     ResolvedBinaryOperator: '||'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '4'
// CHECK-NEXT:       | value: 4
// CHECK-NEXT:       NumberLiteral: '3'
// CHECK-NEXT:       | value: 3
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 3 4 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     ResolvedBinaryOperator: '||'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '4'
// CHECK-NEXT:       | value: 4
// CHECK-NEXT:       NumberLiteral: '3'
// CHECK-NEXT:       | value: 3
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs: 

fn multipleAnd(): void {
    4.0 && 3.0 && 2.0;
    1.0;
}
// CHECK: multipleAnd
// CHECK-NEXT: ----------
// CHECK-NEXT: [5 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 4 
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 5 
// CHECK-NEXT:   succs: 1(U) 3 
// CHECK-NEXT:   NumberLiteral: '4'
// CHECK-NEXT:   | value: 4
// CHECK-NEXT:   ResolvedBinaryOperator: '&&'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '4'
// CHECK-NEXT:     | value: 4
// CHECK-NEXT:     NumberLiteral: '3'
// CHECK-NEXT:     | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 1(U) 2 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT:   ResolvedBinaryOperator: '&&'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     ResolvedBinaryOperator: '&&'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '4'
// CHECK-NEXT:       | value: 4
// CHECK-NEXT:       NumberLiteral: '3'
// CHECK-NEXT:       | value: 3
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 3(U) 4(U) 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   ResolvedBinaryOperator: '&&'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     ResolvedBinaryOperator: '&&'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '4'
// CHECK-NEXT:       | value: 4
// CHECK-NEXT:       NumberLiteral: '3'
// CHECK-NEXT:       | value: 3
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs: 

fn andOr(): void {
    4.0 && 3.0 || 2.0;
    1.0;
}
// CHECK: andOr
// CHECK-NEXT: ----------
// CHECK-NEXT: [5 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 4 
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 5 
// CHECK-NEXT:   succs: 2(U) 3 
// CHECK-NEXT:   NumberLiteral: '4'
// CHECK-NEXT:   | value: 4
// CHECK-NEXT:   ResolvedBinaryOperator: '&&'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '4'
// CHECK-NEXT:     | value: 4
// CHECK-NEXT:     NumberLiteral: '3'
// CHECK-NEXT:     | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 1 2(U) 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     ResolvedBinaryOperator: '&&'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '4'
// CHECK-NEXT:       | value: 4
// CHECK-NEXT:       NumberLiteral: '3'
// CHECK-NEXT:       | value: 3
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3(U) 4(U) 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 3 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     ResolvedBinaryOperator: '&&'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '4'
// CHECK-NEXT:       | value: 4
// CHECK-NEXT:       NumberLiteral: '3'
// CHECK-NEXT:       | value: 3
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs: 

fn orAnd(): void {
    4.0 || 3.0 && 2.0;
    1.0;
}
// CHECK: orAnd
// CHECK-NEXT: ----------
// CHECK-NEXT: [5 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 4 
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 5 
// CHECK-NEXT:   succs: 1 3(U) 
// CHECK-NEXT:   NumberLiteral: '4'
// CHECK-NEXT:   | value: 4
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '4'
// CHECK-NEXT:     | value: 4
// CHECK-NEXT:     ResolvedBinaryOperator: '&&'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '3'
// CHECK-NEXT:       | value: 3
// CHECK-NEXT:       NumberLiteral: '2'
// CHECK-NEXT:       | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4(U) 
// CHECK-NEXT:   succs: 1(U) 2 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT:   ResolvedBinaryOperator: '&&'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '3'
// CHECK-NEXT:     | value: 3
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 3(U) 4 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '4'
// CHECK-NEXT:     | value: 4
// CHECK-NEXT:     ResolvedBinaryOperator: '&&'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '3'
// CHECK-NEXT:       | value: 3
// CHECK-NEXT:       NumberLiteral: '2'
// CHECK-NEXT:       | value: 2
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs: 
