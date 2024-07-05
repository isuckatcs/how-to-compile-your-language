// RUN: compiler %s -cfg-dump 2>&1 | filecheck %s
fn main(): void {
    if 0.0 {}
}
// CHECK: main
// CHECK-NEXT: ----------
// CHECK-NEXT: [2 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '0'
// CHECK-NEXT:   | value: 0
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     NumberLiteral: '0'
// CHECK-NEXT:     | value: 0
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs: 

fn body(): void {
    if 0.0 {
        1.0;
    }
}
// CHECK: body
// CHECK-NEXT: ----------
// CHECK-NEXT: [3 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 2 
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3 
// CHECK-NEXT:   succs: 0 1 
// CHECK-NEXT:   NumberLiteral: '0'
// CHECK-NEXT:   | value: 0
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     NumberLiteral: '0'
// CHECK-NEXT:     | value: 0
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       NumberLiteral: '1'
// CHECK-NEXT:       | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 2 
// CHECK-NEXT:   succs:

fn additionalBlockAfterIf(): void {
    if 0.0 {
        1.0;
    }

    2.0;
}
// CHECK: additionalBlockAfterIf
// CHECK-NEXT: ----------
// CHECK-NEXT: [4 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 3 
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 1 2 
// CHECK-NEXT:   NumberLiteral: '0'
// CHECK-NEXT:   | value: 0
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     NumberLiteral: '0'
// CHECK-NEXT:     | value: 0
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       NumberLiteral: '1'
// CHECK-NEXT:       | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 3 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs:

fn multipleBranches(): void {
    if 0.0 {
        0.0;
    } else if 1.0 {
        1.0;
    } else if 2.0 {
        2.0;
    } else {
        3.0;
    }

    4.0;
}
// CHECK: multipleBranches
// CHECK-NEXT: ----------
// CHECK-NEXT: [9 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 8 
// CHECK-NEXT: 
// CHECK-NEXT: [8]
// CHECK-NEXT:   preds: 9 
// CHECK-NEXT:   succs: 6 7(U) 
// CHECK-NEXT:   NumberLiteral: '0'
// CHECK-NEXT:   | value: 0
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     NumberLiteral: '0'
// CHECK-NEXT:     | value: 0
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       NumberLiteral: '0'
// CHECK-NEXT:       | value: 0
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       ResolvedIfStmt
// CHECK-NEXT:         NumberLiteral: '1'
// CHECK-NEXT:         | value: 1
// CHECK-NEXT:         ResolvedBlock
// CHECK-NEXT:           NumberLiteral: '1'
// CHECK-NEXT:           | value: 1
// CHECK-NEXT:         ResolvedBlock
// CHECK-NEXT:           ResolvedIfStmt
// CHECK-NEXT:             NumberLiteral: '2'
// CHECK-NEXT:             | value: 2
// CHECK-NEXT:             ResolvedBlock
// CHECK-NEXT:               NumberLiteral: '2'
// CHECK-NEXT:               | value: 2
// CHECK-NEXT:             ResolvedBlock
// CHECK-NEXT:               NumberLiteral: '3'
// CHECK-NEXT:               | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [7]
// CHECK-NEXT:   preds: 8(U) 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '0'
// CHECK-NEXT:   | value: 0
// CHECK-NEXT: 
// CHECK-NEXT: [6]
// CHECK-NEXT:   preds: 8 
// CHECK-NEXT:   succs: 4(U) 5 
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     NumberLiteral: '1'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       NumberLiteral: '1'
// CHECK-NEXT:       | value: 1
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       ResolvedIfStmt
// CHECK-NEXT:         NumberLiteral: '2'
// CHECK-NEXT:         | value: 2
// CHECK-NEXT:         ResolvedBlock
// CHECK-NEXT:           NumberLiteral: '2'
// CHECK-NEXT:           | value: 2
// CHECK-NEXT:         ResolvedBlock
// CHECK-NEXT:           NumberLiteral: '3'
// CHECK-NEXT:           | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [5]
// CHECK-NEXT:   preds: 6 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 6(U) 
// CHECK-NEXT:   succs: 2(U) 3 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       NumberLiteral: '2'
// CHECK-NEXT:       | value: 2
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       NumberLiteral: '3'
// CHECK-NEXT:       | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 4(U) 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 3 5 7 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '4'
// CHECK-NEXT:   | value: 4
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs:

fn nestedIfStatements(): void {
    if 0.0 {
        if 0.0 {
            0.0;
        } else {
            1.0;
        }
    }

    2.0;
}
// CHECK: nestedIfStatements
// CHECK-NEXT: ----------
// CHECK-NEXT: [6 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 5 
// CHECK-NEXT: 
// CHECK-NEXT: [5]
// CHECK-NEXT:   preds: 6 
// CHECK-NEXT:   succs: 1 4 
// CHECK-NEXT:   NumberLiteral: '0'
// CHECK-NEXT:   | value: 0
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     NumberLiteral: '0'
// CHECK-NEXT:     | value: 0
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       ResolvedIfStmt
// CHECK-NEXT:         NumberLiteral: '0'
// CHECK-NEXT:         | value: 0
// CHECK-NEXT:         ResolvedBlock
// CHECK-NEXT:           NumberLiteral: '0'
// CHECK-NEXT:           | value: 0
// CHECK-NEXT:         ResolvedBlock
// CHECK-NEXT:           NumberLiteral: '1'
// CHECK-NEXT:           | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 5 
// CHECK-NEXT:   succs: 2 3 
// CHECK-NEXT:   NumberLiteral: '0'
// CHECK-NEXT:   | value: 0
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     NumberLiteral: '0'
// CHECK-NEXT:     | value: 0
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       NumberLiteral: '0'
// CHECK-NEXT:       | value: 0
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       NumberLiteral: '1'
// CHECK-NEXT:       | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '0'
// CHECK-NEXT:   | value: 0
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 3 5 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs:

fn orCondition(): void {
    5.0;
    if 5.0 || 4.0 || 3.0 {
        2.0;
    }

    1.0;
}
// CHECK: orCondition
// CHECK-NEXT: ----------
// CHECK-NEXT: [6 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 5 
// CHECK-NEXT: 
// CHECK-NEXT: [5]
// CHECK-NEXT:   preds: 6 
// CHECK-NEXT:   succs: 2 4(U) 
// CHECK-NEXT:   NumberLiteral: '5'
// CHECK-NEXT:   | value: 5
// CHECK-NEXT:   NumberLiteral: '5'
// CHECK-NEXT:   | value: 5
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '5'
// CHECK-NEXT:     | value: 5
// CHECK-NEXT:     NumberLiteral: '4'
// CHECK-NEXT:     | value: 4
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 5(U) 
// CHECK-NEXT:   succs: 2 3(U) 
// CHECK-NEXT:   NumberLiteral: '4'
// CHECK-NEXT:   | value: 4
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     ResolvedBinaryOperator: '||'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '5'
// CHECK-NEXT:       | value: 5
// CHECK-NEXT:       NumberLiteral: '4'
// CHECK-NEXT:       | value: 4
// CHECK-NEXT:     NumberLiteral: '3'
// CHECK-NEXT:     | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4(U) 
// CHECK-NEXT:   succs: 1(U) 2 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     ResolvedBinaryOperator: '||'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       ResolvedBinaryOperator: '||'
// CHECK-NEXT:       | value: 1
// CHECK-NEXT:         NumberLiteral: '5'
// CHECK-NEXT:         | value: 5
// CHECK-NEXT:         NumberLiteral: '4'
// CHECK-NEXT:         | value: 4
// CHECK-NEXT:       NumberLiteral: '3'
// CHECK-NEXT:       | value: 3
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       NumberLiteral: '2'
// CHECK-NEXT:       | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3 4 5 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 3(U) 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs:

fn andCondition(): void {
    5.0;
    if 5.0 && 4.0 && 3.0 {
        2.0;
    }

    1.0;
}
// CHECK: andCondition
// CHECK-NEXT: ----------
// CHECK-NEXT: [6 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 5 
// CHECK-NEXT: 
// CHECK-NEXT: [5]
// CHECK-NEXT:   preds: 6 
// CHECK-NEXT:   succs: 1(U) 4 
// CHECK-NEXT:   NumberLiteral: '5'
// CHECK-NEXT:   | value: 5
// CHECK-NEXT:   NumberLiteral: '5'
// CHECK-NEXT:   | value: 5
// CHECK-NEXT:   ResolvedBinaryOperator: '&&'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '5'
// CHECK-NEXT:     | value: 5
// CHECK-NEXT:     NumberLiteral: '4'
// CHECK-NEXT:     | value: 4
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 5 
// CHECK-NEXT:   succs: 1(U) 3 
// CHECK-NEXT:   NumberLiteral: '4'
// CHECK-NEXT:   | value: 4
// CHECK-NEXT:   ResolvedBinaryOperator: '&&'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     ResolvedBinaryOperator: '&&'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '5'
// CHECK-NEXT:       | value: 5
// CHECK-NEXT:       NumberLiteral: '4'
// CHECK-NEXT:       | value: 4
// CHECK-NEXT:     NumberLiteral: '3'
// CHECK-NEXT:     | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 1(U) 2 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     ResolvedBinaryOperator: '&&'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       ResolvedBinaryOperator: '&&'
// CHECK-NEXT:       | value: 1
// CHECK-NEXT:         NumberLiteral: '5'
// CHECK-NEXT:         | value: 5
// CHECK-NEXT:         NumberLiteral: '4'
// CHECK-NEXT:         | value: 4
// CHECK-NEXT:       NumberLiteral: '3'
// CHECK-NEXT:       | value: 3
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       NumberLiteral: '2'
// CHECK-NEXT:       | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3 
// CHECK-NEXT:   succs: 1 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 3(U) 4(U) 5(U) 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs:
