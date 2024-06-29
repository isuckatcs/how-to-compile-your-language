// RUN: compiler %s -cfg-dump 2>&1 | filecheck %s
fn main(): void {
    5.0;
    while 4.0 {
        3.0;
    }

    1.0;
}
// CHECK: main
// CHECK-NEXT: ----------
// CHECK-NEXT: [6 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 5 
// CHECK-NEXT: 
// CHECK-NEXT: [5]
// CHECK-NEXT:   preds: 6 
// CHECK-NEXT:   succs: 4 
// CHECK-NEXT:   NumberLiteral: '5'
// CHECK-NEXT:   | value: 5
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 2 5 
// CHECK-NEXT:   succs: 1 3 
// CHECK-NEXT:   NumberLiteral: '4'
// CHECK-NEXT:   | value: 4
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 2 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3 
// CHECK-NEXT:   succs: 4 
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs: 

fn orCondition(): void {
    7.0;
    while 6.0 || 5.0 || 4.0 {
        3.0;
    }

    1.0;
}
// CHECK: orCondition
// CHECK-NEXT: ----------
// CHECK-NEXT: [8 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 7 
// CHECK-NEXT: 
// CHECK-NEXT: [7]
// CHECK-NEXT:   preds: 8 
// CHECK-NEXT:   succs: 6 
// CHECK-NEXT:   NumberLiteral: '7'
// CHECK-NEXT:   | value: 7
// CHECK-NEXT: 
// CHECK-NEXT: [6]
// CHECK-NEXT:   preds: 2 7 
// CHECK-NEXT:   succs: 3 5 
// CHECK-NEXT:   NumberLiteral: '6'
// CHECK-NEXT:   | value: 6
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '6'
// CHECK-NEXT:     | value: 6
// CHECK-NEXT:     NumberLiteral: '5'
// CHECK-NEXT:     | value: 5
// CHECK-NEXT: 
// CHECK-NEXT: [5]
// CHECK-NEXT:   preds: 6 
// CHECK-NEXT:   succs: 3 4 
// CHECK-NEXT:   NumberLiteral: '5'
// CHECK-NEXT:   | value: 5
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     ResolvedBinaryOperator: '||'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '6'
// CHECK-NEXT:       | value: 6
// CHECK-NEXT:       NumberLiteral: '5'
// CHECK-NEXT:       | value: 5
// CHECK-NEXT:     NumberLiteral: '4'
// CHECK-NEXT:     | value: 4
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 5 
// CHECK-NEXT:   succs: 1 3 
// CHECK-NEXT:   NumberLiteral: '4'
// CHECK-NEXT:   | value: 4
// CHECK-NEXT:   ResolvedWhileStmt
// CHECK-NEXT:     ResolvedBinaryOperator: '||'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       ResolvedBinaryOperator: '||'
// CHECK-NEXT:       | value: 1
// CHECK-NEXT:         NumberLiteral: '6'
// CHECK-NEXT:         | value: 6
// CHECK-NEXT:         NumberLiteral: '5'
// CHECK-NEXT:         | value: 5
// CHECK-NEXT:       NumberLiteral: '4'
// CHECK-NEXT:       | value: 4
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       NumberLiteral: '3'
// CHECK-NEXT:       | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 5 6 
// CHECK-NEXT:   succs: 2 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3 
// CHECK-NEXT:   succs: 6
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs: 

fn andCondition(): void {
    7.0;
    while 6.0 && 5.0 && 4.0 {
        3.0;
    }

    1.0;
}
// CHECK: andCondition
// CHECK-NEXT: ----------
// CHECK-NEXT: [8 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 7 
// CHECK-NEXT: 
// CHECK-NEXT: [7]
// CHECK-NEXT:   preds: 8 
// CHECK-NEXT:   succs: 6 
// CHECK-NEXT:   NumberLiteral: '7'
// CHECK-NEXT:   | value: 7
// CHECK-NEXT: 
// CHECK-NEXT: [6]
// CHECK-NEXT:   preds: 2 7 
// CHECK-NEXT:   succs: 1 5 
// CHECK-NEXT:   NumberLiteral: '6'
// CHECK-NEXT:   | value: 6
// CHECK-NEXT:   ResolvedBinaryOperator: '&&'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '6'
// CHECK-NEXT:     | value: 6
// CHECK-NEXT:     NumberLiteral: '5'
// CHECK-NEXT:     | value: 5
// CHECK-NEXT: 
// CHECK-NEXT: [5]
// CHECK-NEXT:   preds: 6 
// CHECK-NEXT:   succs: 1 4 
// CHECK-NEXT:   NumberLiteral: '5'
// CHECK-NEXT:   | value: 5
// CHECK-NEXT:   ResolvedBinaryOperator: '&&'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     ResolvedBinaryOperator: '&&'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '6'
// CHECK-NEXT:       | value: 6
// CHECK-NEXT:       NumberLiteral: '5'
// CHECK-NEXT:       | value: 5
// CHECK-NEXT:     NumberLiteral: '4'
// CHECK-NEXT:     | value: 4
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 5 
// CHECK-NEXT:   succs: 1 3 
// CHECK-NEXT:   NumberLiteral: '4'
// CHECK-NEXT:   | value: 4
// CHECK-NEXT:   ResolvedWhileStmt
// CHECK-NEXT:     ResolvedBinaryOperator: '&&'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       ResolvedBinaryOperator: '&&'
// CHECK-NEXT:       | value: 1
// CHECK-NEXT:         NumberLiteral: '6'
// CHECK-NEXT:         | value: 6
// CHECK-NEXT:         NumberLiteral: '5'
// CHECK-NEXT:         | value: 5
// CHECK-NEXT:       NumberLiteral: '4'
// CHECK-NEXT:       | value: 4
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       NumberLiteral: '3'
// CHECK-NEXT:       | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 2 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3 
// CHECK-NEXT:   succs: 6
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 4 5 6 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs: 

fn nestedLoops(): void {
    8.0;
    while 7.0 {
        6.0;
        while 5.0 {
            4.0;
        }
    }

    1.0;
}
// CHECK: nestedLoops
// CHECK-NEXT: ----------
// CHECK-NEXT: [9 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 8 
// CHECK-NEXT: 
// CHECK-NEXT: [8]
// CHECK-NEXT:   preds: 9 
// CHECK-NEXT:   succs: 7 
// CHECK-NEXT:   NumberLiteral: '8'
// CHECK-NEXT:   | value: 8
// CHECK-NEXT: 
// CHECK-NEXT: [7]
// CHECK-NEXT:   preds: 2 8 
// CHECK-NEXT:   succs: 1 6 
// CHECK-NEXT:   NumberLiteral: '7'
// CHECK-NEXT:   | value: 7
// CHECK-NEXT: 
// CHECK-NEXT: [6]
// CHECK-NEXT:   preds: 7 
// CHECK-NEXT:   succs: 5 
// CHECK-NEXT:   NumberLiteral: '6'
// CHECK-NEXT:   | value: 6
// CHECK-NEXT: 
// CHECK-NEXT: [5]
// CHECK-NEXT:   preds: 3 6 
// CHECK-NEXT:   succs: 2 4 
// CHECK-NEXT:   NumberLiteral: '5'
// CHECK-NEXT:   | value: 5
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 5 
// CHECK-NEXT:   succs: 3 
// CHECK-NEXT:   NumberLiteral: '4'
// CHECK-NEXT:   | value: 4
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 5 
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 5 
// CHECK-NEXT:   succs: 7 
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 7 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 
// CHECK-NEXT:   succs: 

fn returnMidloop(): void {
    8.0;
    while 7.0 {
        6.0;
        if 6.0 {
            5.0;
            return;

            4.0;
        }

        3.0;
    }

    1.0;
}
// CHECK: returnMidloop
// CHECK-NEXT: ----------
// CHECK-NEXT: [9 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 8 
// CHECK-NEXT: 
// CHECK-NEXT: [8]
// CHECK-NEXT:   preds: 9 
// CHECK-NEXT:   succs: 7 
// CHECK-NEXT:   NumberLiteral: '8'
// CHECK-NEXT:   | value: 8
// CHECK-NEXT: 
// CHECK-NEXT: [7]
// CHECK-NEXT:   preds: 2 8 
// CHECK-NEXT:   succs: 1 6 
// CHECK-NEXT:   NumberLiteral: '7'
// CHECK-NEXT:   | value: 7
// CHECK-NEXT: 
// CHECK-NEXT: [6]
// CHECK-NEXT:   preds: 7 
// CHECK-NEXT:   succs: 3 5 
// CHECK-NEXT:   NumberLiteral: '6'
// CHECK-NEXT:   | value: 6
// CHECK-NEXT:   NumberLiteral: '6'
// CHECK-NEXT:   | value: 6
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     NumberLiteral: '6'
// CHECK-NEXT:     | value: 6
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       NumberLiteral: '5'
// CHECK-NEXT:       | value: 5
// CHECK-NEXT:       ResolvedReturnStmt
// CHECK-NEXT:       NumberLiteral: '4'
// CHECK-NEXT:       | value: 4
// CHECK-NEXT: 
// CHECK-NEXT: [5]
// CHECK-NEXT:   preds: 6 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '5'
// CHECK-NEXT:   | value: 5
// CHECK-NEXT:   ResolvedReturnStmt
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 3 
// CHECK-NEXT:   NumberLiteral: '4'
// CHECK-NEXT:   | value: 4
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 6 
// CHECK-NEXT:   succs: 2 
// CHECK-NEXT:   NumberLiteral: '3'
// CHECK-NEXT:   | value: 3
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 3 
// CHECK-NEXT:   succs: 7 
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 7 
// CHECK-NEXT:   succs: 0 
// CHECK-NEXT:   NumberLiteral: '1'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 1 5 
// CHECK-NEXT:   succs: 
