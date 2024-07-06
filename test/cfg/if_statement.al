// RUN: compiler %s -cfg-dump 2>&1 | filecheck %s
fn main(): void {
    if 0.0 {}
}
// CHECK: main:
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
// CHECK: body:
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
// CHECK: additionalBlockAfterIf:
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
// CHECK: multipleBranches:
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
// CHECK: nestedIfStatements:
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
// CHECK: orCondition:
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
// CHECK: andCondition:
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

fn ifWhileFirstStmt(p: number): void {
    if p {
        while p {}
    } else {
        while p {}
    }
}
// CHECK: ifWhileFirstStmt:
// CHECK-NEXT: [6 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 5 
// CHECK-NEXT: 
// CHECK-NEXT: [5]
// CHECK-NEXT:   preds: 6 
// CHECK-NEXT:   succs: 2 4 
// CHECK-NEXT:   ResolvedDeclRefExpr: @({{.*}}) p
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     ResolvedDeclRefExpr: @({{.*}}) p
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       ResolvedWhileStmt
// CHECK-NEXT:         ResolvedDeclRefExpr: @({{.*}}) p
// CHECK-NEXT:         ResolvedBlock
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       ResolvedWhileStmt
// CHECK-NEXT:         ResolvedDeclRefExpr: @({{.*}}) p
// CHECK-NEXT:         ResolvedBlock
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 3 5 
// CHECK-NEXT:   succs: 0 3 
// CHECK-NEXT:   ResolvedDeclRefExpr: @({{.*}}) p
// CHECK-NEXT:   ResolvedWhileStmt
// CHECK-NEXT:     ResolvedDeclRefExpr: @({{.*}}) p
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 4 
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 1 5
// CHECK-NEXT:   succs: 0 1 
// CHECK-NEXT:   ResolvedDeclRefExpr: @({{.*}}) p
// CHECK-NEXT:   ResolvedWhileStmt
// CHECK-NEXT:     ResolvedDeclRefExpr: @({{.*}}) p
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 
// CHECK-NEXT:   succs: 2 
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 2 4 
// CHECK-NEXT:   succs:

fn nestedInLoopConditionalOps(): void {
    if 10.0 || 9.0 {
        while 8.0 {}
    } else if 6.0 && 5.0 {
        while 4.0 {}
    } else {
        while 2.0 {}
    }
}
// CHECK: nestedInLoopConditionalOps:
// CHECK-NEXT: [11 (entry)]
// CHECK-NEXT:   preds: 
// CHECK-NEXT:   succs: 10 
// CHECK-NEXT: 
// CHECK-NEXT: [10]
// CHECK-NEXT:   preds: 11 
// CHECK-NEXT:   succs: 8 9(U) 
// CHECK-NEXT:   NumberLiteral: '10'
// CHECK-NEXT:   | value: 10
// CHECK-NEXT:   ResolvedBinaryOperator: '||'
// CHECK-NEXT:   | value: 1
// CHECK-NEXT:     NumberLiteral: '10'
// CHECK-NEXT:     | value: 10
// CHECK-NEXT:     NumberLiteral: '9'
// CHECK-NEXT:     | value: 9
// CHECK-NEXT: 
// CHECK-NEXT: [9]
// CHECK-NEXT:   preds: 10(U) 
// CHECK-NEXT:   succs: 6(U) 8 
// CHECK-NEXT:   NumberLiteral: '9'
// CHECK-NEXT:   | value: 9
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     ResolvedBinaryOperator: '||'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '10'
// CHECK-NEXT:       | value: 10
// CHECK-NEXT:       NumberLiteral: '9'
// CHECK-NEXT:       | value: 9
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       ResolvedWhileStmt
// CHECK-NEXT:         NumberLiteral: '8'
// CHECK-NEXT:         | value: 8
// CHECK-NEXT:         ResolvedBlock
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       ResolvedIfStmt
// CHECK-NEXT:         ResolvedBinaryOperator: '&&'
// CHECK-NEXT:         | value: 1
// CHECK-NEXT:           NumberLiteral: '6'
// CHECK-NEXT:           | value: 6
// CHECK-NEXT:           NumberLiteral: '5'
// CHECK-NEXT:           | value: 5
// CHECK-NEXT:         ResolvedBlock
// CHECK-NEXT:           ResolvedWhileStmt
// CHECK-NEXT:             NumberLiteral: '4'
// CHECK-NEXT:             | value: 4
// CHECK-NEXT:             ResolvedBlock
// CHECK-NEXT:         ResolvedBlock
// CHECK-NEXT:           ResolvedWhileStmt
// CHECK-NEXT:             NumberLiteral: '2'
// CHECK-NEXT:             | value: 2
// CHECK-NEXT:             ResolvedBlock
// CHECK-NEXT: 
// CHECK-NEXT: [8]
// CHECK-NEXT:   preds: 7 9 10 
// CHECK-NEXT:   succs: 0(U) 7 
// CHECK-NEXT:   NumberLiteral: '8'
// CHECK-NEXT:   | value: 8
// CHECK-NEXT:   ResolvedWhileStmt
// CHECK-NEXT:     NumberLiteral: '8'
// CHECK-NEXT:     | value: 8
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT: 
// CHECK-NEXT: [7]
// CHECK-NEXT:   preds: 8 
// CHECK-NEXT:   succs: 8 
// CHECK-NEXT: 
// CHECK-NEXT: [6]
// CHECK-NEXT:   preds: 9(U) 
// CHECK-NEXT:   succs: 2(U) 5 
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
// CHECK-NEXT:   succs: 2(U) 4 
// CHECK-NEXT:   NumberLiteral: '5'
// CHECK-NEXT:   | value: 5
// CHECK-NEXT:   ResolvedIfStmt
// CHECK-NEXT:     ResolvedBinaryOperator: '&&'
// CHECK-NEXT:     | value: 1
// CHECK-NEXT:       NumberLiteral: '6'
// CHECK-NEXT:       | value: 6
// CHECK-NEXT:       NumberLiteral: '5'
// CHECK-NEXT:       | value: 5
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       ResolvedWhileStmt
// CHECK-NEXT:         NumberLiteral: '4'
// CHECK-NEXT:         | value: 4
// CHECK-NEXT:         ResolvedBlock
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT:       ResolvedWhileStmt
// CHECK-NEXT:         NumberLiteral: '2'
// CHECK-NEXT:         | value: 2
// CHECK-NEXT:         ResolvedBlock
// CHECK-NEXT: 
// CHECK-NEXT: [4]
// CHECK-NEXT:   preds: 3 5 
// CHECK-NEXT:   succs: 0(U) 3 
// CHECK-NEXT:   NumberLiteral: '4'
// CHECK-NEXT:   | value: 4
// CHECK-NEXT:   ResolvedWhileStmt
// CHECK-NEXT:     NumberLiteral: '4'
// CHECK-NEXT:     | value: 4
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT: 
// CHECK-NEXT: [3]
// CHECK-NEXT:   preds: 4 
// CHECK-NEXT:   succs: 4 
// CHECK-NEXT: 
// CHECK-NEXT: [2]
// CHECK-NEXT:   preds: 1 5(U) 6(U) 
// CHECK-NEXT:   succs: 0(U) 1 
// CHECK-NEXT:   NumberLiteral: '2'
// CHECK-NEXT:   | value: 2
// CHECK-NEXT:   ResolvedWhileStmt
// CHECK-NEXT:     NumberLiteral: '2'
// CHECK-NEXT:     | value: 2
// CHECK-NEXT:     ResolvedBlock
// CHECK-NEXT: 
// CHECK-NEXT: [1]
// CHECK-NEXT:   preds: 2 
// CHECK-NEXT:   succs: 2 
// CHECK-NEXT: 
// CHECK-NEXT: [0 (exit)]
// CHECK-NEXT:   preds: 2(U) 4(U) 8(U) 
// CHECK-NEXT:   succs: 
