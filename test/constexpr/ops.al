// RUN: compiler %s -res-dump 2>&1 | filecheck %s
fn prefix(): void {
    !1.0;
    !0.0;
}
// CHECK:    ResolvedUnaryOperator: '!'
// CHECK-NEXT:    | value: 0
// CHECK-NEXT:      NumberLiteral: '1'
// CHECK-NEXT:      | value: 1
// CHECK-NEXT:    ResolvedUnaryOperator: '!'
// CHECK-NEXT:    | value: 1
// CHECK-NEXT:      NumberLiteral: '0'
// CHECK-NEXT:      | value: 0

fn multiplicative(): void {
    5.0 * 3.0;
    20.0 / 4.0;
}
// CHECK:    ResolvedBinaryOperator: '*'
// CHECK-NEXT:    | value: 15
// CHECK-NEXT:      NumberLiteral: '5'
// CHECK-NEXT:      | value: 5
// CHECK-NEXT:      NumberLiteral: '3'
// CHECK-NEXT:      | value: 3
// CHECK-NEXT:    ResolvedBinaryOperator: '/'
// CHECK-NEXT:    | value: 5
// CHECK-NEXT:      NumberLiteral: '20'
// CHECK-NEXT:      | value: 20
// CHECK-NEXT:      NumberLiteral: '4'
// CHECK-NEXT:      | value: 4

fn additive(): void {
    5.0 + 3.0;
    20.0 - 4.0;
}
// CHECK:    ResolvedBinaryOperator: '+'
// CHECK-NEXT:    | value: 8
// CHECK-NEXT:      NumberLiteral: '5'
// CHECK-NEXT:      | value: 5
// CHECK-NEXT:      NumberLiteral: '3'
// CHECK-NEXT:      | value: 3
// CHECK-NEXT:    ResolvedBinaryOperator: '-'
// CHECK-NEXT:    | value: 16
// CHECK-NEXT:      NumberLiteral: '20'
// CHECK-NEXT:      | value: 20
// CHECK-NEXT:      NumberLiteral: '4'
// CHECK-NEXT:      | value: 4

fn comparison(): void {
    2.0 < 5.0;
    5.0 < 2.0;

    2.0 > 5.0;
    5.0 > 2.0;
}
// CHECK:    ResolvedBinaryOperator: '<'
// CHECK-NEXT:    | value: 1
// CHECK-NEXT:      NumberLiteral: '2'
// CHECK-NEXT:      | value: 2
// CHECK-NEXT:      NumberLiteral: '5'
// CHECK-NEXT:      | value: 5
// CHECK-NEXT:    ResolvedBinaryOperator: '<'
// CHECK-NEXT:    | value: 0
// CHECK-NEXT:      NumberLiteral: '5'
// CHECK-NEXT:      | value: 5
// CHECK-NEXT:      NumberLiteral: '2'
// CHECK-NEXT:      | value: 2
// CHECK-NEXT:    ResolvedBinaryOperator: '>'
// CHECK-NEXT:    | value: 0
// CHECK-NEXT:      NumberLiteral: '2'
// CHECK-NEXT:      | value: 2
// CHECK-NEXT:      NumberLiteral: '5'
// CHECK-NEXT:      | value: 5
// CHECK-NEXT:    ResolvedBinaryOperator: '>'
// CHECK-NEXT:    | value: 1
// CHECK-NEXT:      NumberLiteral: '5'
// CHECK-NEXT:      | value: 5
// CHECK-NEXT:      NumberLiteral: '2'
// CHECK-NEXT:      | value: 2

fn equality(): void {
    2.0 == 2.0;
    5.0 == 3.0;
}
// CHECK:    ResolvedBinaryOperator: '=='
// CHECK-NEXT:    | value: 1
// CHECK-NEXT:      NumberLiteral: '2'
// CHECK-NEXT:      | value: 2
// CHECK-NEXT:      NumberLiteral: '2'
// CHECK-NEXT:      | value: 2
// CHECK-NEXT:    ResolvedBinaryOperator: '=='
// CHECK-NEXT:    | value: 0
// CHECK-NEXT:      NumberLiteral: '5'
// CHECK-NEXT:      | value: 5
// CHECK-NEXT:      NumberLiteral: '3'
// CHECK-NEXT:      | value: 3

fn conjunction(): void {
    2.0 && 3.0;
    0.0 && 1.0;
    1.0 && 0.0;
    0.0 && 0.0;
}
// CHECK:    ResolvedBinaryOperator: '&&'
// CHECK-NEXT:    | value: 1
// CHECK-NEXT:      NumberLiteral: '2'
// CHECK-NEXT:      | value: 2
// CHECK-NEXT:      NumberLiteral: '3'
// CHECK-NEXT:      | value: 3
// CHECK-NEXT:    ResolvedBinaryOperator: '&&'
// CHECK-NEXT:    | value: 0
// CHECK-NEXT:      NumberLiteral: '0'
// CHECK-NEXT:      | value: 0
// CHECK-NEXT:      NumberLiteral: '1'
// CHECK-NEXT:      | value: 1
// CHECK-NEXT:    ResolvedBinaryOperator: '&&'
// CHECK-NEXT:    | value: 0
// CHECK-NEXT:      NumberLiteral: '1'
// CHECK-NEXT:      | value: 1
// CHECK-NEXT:      NumberLiteral: '0'
// CHECK-NEXT:      | value: 0
// CHECK-NEXT:    ResolvedBinaryOperator: '&&'
// CHECK-NEXT:    | value: 0
// CHECK-NEXT:      NumberLiteral: '0'
// CHECK-NEXT:      | value: 0
// CHECK-NEXT:      NumberLiteral: '0'
// CHECK-NEXT:      | value: 0

fn disjunction(): void {
    2.0 || 0.0;
    0.0 || 2.0;
    2.0 || 3.0;
    0.0 || 0.0;
}
// CHECK:    ResolvedBinaryOperator: '||'
// CHECK-NEXT:    | value: 1
// CHECK-NEXT:      NumberLiteral: '2'
// CHECK-NEXT:      | value: 2
// CHECK-NEXT:      NumberLiteral: '0'
// CHECK-NEXT:      | value: 0
// CHECK-NEXT:    ResolvedBinaryOperator: '||'
// CHECK-NEXT:    | value: 1
// CHECK-NEXT:      NumberLiteral: '0'
// CHECK-NEXT:      | value: 0
// CHECK-NEXT:      NumberLiteral: '2'
// CHECK-NEXT:      | value: 2
// CHECK-NEXT:    ResolvedBinaryOperator: '||'
// CHECK-NEXT:    | value: 1
// CHECK-NEXT:      NumberLiteral: '2'
// CHECK-NEXT:      | value: 2
// CHECK-NEXT:      NumberLiteral: '3'
// CHECK-NEXT:      | value: 3
// CHECK-NEXT:    ResolvedBinaryOperator: '||'
// CHECK-NEXT:    | value: 0
// CHECK-NEXT:      NumberLiteral: '0'
// CHECK-NEXT:      | value: 0
// CHECK-NEXT:      NumberLiteral: '0'
// CHECK-NEXT:      | value: 0

fn main(): void {}
