// RUN: compiler %s -res-dump 2>&1 | filecheck %s
fn foo(x: number): void {}

fn bar(x: number): void {
    foo(x);
}

fn main(): void {
    bar(1.0);
}

// CHECK: ResolvedFunctionDecl: @({{.*}}) foo:
// CHECK-NEXT:   ResolvedParamDecl: @({{.*}}) x:
// CHECK-NEXT:   ResolvedBlock
// CHECK-NEXT: ResolvedFunctionDecl: @({{.*}}) bar:
// CHECK-NEXT:   ResolvedParamDecl: @({{.*}}) x:
// CHECK-NEXT:   ResolvedBlock
// CHECK-NEXT:     ResolvedCallExpr: @({{.*}}) foo
// CHECK-NEXT:       ResolvedDeclRefExpr: @({{.*}}) x
// CHECK-NEXT: ResolvedFunctionDecl: @({{.*}}) main:
// CHECK-NEXT:   ResolvedBlock
// CHECK-NEXT:     ResolvedCallExpr: @({{.*}}) bar
// CHECK-NEXT:       NumberLiteral: '1'
// CHECK-NEXT:       | value: 1
