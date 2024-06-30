// RUN: compiler %s -res-dump 2>&1 | filecheck %s
fn main(): void {}

// CHECK: ResolvedFunctionDecl: @({{.*}}) print:
// CHECK-NEXT:  ResolvedParamDecl: @({{.*}}) n:
// CHECK-NEXT:  ResolvedBlock
