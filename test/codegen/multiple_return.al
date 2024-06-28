// RUN: compiler %s -llvm-dump 2>&1 | filecheck %s
// RUN: compiler %s -o multiple_return && ./multiple_return
fn main(): void {
    return;

    let x: number = 1.0;
    return;
}

// CHECK: define void @__builtin_main() {
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: return:                                           ; preds = %0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }
