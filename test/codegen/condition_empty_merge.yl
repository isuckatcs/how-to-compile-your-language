// RUN: compiler %s -llvm-dump 2>&1 | filecheck %s
// RUN: compiler %s -o condition_empty_merge && ./condition_empty_merge | ( ! grep ^ )
fn foo(x: number): void {
    if x == 0.0 {
        return;
    }
}

fn main(): void {
    foo(2.0);
}

// CHECK: define void @foo(double %x) {
// CHECK-NEXT:   %x1 = alloca double, align 8
// CHECK-NEXT:   store double %x, double* %x1, align 8
// CHECK-NEXT:   %1 = load double, double* %x1, align 8
// CHECK-NEXT:   %2 = fcmp oeq double %1, 0.000000e+00
// CHECK-NEXT:   %toDouble = uitofp i1 %2 to double
// CHECK-NEXT:   %toBool = fcmp one double %toDouble, 0.000000e+00
// CHECK-NEXT:   br i1 %toBool, label %then, label %merge
// CHECK-NEXT: 
// CHECK-NEXT: then:                                             ; preds = %0
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: merge:                                            ; preds = <null operand!>, %0
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: return:                                           ; preds = %merge, %then
// CHECK-NEXT:   ret void
// CHECK-NEXT: }
