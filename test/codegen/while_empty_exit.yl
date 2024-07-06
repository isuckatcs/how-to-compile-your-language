// RUN: compiler %s -llvm-dump 2>&1 | filecheck %s
// RUN: compiler %s -o while_empty_exit && ./while_empty_exit | ( ! grep ^ )
fn foo(x: number): void {
    while x > 1.0 {
        return;
    }
}
// CHECK: define void @foo(double %x) {
// CHECK-NEXT:   %x1 = alloca double, align 8
// CHECK-NEXT:   store double %x, double* %x1, align 8
// CHECK-NEXT:   br label %whileCond
// CHECK-NEXT: 
// CHECK-NEXT: whileCond:                                        ; preds = <null operand!>, %0
// CHECK-NEXT:   %1 = load double, double* %x1, align 8
// CHECK-NEXT:   %2 = fcmp ogt double %1, 1.000000e+00
// CHECK-NEXT:   %toDouble = uitofp i1 %2 to double
// CHECK-NEXT:   %toBool = fcmp one double %toDouble, 0.000000e+00
// CHECK-NEXT:   br i1 %toBool, label %whileBody, label %whileExit
// CHECK-NEXT: 
// CHECK-NEXT: whileBody:                                        ; preds = %whileCond
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: whileExit:                                        ; preds = %whileCond
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: return:                                           ; preds = %whileExit, %whileBody
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

fn main(): void {
    foo(2.0);
}
