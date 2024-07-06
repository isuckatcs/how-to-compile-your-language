// RUN: compiler %s -llvm-dump 2>&1 | filecheck %s
// RUN: compiler %s -o while_empty_exit_ret && ./while_empty_exit_ret | grep -Plzx '2\n'
fn foo(x: number): void {
    var i: number = x;

    while i > 1.0 {
        print(i);
        i = i - 1.0;
    }
}

fn main(): void {
    foo(2.0);
}
// CHECK: define void @foo(double %x) {
// CHECK-NEXT:   %x1 = alloca double, align 8
// CHECK-NEXT:   %i = alloca double, align 8
// CHECK-NEXT:   store double %x, double* %x1, align 8
// CHECK-NEXT:   %1 = load double, double* %x1, align 8
// CHECK-NEXT:   store double %1, double* %i, align 8
// CHECK-NEXT:   br label %whileCond
// CHECK-NEXT: 
// CHECK-NEXT: whileCond:                                        ; preds = %whileBody, %0
// CHECK-NEXT:   %2 = load double, double* %i, align 8
// CHECK-NEXT:   %3 = fcmp ogt double %2, 1.000000e+00
// CHECK-NEXT:   %toDouble = uitofp i1 %3 to double
// CHECK-NEXT:   %toBool = fcmp one double %toDouble, 0.000000e+00
// CHECK-NEXT:   br i1 %toBool, label %whileBody, label %whileExit
// CHECK-NEXT: 
// CHECK-NEXT: whileBody:                                        ; preds = %whileCond
// CHECK-NEXT:   %4 = load double, double* %i, align 8
// CHECK-NEXT:   call void @print(double %4)
// CHECK-NEXT:   %5 = load double, double* %i, align 8
// CHECK-NEXT:   %6 = fsub double %5, 1.000000e+00
// CHECK-NEXT:   store double %6, double* %i, align 8
// CHECK-NEXT:   br label %whileCond
// CHECK-NEXT: 
// CHECK-NEXT: whileExit:                                        ; preds = %whileCond
// CHECK-NEXT:   ret void
// CHECK-NEXT: }
