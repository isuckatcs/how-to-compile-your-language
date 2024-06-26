// RUN: compiler %s -llvm-dump 2>&1 | filecheck %s
// RUN: compiler %s -o multiple_return_if && ./multiple_return_if | grep -Plz '2.000000\n10.000000\n5.200000\n'
fn foo(x: number): number {
    if x == 1.0 {
        return 2.0;

        return 3.0;
    } else if x == 2.0 {
        return 10.0;

        let y: number = 5.0 + 1.0;
    }

    return 5.2;
}
// CHECK: then:                                             ; preds = %0
// CHECK-NEXT:   store double 2.000000e+00, double* %retval, align 8
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: else:                                             ; preds = %0
// CHECK-NEXT:   %3 = load double, double* %x1, align 8
// CHECK-NEXT:   %4 = fcmp oeq double %3, 2.000000e+00
// CHECK-NEXT:   %toDouble2 = uitofp i1 %4 to double
// CHECK-NEXT:   %toBool4 = fcmp one double %toDouble2, 0.000000e+00
// CHECK-NEXT:   br i1 %toBool4, label %then3, label %merge
// CHECK-NEXT: 
// CHECK-NEXT: then3:                                            ; preds = %else
// CHECK-NEXT:   store double 1.000000e+01, double* %retval, align 8
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: merge:                                            ; preds = <null operand!>, %else
// CHECK-NEXT:   br label %merge5
// CHECK-NEXT: 
// CHECK-NEXT: merge5:                                           ; preds = %merge, <null operand!>
// CHECK-NEXT:   store double 5.200000e+00, double* %retval, align 8
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: return:                                           ; preds = %merge5, %then3, %then
// CHECK-NEXT:   %5 = load double, double* %retval, align 8
// CHECK-NEXT:   ret double %5

fn main(): void {
    print(foo(1.0));
    print(foo(2.0));
    print(foo(3.0));
}
