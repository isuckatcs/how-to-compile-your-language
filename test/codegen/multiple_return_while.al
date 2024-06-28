// RUN: compiler %s -llvm-dump 2>&1 | filecheck %s
// RUN: compiler %s -o multiple_return_while && ./multiple_return_while | grep -Plz '0.000000\n5.000000\n3.000000\n'
fn foo(x: number): number {
    var n: number = x;
    while n > 10.0 {
        n = n - 1.0;
        
        if x == 15.0 {
            return 3.0;

            return 1.0;
            
            if 0.0 {

            } else {
                return 2.0;
            }
        }

        return 5.0;
        
        return 1.0;
        1.0 + 3.0;
    }

    return 0.0;
}

// CHECK: whileExit:                                        ; preds = %whileCond
// CHECK-NEXT:   store double 0.000000e+00, double* %retval, align 8
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: then:                                             ; preds = %whileBody
// CHECK-NEXT:   store double 3.000000e+00, double* %retval, align 8
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: merge:                                            ; preds = <null operand!>, %whileBody
// CHECK-NEXT:   store double 5.000000e+00, double* %retval, align 8
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: return:                                           ; preds = %whileExit, %merge, %then
// CHECK-NEXT:   %8 = load double, double* %retval, align 8
// CHECK-NEXT:   ret double %8

fn main(): void {
    print(foo(10.0));
    print(foo(11.0));
    print(foo(15.0));
}
