// RUN: compiler %s -llvm-dump 2>&1 | filecheck %s
// RUN: compiler %s -o alloca_order && ./alloca_order | grep -Plzx '1\n2\n3\n'
fn foo(x: number, y: number, z: number): void {
    let x: number = x;
    let y: number = y;
    let z: number = z;

    print(x);
    print(y);
    print(z);
}
// CHECK: define void @foo(double %x, double %y, double %z) {
// CHECK-NEXT:   %x1 = alloca double, align 8
// CHECK-NEXT:   %y2 = alloca double, align 8
// CHECK-NEXT:   %z3 = alloca double, align 8
// CHECK-NEXT:   %x4 = alloca double, align 8
// CHECK-NEXT:   %y5 = alloca double, align 8
// CHECK-NEXT:   %z6 = alloca double, align 8
// CHECK-NEXT:   store double %x, double* %x1, align 8
// CHECK-NEXT:   store double %y, double* %y2, align 8
// CHECK-NEXT:   store double %z, double* %z3, align 8
// CHECK-NEXT:   %1 = load double, double* %x1, align 8
// CHECK-NEXT:   store double %1, double* %x4, align 8
// CHECK-NEXT:   %2 = load double, double* %y2, align 8
// CHECK-NEXT:   store double %2, double* %y5, align 8
// CHECK-NEXT:   %3 = load double, double* %z3, align 8
// CHECK-NEXT:   store double %3, double* %z6, align 8
// CHECK-NEXT:   %4 = load double, double* %x4, align 8
// CHECK-NEXT:   call void @print(double %4)
// CHECK-NEXT:   %5 = load double, double* %y5, align 8
// CHECK-NEXT:   call void @print(double %5)
// CHECK-NEXT:   %6 = load double, double* %z6, align 8
// CHECK-NEXT:   call void @print(double %6)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

fn main(): void {
    foo(1.0, 2.0, 3.0);
}
