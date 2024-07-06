// RUN: compiler %s -llvm-dump 2>&1 | filecheck %s
// RUN: compiler %s -o return && ./return | ( ! grep ^ )
fn main(): void {}

fn noInsertPoint(): void {
    return;
}
// CHECK: define void @noInsertPoint() {
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: return:                                           ; preds = <null operand!>, %0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

fn insertPointEmptyBlock(): void {
    if 1.0 {
        return;
    }
}
// CHECK: define void @insertPointEmptyBlock() {
// CHECK-NEXT:   br i1 true, label %then, label %merge
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

fn insertPointEmptyBlock2(): void {
    while 1.0 {
        return;
    }
}
// CHECK: define void @insertPointEmptyBlock2() {
// CHECK-NEXT:   br label %whileCond
// CHECK-NEXT: 
// CHECK-NEXT: whileCond:                                        ; preds = <null operand!>, %0
// CHECK-NEXT:   br i1 true, label %whileBody, label %whileExit
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

fn insertPointNonEmptyBlock(): void {
    if 1.0 {
        return;
    }

    let x: number = 1.0;
}
// CHECK: define void @insertPointNonEmptyBlock() {
// CHECK-NEXT:   %x = alloca double, align 8
// CHECK-NEXT:   br i1 true, label %then, label %merge
// CHECK-NEXT: 
// CHECK-NEXT: then:                                             ; preds = %0
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: merge:                                            ; preds = <null operand!>, %0
// CHECK-NEXT:   store double 1.000000e+00, double* %x, align 8
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: return:                                           ; preds = %merge, %then
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

fn insertPointNonEmptyBlock2(): void {
    while 1.0 {
        return;
    }

    let x: number = 1.0;
}
// CHECK: define void @insertPointNonEmptyBlock2() {
// CHECK-NEXT:   %x = alloca double, align 8
// CHECK-NEXT:   br label %whileCond
// CHECK-NEXT: 
// CHECK-NEXT: whileCond:                                        ; preds = <null operand!>, %0
// CHECK-NEXT:   br i1 true, label %whileBody, label %whileExit
// CHECK-NEXT: 
// CHECK-NEXT: whileBody:                                        ; preds = %whileCond
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: whileExit:                                        ; preds = %whileCond
// CHECK-NEXT:   store double 1.000000e+00, double* %x, align 8
// CHECK-NEXT:   br label %return
// CHECK-NEXT: 
// CHECK-NEXT: return:                                           ; preds = %whileExit, %whileBody
// CHECK-NEXT:   ret void
// CHECK-NEXT: }
