// RUN: compiler %s -res-dump
fn main(): void {
    let x: number;
    
    if 1.0 {
        x = 2.0;
    } else {
        x = 3.0;
    }

    x;
}

fn modifyImmutable(): void {
    let x: number = 1.0;
    
    if 1.0 {
        // CHECK: [[# @LINE + 1 ]]:9: error: 'x' cannot be mutated
        x = 1.0;
    } else {
        // CHECK: [[# @LINE + 1 ]]:9: error: 'x' cannot be mutated
        x = 2.0;
    }

    // CHECK: [[# @LINE + 1 ]]:5: error: 'x' cannot be mutated
    x = 3.0;
}

fn uninitOneBranch(): void {
    let x: number;
    
    if 1.0 {
        let x: number;
        x = 1.0; // init inner 'x'
    } else {
        x = 2.0;
    }

    // CHECK: [[# @LINE + 1 ]]:5: error: 'x' is not initialized
    x;
}

fn initEveryBranch(): void {
    var x: number;
    
    if 1.0 {
        x = 1.0;
    } else {
        x = 2.0;
    }

    // CHECK-NOT: [[# @LINE + 1 ]]:5: error
    x;
}

fn initConditionVar(): void {
    var x: number;
    
    // CHECK: [[# @LINE + 1 ]]:15: error: 'x' is not initialized
    if 1.0 || x {
        x = 1.0;
    // CHECK: [[# @LINE + 1 ]]:22: error: 'x' is not initialized
    } else if 0.0 && x == 2.0 {
        x = 3.0;
    } else {
        x = 2.0;
    }

    // CHECK-NOT: [[# @LINE + 1 ]]:5: error
    x;
}

fn loop(): void {
    let x: number;
    var y: number;
    
    while 1.0 {
        // CHECK: [[# @LINE + 1 ]]:9: error: 'x' cannot be mutated
        x = 1.0;

        // CHECK-NOT: [[# @LINE + 1 ]]:9: error
        y = 1.0;
    }

    // CHECK: [[# @LINE + 1 ]]:5: error: 'x' is not initialized
    x;
    // CHECK: [[# @LINE + 1 ]]:5: error: 'y' is not initialized
    y;
}

fn shadowInitialization(): void {
    let x: number;
    
    if 1.0 {
        let x: number;
        x = 1.0;
    } else {
        x = 2.0;
    }

    // CHECK: [[# @LINE + 1 ]]:5: error: 'x' is not initialized
    x;
}

fn shadowInitialized(): void {
    let x: number;
    
    if 1.0 {
        x = 1.0;
        let x: number;
        // CHECK: [[# @LINE + 1 ]]:9: error: 'x' is not initialized
        x;
    } else {
        x = 2.0;
    }

    x;
}
