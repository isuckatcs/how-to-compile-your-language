// RUN: (compiler %s || true) 2>&1 | filecheck %s
// CHECK: [[# @LINE + 1 ]]:1: error: non-void function doesn't return a value
fn noReturn(): number {}

// CHECK: [[# @LINE + 1 ]]:1: error: non-void function doesn't return a value on every path
fn noReturnAllBranch(x: number): number {
    if x == 0.0 {
        return 1.0;
    }
}

// CHECK: [[# @LINE + 1 ]]:1: error: non-void function doesn't return a value on every path
fn returnAllBranch2(x: number): number {
    if x == 0.0 {
        return 1.0;
    } else if x == 1.0 {
        return 2.0;
    }
}

fn returnAllBranch(x: number): number {
    if x == 0.0 {
        return 1.0;
    } else if x == 1.0 {
        return 2.0;
    } else {
        return 3.0;
    }
}

fn alwaysReturnIf(): number {
    if 1.0 {
        return 0.0;
    }

    // unreachable
}

// CHECK: [[# @LINE + 1 ]]:1: error: non-void function doesn't return a value
fn noReturnIf(): number {
    if 0.0 {
        // unreachable
        return 0.0;
    }
}

fn alwaysReturnElseIf(): number {
    if 0.0 {
        // unreachable
        return 0.0;
    } else if 1.0 {
        return 0.0;
    }

    // unreachable
}

// CHECK: [[# @LINE + 1 ]]:1: error: non-void function doesn't return a value
fn noReturnElseIf(): number {
    if 0.0 {
        // unreachable
        return 0.0;
    } else if 0.0 {
        // unreachable
        return 0.0;
    }
}

fn alwaysReturnElseIf2(x: number): number {
    if x {
        return 0.0;
    } else if 1.0 {
        return 0.0;
    }
}

// CHECK: [[# @LINE + 1 ]]:1: error: non-void function doesn't return a value on every path
fn uknownReturnElseIf(x: number): number {
    if x > 1.0 {
        return 0.0;
    } else if x < 1.0 {
        return 0.0;
    } else if x == 0.0 {
        return 3.0;
    }
}

fn alwaysReturnWhile(): number {
    while 1.0 {
        return 1.0;
    }

    // unreachable
}

// CHECK: [[# @LINE + 1 ]]:1: error: non-void function doesn't return a value
fn noReturnWhile(): number {
    while 0.0 {
        // unreachable
        return 1.0;
    }
}

// CHECK: [[# @LINE + 1 ]]:1: error: non-void function doesn't return a value on every path
fn uknownReturnWhile(x: number): number {
    while x {
        return 1.0;
    }
}

fn alwaysReturnCondition(x: number): number {
    if x || x || (0.0 - 1.0) {
        return 0.0;
    }
}

// CHECK: [[# @LINE + 1 ]]:1: error: non-void function doesn't return a value on every path
fn noReturnCondition(x: number): number {
    if x && (0.0 - 1.0) {
        return 0.0;
    }
}

fn alwaysReturnCondition2(x: number): number {
    if x && (0.0 - 1.0) {
        return 0.0;
    }

    return 0.0;
}

// CHECK: [[# @LINE + 1 ]]:1: error: non-void function doesn't return a value
fn noReturnLoop(x: number): number {
    while x {}
}

// CHECK: [[# @LINE + 1 ]]:1: error: non-void function doesn't return a value
fn noReturnInfiniteLoop(): number {
    while 1.0 {}
}

// CHECK: [[# @LINE + 1 ]]:1: error: non-void function doesn't return a value
fn noReturnNeverRunningLoop(): number {
    while 0.0 {}
}

fn main(): void {}
