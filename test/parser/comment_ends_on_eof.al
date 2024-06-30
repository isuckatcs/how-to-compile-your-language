// RUN: compiler %s 2>&1

fn main(): void {}

// It used to crash, when the comment ended on EOF.