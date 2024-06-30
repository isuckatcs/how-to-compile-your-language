// RUN: compiler %s || test $? -eq 1
fn main(): semaError {}
