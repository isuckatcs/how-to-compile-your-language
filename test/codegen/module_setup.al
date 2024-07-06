// RUN: compiler %s -llvm-dump 2>&1 | filecheck %s

// CHECK: ; ModuleID = '<translation_unit>'
// CHECK-NEXT: source_filename = "{{.*}}/module_setup.al"
// CHECK-NEXT: target triple = "{{.*}}"

fn main(): void {}
