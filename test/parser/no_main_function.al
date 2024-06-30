// RUN: compiler %s 2>&1 -ast-dump | filecheck %s
// CHECK: [[# @LINE + 1 ]]:1: error: main function not found
