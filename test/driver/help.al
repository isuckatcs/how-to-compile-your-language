// RUN: compiler -h 2>&1 | filecheck %s

// CHECK: Usage:
// CHECK-NEXT:   your-compiler [options] <source_file>
// CHECK-NEXT: 
// CHECK-NEXT: Options:
// CHECK-NEXT:   -h           display this message
// CHECK-NEXT:   -o <file>    write executable to <file>
// CHECK-NEXT:   -ast-dump    print the abstract syntax tree
// CHECK-NEXT:   -res-dump    print the resolved syntax tree
// CHECK-NEXT:   -llvm-dump   print the llvm module
// CHECK-NEXT:   -cfg-dump   print the control flow graph
