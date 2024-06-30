// RUN: (compiler src1 src2 || true) 2>&1 | filecheck %s
// CHECK: error: unexpected argument 'src2'
