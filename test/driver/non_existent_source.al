// RUN: (compiler ./non_existent.al || true) 2>&1 | filecheck %s
// CHECK: error: failed to open './non_existent.al'
