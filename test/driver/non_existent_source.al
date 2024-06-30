// RUN: (compiler ./non_existent_source || true) 2>&1 | filecheck %s
// CHECK: error: failed to open './non_existent_source'
