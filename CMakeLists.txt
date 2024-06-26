cmake_minimum_required(VERSION 3.2)

project(a-compiler)

find_package(LLVM REQUIRED CONFIG)

message("${LLVM_LD_FLAGS}")

set(CMAKE_CXX_STANDARD 17)

include_directories(include "${LLVM_INCLUDE_DIR}")

add_subdirectory(src)
add_subdirectory(test)
