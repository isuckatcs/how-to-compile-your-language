cmake_minimum_required(VERSION 3.2)

project(how-to-compile-your-language)

find_package(LLVM REQUIRED CONFIG)
include_directories(include "${LLVM_INCLUDE_DIR}")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

set(COVERAGE_FLAGS "-g -O0 --coverage -fprofile-arcs -ftest-coverage -fno-exceptions")
SET(CMAKE_CXX_FLAGS_COVERAGE "${COVERAGE_FLAGS}")
SET(CMAKE_C_FLAGS_COVERAGE "${COVERAGE_FLAGS}")

add_subdirectory(src)
add_subdirectory(test)
