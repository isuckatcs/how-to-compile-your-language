cmake_minimum_required(VERSION 3.2)

project(how-to-compile-your-language)

find_package(LLVM 20.1 REQUIRED CONFIG)
include_directories(include "${LLVM_INCLUDE_DIR}")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

if(CMAKE_BUILD_TYPE STREQUAL "Coverage")
    set(COVERAGE_FLAGS "-g -O0 --coverage -fno-exceptions")
    SET(CMAKE_CXX_FLAGS "${COVERAGE_FLAGS}")
    SET(CMAKE_C_FLAGS "${COVERAGE_FLAGS}")

    execute_process(COMMAND ${CMAKE_C_COMPILER} "-print-file-name=libgcov.a" OUTPUT_VARIABLE LIBGCOV OUTPUT_STRIP_TRAILING_WHITESPACE)
    message(STATUS "Using ${LIBGCOV} for coverage")
    add_compile_definitions(YL_LIBGCOV="${LIBGCOV}")
endif()

add_subdirectory(runtime)
add_subdirectory(src)
add_subdirectory(test)
