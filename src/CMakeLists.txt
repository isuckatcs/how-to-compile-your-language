file(GLOB compiler_src CONFIGURE_DEPENDS "*.cpp")

add_executable(compiler ${compiler_src})
target_compile_options(compiler PRIVATE -Wall -Wextra -pedantic)

llvm_map_components_to_libnames(llvm_libs core)

target_link_libraries(compiler ${llvm_libs})
