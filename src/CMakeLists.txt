file(GLOB compiler_src CONFIGURE_DEPENDS "*.cpp")

add_executable(compiler ${compiler_src})

# llvm_map_components_to_libnames(llvm_libs LLVM)

target_link_libraries(compiler LLVM)
