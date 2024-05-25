add_executable(compiler lexer.cpp parser.cpp compiler.cpp)

llvm_map_components_to_libnames(llvm_libs core)

target_link_libraries(compiler ${llvm_libs})
