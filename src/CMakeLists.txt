add_executable(compiler lexer.cpp parser.cpp driver.cpp sema.cpp utils.cpp codegen.cpp constexpr.cpp cfg.cpp)

llvm_map_components_to_libnames(llvm_libs core)

target_link_libraries(compiler ${llvm_libs})
