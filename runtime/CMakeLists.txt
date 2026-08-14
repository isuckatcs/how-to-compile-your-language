file(GLOB runtime_src CONFIGURE_DEPENDS "*.c")

add_library(yl_runtime ${runtime_src})
target_compile_options(yl_runtime PRIVATE -Wall -Wextra -pedantic)
