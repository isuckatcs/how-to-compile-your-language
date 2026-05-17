file(GLOB runtime_src CONFIGURE_DEPENDS "*.c")

add_library(yl_runtime ${runtime_src})
