add_custom_target(check
    COMMAND python3 ${CMAKE_CURRENT_SOURCE_DIR}/lit-runner.py ${CMAKE_CURRENT_SOURCE_DIR} -v -s --path=${CMAKE_BINARY_DIR}/bin
    DEPENDS compiler
    USES_TERMINAL)
