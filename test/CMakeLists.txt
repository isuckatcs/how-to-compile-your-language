message(${CMAKE_BINARY_DIR}/src)

add_custom_target(test
    python ${CMAKE_CURRENT_SOURCE_DIR}/lit-runner.py
    ${CMAKE_CURRENT_SOURCE_DIR}
    -s
    --path=${CMAKE_BINARY_DIR}/src
    DEPENDS compiler)
