add_library(extmem STATIC source/extmem.c include/extmem.h)
set_target_properties(extmem PROPERTIES LINKER_LANGUAGE CXX)
target_include_directories(extmem PUBLIC include)

add_executable(extmem_test source/test.c)
set_target_properties(extmem_test PROPERTIES LINKER_LANGUAGE CXX)
target_link_libraries(extmem_test extmem)
target_include_directories(extmem_test PRIVATE include)
add_custom_command(TARGET extmem_test PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/data ${EXECUTABLE_OUTPUT_PATH}/data/test DEPENDS extmem)
