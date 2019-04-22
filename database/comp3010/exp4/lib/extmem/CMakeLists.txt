# build libextmem
file(GLOB EXTMEM_SOURCES extmem.h extmem.c)
add_library(extmem STATIC ${EXTMEM_SOURCES})
set_target_properties(extmem PROPERTIES LINKER_LANGUAGE C)
message("-- Copy data to extmem target directory...")
add_custom_command(TARGET extmem POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory
        ${CMAKE_CURRENT_SOURCE_DIR}/data $<TARGET_FILE_DIR:extmem>)
message("-- Successfully copied!")

add_executable(extmem_test test.c)
set_target_properties(extmem_test PROPERTIES LINKER_LANGUAGE C)
target_link_libraries(extmem_test extmem)
