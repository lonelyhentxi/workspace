# build libextmem
file(GLOB EXTMEM_SOURCES extmem.h extmem.c)
add_library(extmem SHARED ${EXTMEM_SOURCES})
message("-- Copy data to extmem target directory...")
add_custom_command(TARGET extmem POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory
        ${CMAKE_CURRENT_SOURCE_DIR}/data $<TARGET_FILE_DIR:extmem>/data)
message("-- Successfully copied!")

target_link_directories($<TARGET_FILE_DIR:extmem>)
add_executable(extmem_test test.c)
target_link_libraries(extmem_test extmem)
