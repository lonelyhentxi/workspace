# build libextmem
file(GLOB EXTMEM_SOURCES extmem.h extmem.c)
add_library(extmem SHARED ${EXTMEM_SOURCES})

# test libextmem
add_executable(extmem_test test.c)
link_directories($<TARGET_FILE_DIR:extmem>)
target_link_libraries(extmem_test extmem)
