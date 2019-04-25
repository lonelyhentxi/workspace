# build libextmem
message("-- Copy data to extmem target directory...")
file(COPY ./data DESTINATION ${EXECUTABLE_OUTPUT_PATH})
message("-- Successfully copied!")

file(GLOB EXTMEM_SOURCES extmem.h extmem.c)
add_library(extmem STATIC ${EXTMEM_SOURCES})
target_include_directories(extmem PUBLIC .)
set_target_properties(extmem PROPERTIES LINKER_LANGUAGE C)

add_executable(extmem_test test.c)
target_include_directories(extmem_test PRIVATE .)
set_target_properties(extmem_test PROPERTIES LINKER_LANGUAGE C)
target_link_libraries(extmem_test extmem)
