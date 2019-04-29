file(GLOB EXTMEMFS_SOURCES include/* include/vfs/* source/*)
find_package(Boost REQUIRED)
add_library(extmemfs STATIC ${EXTMEMFS_SOURCES})
set_target_properties(extmemfs PROPERTIES LINKER_LANGUAGE CXX)
find_library({EXTMEM_LIB} NAMES extmem PATHS ../extmem)
target_include_directories(extmemfs PUBLIC
        include
        include/vfs
        ${Boost_INCLUDE_DIRS}
        )
target_link_libraries(extmemfs PRIVATE ${Boost_LIBRARIES} extmem)
