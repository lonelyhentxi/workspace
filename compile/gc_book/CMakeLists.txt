# toolchain file
cmake_minimum_required (VERSION 3.8)

if(NOT TEST CMAKE_TOOLCHAIN_FILE)
    set(CMAKE_TOOLCHAIN_FILE $ENV{VCPKG_HOME}/scripts/buildsystems/vcpkg.cmake)
endif()

project ("gc_book")

set(LIBRARY_NAME "tiny_gc")
file(GLOB_RECURSE SOURCE_FILES src/*.cpp)
add_library (${LIBRARY_NAME} STATIC ${SOURCE_FILES})
target_include_directories(${LIBRARY_NAME} PRIVATE src)

set(SPEC_NAME "tiny_gc_spec")
find_package(Catch2 CONFIG REQUIRED)
file(GLOB_RECURSE SPEC_FILES spec/*.cpp)
add_executable(${SPEC_NAME} test.cpp ${SPEC_FILES})
target_include_directories(${SPEC_NAME} PRIVATE src ${CATCH2_INCLUDE_PATH})
target_link_libraries(${SPEC_NAME} PRIVATE ${LIBRARY_NAME} Catch2::Catch2)