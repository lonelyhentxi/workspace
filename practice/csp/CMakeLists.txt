cmake_minimum_required(VERSION 3.11)

set(CMAKE_TOOLCHAIN_FILE $ENV{VCPKG_HOME}/scripts/buildsystems/vcpkg.cmake)
project(csp)

set(CMAKE_CXX_STANDARD 11)
add_compile_definitions(WITH_CMAKE)
find_package(Catch2 CONFIG REQUIRED)
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/cmake-build)
include_directories(
        src/201312)
file(GLOB_RECURSE SOURCES src/**/*.hpp src/**/*_test.cpp)
add_executable(test src/test.cpp ${SOURCES})
target_link_libraries(test Catch2::Catch2)
