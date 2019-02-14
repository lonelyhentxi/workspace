cmake_minimum_required(VERSION 3.11)

set(CMAKE_TOOLCHAIN_FILE $ENV{VCPKG_HOME}/scripts/buildsystems/vcpkg.cmake)
project(codewars)

set(CMAKE_CXX_STANDARD 14)
find_package(Catch2 CONFIG REQUIRED)
include_directories(
        src/fivekyu
        src/sixkyu
        src/sevenkyu
		src/fourkyu
		src/threekyu
        src/twokyu)
file(GLOB_RECURSE SOURCES src/**/*.hpp src/**/*.cpp)
add_executable(test src/test.cpp ${SOURCES})
target_link_libraries(test Catch2::Catch2)