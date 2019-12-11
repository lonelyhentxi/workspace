cmake_minimum_required(VERSION 3.10)


set(CMAKE_TOOLCHAIN_FILE $ENV{VCPKG_HOME}/scripts/buildsystems/vcpkg.cmake)
project(codewars)

set(CMAKE_CXX_STANDARD 14)
find_package(Catch2 CONFIG REQUIRED)
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/cmake-build-debug)
include_directories(
        src/fivekyu
        src/sixkyu
		src/fourkyu
		src/threekyu
        src/twokyu)
file(GLOB_RECURSE SOURCES src/**/*.hpp spec/**/*.cpp)
add_executable(main spec/test.cpp ${SOURCES})
target_link_libraries(main Catch2::Catch2)