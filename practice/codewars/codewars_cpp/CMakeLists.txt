cmake_minimum_required(VERSION 3.10)


set(CMAKE_GENERATOR "Ninja")
message("generator is set to ${CMAKE_GENERATOR}")
set(CMAKE_TOOLCHAIN_FILE $ENV{VCPKG_HOME}/scripts/buildsystems/vcpkg.cmake)

project(codewars)

set(CMAKE_CXX_STANDARD 17)

find_package(Catch2 CONFIG REQUIRED)

set(CMAKE_BINARY_DIR ${PROJECT_SOURCE_DIR}/cmake-build-${CMAKE_BUILD_TYPE})
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/cmake-build-${CMAKE_BUILD_TYPE})
set(LIBRARY_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/cmake-build-${CMAKE_BUILD_TYPE})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/cmake-build-${CMAKE_BUILD_TYPE})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/cmake-build-${CMAKE_BUILD_TYPE})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/cmake-build-${CMAKE_BUILD_TYPE})

include_directories(
        src/fivekyu
        src/sixkyu
		src/fourkyu
		src/threekyu
        src/twokyu)
file(GLOB_RECURSE SOURCES src/**/*.hpp spec/**/*.cpp)
add_executable(test spec/test.cpp ${SOURCES})
target_link_libraries(test Catch2::Catch2)