cmake_minimum_required(VERSION 3.10)
# if use vcpkg, install it from github, set environment variable VCPKG_HOME, remove comments
# set(CMAKE_TOOLCHAIN_FILE $ENV{VCPKG_HOME}/scripts/buildsystems/vcpkg.cmake)
# find_package(Catch2 CONFIG REQUIRED)
set(CMAKE_CXX_STANDARD 17)

project(tiny_db_engine)
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/cmake-build-debug)

add_subdirectory(lib/extmem)
link_directories(lib/extmem)

message("-- Copy extmem data to target directory...")
file(COPY lib/extmem/data DESTINATION ${EXECUTABLE_OUTPUT_PATH})
message("-- Successfully copied!")

include_directories(includes lib/extmem)
file(GLOB_RECURSE SOURCES includes/*.hpp sources/*.cpp)

add_executable(tiny_db_engine main.cpp ${SOURCES})
target_link_libraries(tiny_db_engine extmem)
# if use vcpkg, remove comments
# target_link_libraries(tiny_db_engine Catch2::Catch2)
