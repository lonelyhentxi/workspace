cmake_minimum_required(VERSION 3.10)
set(CMAKE_TOOLCHAIN_FILE $ENV{VCPKG_HOME}/scripts/buildsystems/vcpkg.cmake)
set(CMAKE_CXX_STANDARD 17)

project(tiny_db_engine)
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/cmake-build-debug)

find_package(Catch2 CONFIG REQUIRED)

include_directories(includes)
file(GLOB_RECURSE SOURCES includes/*.hpp sources/*.cpp)

add_subdirectory(lib/extmem)

add_executable(tiny_db_engine main.cpp ${SOURCES})
target_link_libraries(tiny_db_engine Catch2::Catch2)
