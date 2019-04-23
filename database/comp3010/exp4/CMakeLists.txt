cmake_minimum_required(VERSION 3.10)

set(CMAKE_TOOLCHAIN_FILE $ENV{VCPKG_HOME}/scripts/buildsystems/vcpkg.cmake)
set(VCPKG_INCLUDE_DIR $ENV{VCPKG_HOME}/installed/${VCPKG_TARGET_TRIPLET}/include)
set(VCPKG_LIBRARY_DIR $ENV{VCPKG_HOME}/installed/${VCPKG_TARGET_TRIPLET}/lib)
set(CMAKE_CXX_STANDARD 17)

project(tiny_db_engine)

SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/cmake-build-debug)
find_package(Catch2 CONFIG REQUIRED)
find_package(Boost REQUIRED locale)
find_library(Tinyutf8_LIBRARY NAMES tinyutf8 PATHS "${VCPKG_INCLUDE_DIR}/tinyutf8.h")
find_path(Tinyutf8_INCLUDE_DIR ${VCPKG_LIBRARY_DIR}/tinyutf8.h)
include_directories(${tinyutf8_INCLUDE_DIR} ${Boost_INCLUDE_DIRS} ${Catch2_INCLUDE_DIRS} include)
file(GLOB_RECURSE SOURCES include/*.hpp source/*.cpp)

add_subdirectory(lib/extmem)

add_executable(tiny_db_engine main.cpp ${SOURCES})
target_link_libraries(tiny_db_engine LINK_PUBLIC Catch2::Catch2 ${Boost_LIBRARIES} ${Tinyutf8_LIBRARY})
