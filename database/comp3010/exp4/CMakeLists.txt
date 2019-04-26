cmake_minimum_required(VERSION 3.10)

#set(VCPKG_LIBRARY_LINKAGE static)
set(CMAKE_TOOLCHAIN_FILE $ENV{VCPKG_HOME}/scripts/buildsystems/vcpkg.cmake)
set(CMAKE_CXX_STANDARD 17)

project(tiny_db_engine)

SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/cmake-build-debug)
set(VCPKG_INCLUDE_DIR $ENV{VCPKG_HOME}/installed/${VCPKG_TARGET_TRIPLET}/include)
set(VCPKG_LIBRARY_DIR $ENV{VCPKG_HOME}/installed/${VCPKG_TARGET_TRIPLET}/lib)

file(GLOB_RECURSE PROJECT_INCLUDES include/*)
file(GLOB_RECURSE PROJECT_SOURCES source/*)

find_package(Catch2 CONFIG REQUIRED)
find_package(Boost REQUIRED)
find_package(Qt5 COMPONENTS Core CONFIG REQUIRED)
add_subdirectory(lib/extmem)
add_subdirectory(lib/extmemfs)

include_directories(${Boost_INCLUDE_DIRS} ${Catch2_INCLUDE_DIRS} ${Qt5_INCLUDE_DIRS} include lib/extmemfs/include/filesystem)

add_executable(tiny_db_engine main.cpp ${PROJECT_INCLUDES} ${PROJECT_SOURCES})
target_link_libraries(tiny_db_engine LINK_PUBLIC Catch2::Catch2 ${Boost_LIBRARIES} Qt5::Core extmem)
