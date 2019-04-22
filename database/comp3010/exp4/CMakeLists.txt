cmake_minimum_required(VERSION 3.10)
set(CMAKE_TOOLCHAIN_FILE $ENV{VCPKG_HOME}/scripts/buildsystems/vcpkg.cmake)
set(CMAKE_CXX_STANDARD 17)

project(tiny_db_engine)
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/cmake-build-debug)
find_package(Catch2 CONFIG REQUIRED)
find_package(Boost REQUIRED locale)
find_package(ICU)

include_directories(${Boost_INCLUDE_DIRS} ${Catch2_INCLUDE_DIRS} ${Icu_INCLUDE_DIRS} include)
file(GLOB_RECURSE SOURCES include/*.hpp source/*.cpp)

add_subdirectory(lib/extmem)

add_executable(tiny_db_engine main.cpp ${SOURCES} include/shim.hpp)
target_link_libraries(tiny_db_engine LINK_PUBLIC Catch2::Catch2 ${Boost_LIBRARIES} ${Icu_LIBRARIES})
