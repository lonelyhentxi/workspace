cmake_minimum_required(VERSION 3.11)
project(software_rendering)

set(CMAKE_CXX_STANDARD 17)
include_directories(deps includes)
set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/libs)
link_directories(${PROJECT_BINARY_DIR}/libs)
add_subdirectory(deps)
add_subdirectory(test)