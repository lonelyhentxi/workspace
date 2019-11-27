cmake_minimum_required(VERSION 3.10)
set(CMAKE_TOOLCHAIN_FILE $ENV{VCPKG_HOME}/scripts/buildsystems/vcpkg.cmake)
set(CMAKE_CXX_STANDARD 17)

project(opencv_learning)
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/cmake-build-debug)

find_package(OpenCV REQUIRED)
find_package(Catch2 CONFIG REQUIRED)

include_directories(${OpenCV_INCLUDE_DIRS} includes)
file(GLOB_RECURSE SOURCES includes/**/*.hpp sources/**/*.cpp)

add_executable(opencv_learning sources/main.cpp ${SOURCES})
target_link_libraries(opencv_learning ${OpenCV_LIBS} Catch2::Catch2)
