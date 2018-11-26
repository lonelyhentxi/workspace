cmake_minimum_required(VERSION 3.12)
project(opencv_learning)

set(CMAKE_CXX_STANDARD 17)
find_package(OpenCV REQUIRED)

add_executable(opencv_learning main.cpp)
target_include_directories(opencv_learning PRIVATE ${OpenCV_INCLUDE_DIRS})
target_link_libraries(opencv_learning PRIVATE ${OpenCV_LIBS})
