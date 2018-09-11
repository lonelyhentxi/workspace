cmake_minimum_required(VERSION 3.11)
project(boost_cookbook)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    set(CMAKE_CXX_FLAGS "-stdlib=libc++")
    set(CMAKE_C_FLAGS "-stdlib=libc++")
endif()

find_package(Boost COMPONENTS REQUIRED)
include_directories(${Boost_INCLUDE_DIR})



add_executable(boost_cookbook main.cpp)