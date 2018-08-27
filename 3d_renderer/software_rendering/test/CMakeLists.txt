project(software_rendering)

set(bresenham_line bresenham_line.cpp)
add_executable(bresenham_line bresenham_line.cpp)
target_link_libraries(bresenham_line tga)

set(home home.cpp)
add_executable(home home.cpp)
target_link_libraries(home tga)

set(triangle triangle.cpp)
add_executable(triangle triangle.cpp)
target_link_libraries(triangle tga)