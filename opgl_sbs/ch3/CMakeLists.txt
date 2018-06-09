add_executable(first_triangle first_triangle.cpp)
target_link_libraries(first_triangle PRIVATE GLUT::GLUT GLEW::GLEW glm)