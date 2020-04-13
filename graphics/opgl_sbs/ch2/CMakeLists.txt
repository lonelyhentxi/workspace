add_executable(hello_vertex hello_vertex.cpp)
target_link_libraries(hello_vertex PRIVATE GLUT::GLUT GLEW::GLEW glm)