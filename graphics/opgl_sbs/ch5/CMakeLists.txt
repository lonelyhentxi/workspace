add_executable(shader shader.cpp)
target_link_libraries(shader PRIVATE GLUT::GLUT GLEW::GLEW glm ${Boost_LIBRARIES})