project(emcpp)

include_directories(${Boost_INCLUDE_DIR})
set(ti type_inference.cpp)
add_executable(ti type_inference.cpp)
target_link_libraries(ti ${Boost_LIBRARIES})