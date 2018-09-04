project(emcpp)

set(ti type_inference.cpp)
add_executable(ti type_inference.cpp)
# target_link_libraries(ti ${Boost_LIBRARIES})

set(auto auto.cpp)
add_executable(auto auto.cpp)

set(mc modern_cpp.cpp)
add_executable(mc modern_cpp.cpp)

set(sp smart_ptr.cpp)
add_executable(sp smart_ptr.cpp)