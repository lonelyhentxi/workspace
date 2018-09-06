project(emcpp)

set(ti type_inference.cpp)
add_executable(ti type_inference.cpp)
# target_link_libraries(ti ${Boost_LIBRARIES})

set(auto auto.cpp)
add_executable(auto auto.cpp)

set(mc modern_cpp.cpp)
add_executable(mc modern_cpp.cpp)

set(sp smart_ptr.cpp)
file(GLOB_RECURSE WIDGET_SOURCE smart_ptr/widget.cpp)
add_executable(sp smart_ptr.cpp ${WIDGET_SOURCE})
target_include_directories(sp PUBLIC smart_ptr)

set(rmf rr_mv_fw.cpp)
add_executable(rmf rr_mv_fw.cpp)