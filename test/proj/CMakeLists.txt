cmake_minimum_required(VERSION 2.8.5)

project(ede-compdb-test CXX)

set(build_type_cpp ${CMAKE_CURRENT_BINARY_DIR}/build_type.cpp)

configure_file(build_type.cpp.in ${build_type_cpp})

add_library(world world/world.cpp ${build_type_cpp})
include_directories(world)
include_directories(utility)

add_definitions("-DHELLO_FOO=HelloFoo")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -include ${CMAKE_SOURCE_DIR}/config.hpp")

add_executable(hello main.cpp)
target_link_libraries(hello world)
