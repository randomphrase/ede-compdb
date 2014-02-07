cmake_minimum_required(VERSION 2.8)

project(ede-compdb-test CXX)

set(build_type_cpp ${CMAKE_CURRENT_BINARY_DIR}/build_type.cpp)

configure_file(build_type.cpp.in ${build_type_cpp})

add_library(world world/world.hpp world/world.cpp ${build_type_cpp})
include_directories(world)

add_definitions("-DHELLO_FOO=HelloFoo")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -include ${CMAKE_SOURCE_DIR}/config.hpp")

add_executable(hello hello.cpp)
target_link_libraries(hello world)
