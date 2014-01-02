cmake_minimum_required(VERSION 2.8)

project(ede-compdb-test CXX)

add_library(world world/world.hpp world/world.cpp)
include_directories(world)

add_definitions("-DHELLO_FOO=HelloFoo")
add_definitions("-DBUILD_TYPE=${CMAKE_BUILD_TYPE}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -include ${CMAKE_SOURCE_DIR}/config.hpp")

add_executable(hello hello.cpp)
target_link_libraries(hello world)
