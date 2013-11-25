cmake_minimum_required(VERSION 2.8)

project(ede-ninja-test CXX)

add_library(world world/world.hpp world/world.cpp)
include_directories(world)

add_executable(hello hello.cpp)
