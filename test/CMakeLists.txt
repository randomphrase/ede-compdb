cmake_minimum_required(VERSION 2.8)

project(ede-ninja-test CXX)

add_library(world world.hpp world.cpp)

add_executable(hello hello.cpp)
