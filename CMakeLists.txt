cmake_minimum_required(VERSION 3.10)
project(Athena)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wpedantic")

set(CMAKE_CXX_COMPILER "/opt/homebrew/Cellar/llvm/17.0.2/bin/clang++")

set(CMAKE_BUILD_TYPE Release)

add_executable(Athena main.cpp)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/build")
