cmake_minimum_required(VERSION 3.10)
project(Athena)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wpedantic")

find_program(CPPCHECK NAMES cppcheck)

if(CPPCHECK)
    # Get all source files in your project
    file(GLOB_RECURSE ALL_SOURCE_FILES ${CMAKE_SOURCE_DIR}/*.cpp ${CMAKE_SOURCE_DIR}/*.h)

    # Create cppcheck custom target
    add_custom_target(cppcheck
        COMMAND ${CPPCHECK}
        --enable=warning,performance,portability,style --inconclusive
        --library=posix --library=gnu --std=c++23
        --verbose --template=gcc
        ${ALL_SOURCE_FILES}
        COMMENT "Running cppcheck"
    )
else()
    message(WARNING "cppcheck not found!")
endif()



set(CMAKE_BUILD_TYPE Debug)

set(CMAKE_CXX_COMPILER "/opt/homebrew/Cellar/llvm/17.0.4/bin/clang++")


add_executable(Athena main.cpp)

# target_compile_options(Athena PRIVATE -fsanitize=undefined)
# target_link_options(Athena PRIVATE -fsanitize=undefined)
target_compile_options(Athena PRIVATE -fsanitize=address)
target_link_options(Athena PRIVATE -fsanitize=address)


set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/build")
