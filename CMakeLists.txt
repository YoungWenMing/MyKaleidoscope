#指定需要CMAKE的最小版本
cmake_minimum_required(VERSION 3.4.1)

project(Kaleidoscope)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
list(APPEND CMAKE_MODULE_PATH "${LLVM_DIR}")

message("LLVM_DIR : " ${LLVM_DIR})

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)

message("LLVM STATUS:
  Definitions ${LLVM_DEFINITIONS}
  Includes    ${LLVM_INCLUDE_DIRS}
  Libraries   ${LLVM_LIBRARY_DIRS}
  Targets     ${LLVM_TARGETS_TO_BUILD}"
)

# Now set the LLVM header and library paths:
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})
add_definitions(${LLVM_DEFINITIONS})


llvm_map_components_to_libnames(REQ_LLVM_LIBRARIES
  Analysis
  Core
  ExecutionEngine
  InstCombine
  Object
  OrcJIT
  RuntimeDyld
  ScalarOpts
  Support
  native
)

# add_compile_options(-fexceptions)
if (CMAKE_BUILD_TYPE MATCHES "Debug")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -O0 -g ")
else()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -O2")
endif()

message("build type: ${CMAKE_BUILD_TYPE} ; cxx flags: ${CMAKE_CXX_FLAGS}")

set(CMAKE_CXX_COMPILER "/usr/bin/g++")

# if ((CMAKE_BUILD_TYPE MATCHES "Debug"))

#     add_definitions(-DDEBUG)

# endif()

set(EXECUTABLE_OUTPUT_PATH "${CMAKE_SOURCE_DIR}/bin")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/bin")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")


include_directories(
  ${CMAKE_SOURCE_DIR}/
  ${CMAKE_SOURCE_DIR}/build-include/
  ${CMAKE_SOURCE_DIR}/llvm-include/
  # ${CMAKE_SOURCE_DIR}/include
  # ${CMAKE_SOURCE_DIR}/include
)

set(myparser_src
  ${CMAKE_SOURCE_DIR}/src/token.h
  ${CMAKE_SOURCE_DIR}/src/token-inl.h
  ${CMAKE_SOURCE_DIR}/src/token.cc
  ${CMAKE_SOURCE_DIR}/src/lexer.cc
  ${CMAKE_SOURCE_DIR}/src/lexer.h
  ${CMAKE_SOURCE_DIR}/src/script.h
  ${CMAKE_SOURCE_DIR}/src/script.cc
  ${CMAKE_SOURCE_DIR}/src/ast.h
  ${CMAKE_SOURCE_DIR}/src/ast.cc
  ${CMAKE_SOURCE_DIR}/src/ast-printer.h
  ${CMAKE_SOURCE_DIR}/src/ast-printer.cc
  ${CMAKE_SOURCE_DIR}/src/lexer-inl.h
  ${CMAKE_SOURCE_DIR}/src/parser.h
  ${CMAKE_SOURCE_DIR}/src/parser.cc
  ${CMAKE_SOURCE_DIR}/src/Codegen.h
  ${CMAKE_SOURCE_DIR}/src/Codegen.cc
  ${CMAKE_SOURCE_DIR}/src/Codegen-inl.h
  
  ${CMAKE_SOURCE_DIR}/src/global.h
  ${CMAKE_SOURCE_DIR}/src/logging.cc
  ${CMAKE_SOURCE_DIR}/src/util.h
  ${CMAKE_SOURCE_DIR}/src/util.cc

)

add_executable(cctest
  ${myparser_src}
  ${CMAKE_SOURCE_DIR}/cctest/test_main.cc
)


target_link_libraries(cctest
  ${REQ_LLVM_LIBRARIES}
)