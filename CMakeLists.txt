#
# bmif90
#
cmake_minimum_required(VERSION 2.8)

project(bmif90 Fortran)

if (APPLE)
  set(CMAKE_MACOSX_RPATH TRUE)
endif()

add_subdirectory(heat)
# add_subdirectory(testing)

include(CTest)

