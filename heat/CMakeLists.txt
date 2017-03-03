cmake_minimum_required (VERSION 2.6)

project (heat)
enable_language (Fortran)
set (HEAT_VERSION 0.1)

configure_file (${CMAKE_CURRENT_SOURCE_DIR}/heat.pc.cmake
  ${CMAKE_CURRENT_SOURCE_DIR}/heat.pc)

set (BUILD_SHARED_LIBS ON)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})

set (heat_lib_sources heat.f90)
add_library (heat ${heat_lib_sources})

set (heat_sources main.f90 heat.f90)
add_executable (run_heat ${heat_sources})

install (
  PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run_heat
  DESTINATION bin
  COMPONENT heat)
install (
  TARGETS heat
  DESTINATION lib
  COMPONENT heat)
install (
  FILES heat.pc
  DESTINATION lib/pkgconfig)
install (
  FILES ${CMAKE_CURRENT_BINARY_DIR}/heat.mod
  DESTINATION lib)
