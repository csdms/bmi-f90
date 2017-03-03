cmake_minimum_required (VERSION 2.6)

project (heat)
enable_language (Fortran)
set (HEAT_VERSION 0.1)

configure_file (
  ${CMAKE_CURRENT_SOURCE_DIR}/heatf.pc.cmake
  ${CMAKE_CURRENT_SOURCE_DIR}/heatf.pc)

set (BUILD_SHARED_LIBS ON)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})
include_directories (${CMAKE_BINARY_DIR}/bmi)

set (bmi_heat_lib_sources bmi_heat.f90 heat.f90)
add_library (bmi_heatf ${bmi_heat_lib_sources})

set (heat_sources main.f90 heat.f90)
add_executable (run_heatf ${heat_sources})

install (
  PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run_heatf
  DESTINATION bin
  COMPONENT heatf)
install (
  TARGETS bmi_heatf
  DESTINATION lib
  COMPONENT heatf)
install (
  FILES heatf.pc
  DESTINATION lib/pkgconfig)
install (
  FILES ${CMAKE_CURRENT_BINARY_DIR}/heat.mod
  DESTINATION lib)
