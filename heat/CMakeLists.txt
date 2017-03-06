cmake_minimum_required (VERSION 2.6)

project (heat)
enable_language (Fortran)
set (HEAT_VERSION 0.1)
set (PKG_NAME heatf90)

configure_file (
  ${CMAKE_CURRENT_SOURCE_DIR}/${PKG_NAME}.pc.cmake
  ${CMAKE_CURRENT_SOURCE_DIR}/${PKG_NAME}.pc)

set (BUILD_SHARED_LIBS ON)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})
include_directories (${CMAKE_BINARY_DIR}/bmi)

set (bmi_heat_lib_sources bmi_heat.f90 heat.f90)
add_library (bmi_${PKG_NAME} ${bmi_heat_lib_sources})

set (heat_sources main.f90 heat.f90)
add_executable (run_${PKG_NAME} ${heat_sources})

install (
  PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run_${PKG_NAME}
  DESTINATION bin
  COMPONENT ${PKG_NAME})
install (
  TARGETS bmi_${PKG_NAME}
  DESTINATION lib
  COMPONENT ${PKG_NAME})
install (
  FILES ${PKG_NAME}.pc
  DESTINATION lib/pkgconfig)
install (
  FILES ${CMAKE_CURRENT_BINARY_DIR}/heat.mod
  DESTINATION lib)
