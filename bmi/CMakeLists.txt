cmake_minimum_required(VERSION 2.6)

project (bmi)
enable_language (Fortran)
set (BMI_VERSION 0.2)
set (PKG_NAME bmif90)

configure_file (${CMAKE_CURRENT_SOURCE_DIR}/${PKG_NAME}.pc.cmake
  ${CMAKE_CURRENT_SOURCE_DIR}/${PKG_NAME}.pc)

set (BUILD_SHARED_LIBS ON)

include_directories( ${CMAKE_CURRENT_SOURCE_DIR} )

set (bmi_lib_SRCS
  bmi_params.f90)

add_library (${PKG_NAME} ${bmi_lib_SRCS} )

install(
  TARGETS ${PKG_NAME}
  DESTINATION lib
  COMPONENT bmi)
install(
  FILES ${PKG_NAME}.pc
  DESTINATION lib/pkgconfig)
install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/bmi_params.mod
  DESTINATION lib)

