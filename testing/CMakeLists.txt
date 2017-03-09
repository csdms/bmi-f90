cmake_minimum_required(VERSION 2.6)

project (bmi)
enable_language (Fortran)
include (CTest)

add_test (BMI_IRF irf_test)
add_test (BMI_GET_VALUES get_value_test)
add_test (BMI_SET_VALUES set_value_test)
add_test (BMI_VARGRID vargrid_test)
add_test (INSTANCE_CONFLICT conflicting_instances_test)

set (BUILD_SHARED_LIBS ON)

include_directories( ${CMAKE_BINARY_DIR}/bmi )
include_directories( ${CMAKE_BINARY_DIR}/heat )

set (bmi_irf_SRCS irf_test.f90 testing_helpers.f90)
set (bmi_get_value_SRCS get_value_test.f90 testing_helpers.f90)
set (bmi_set_value_SRCS set_value_test.f90 testing_helpers.f90)
set (bmi_vargrid_SRCS vargrid_test.f90 testing_helpers.f90)
set (instance_conflict_SRCS conflicting_instances_test.f90 testing_helpers.f90)

add_executable(irf_test ${bmi_irf_SRCS})
add_executable(get_value_test ${bmi_get_value_SRCS})
add_executable(set_value_test ${bmi_set_value_SRCS})
add_executable(vargrid_test ${bmi_vargrid_SRCS})
add_executable(conflicting_instances_test ${instance_conflict_SRCS})

file (
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/test1.cfg
  DESTINATION ${CMAKE_BINARY_DIR}/testing)
file (
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/test2.cfg
  DESTINATION ${CMAKE_BINARY_DIR}/testing)

target_link_libraries(irf_test bmif90 bmi_heatf90)
target_link_libraries(get_value_test bmif90 bmi_heatf90)
target_link_libraries(set_value_test bmif90 bmi_heatf90)
target_link_libraries(vargrid_test bmif90 bmi_heatf90)
target_link_libraries(conflicting_instances_test bmif90 bmi_heatf90)
