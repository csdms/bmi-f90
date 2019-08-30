#
# testing
#
include(CTest)
set(BUILD_SHARED_LIBS ON)
include_directories(${CMAKE_BINARY_DIR}/heat)

add_test(BMI_IRF irf_test)
add_test(BMI_GET_VALUES get_value_test)
add_test(BMI_SET_VALUES set_value_test)
add_test(BMI_VARGRID vargrid_test)
add_test(INSTANCE_CONFLICT conflicting_instances_test)

set(bmi_irf_src irf_test.f90 testing_helpers.f90)
set(bmi_get_value_src get_value_test.f90 testing_helpers.f90)
set(bmi_set_value_src set_value_test.f90 testing_helpers.f90)
set(bmi_vargrid_src vargrid_test.f90 testing_helpers.f90)
set(instance_conflict_src conflicting_instances_test.f90 testing_helpers.f90)

add_executable(irf_test ${bmi_irf_src})
add_executable(get_value_test ${bmi_get_value_src})
add_executable(set_value_test ${bmi_set_value_src})
add_executable(vargrid_test ${bmi_vargrid_src})
add_executable(conflicting_instances_test ${instance_conflict_src})

target_link_libraries(irf_test bmi${model_name})
target_link_libraries(get_value_test bmi${model_name})
target_link_libraries(set_value_test bmi${model_name})
target_link_libraries(vargrid_test bmi${model_name})
target_link_libraries(conflicting_instances_test bmi${model_name})

file (
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/test1.cfg
  DESTINATION ${CMAKE_BINARY_DIR}/testing)
file (
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/test2.cfg
  DESTINATION ${CMAKE_BINARY_DIR}/testing)
