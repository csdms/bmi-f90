#
# heat and bmi_heat
#
add_library(${model_name} SHARED heat.f90)
add_library(bmi${model_name} SHARED bmi_heat.f90 heat.f90)

add_executable(run_${model_name} main.f90)
target_link_libraries(run_${model_name} ${model_name})

add_executable(run_bmi${model_name} bmi_main.f90)
target_link_libraries(run_bmi${model_name} bmi${model_name} ${model_name})

install(
  TARGETS run_${model_name} run_bmi${model_name}
  RUNTIME DESTINATION bin
)
install(
  TARGETS ${model_name} bmi${model_name}
  LIBRARY DESTINATION lib
)
install(
  FILES
    ${CMAKE_BINARY_DIR}/heat/heat.mod
    ${CMAKE_BINARY_DIR}/heat/bmi_heat.mod
  DESTINATION include
)
