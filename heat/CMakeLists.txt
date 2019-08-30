#
# heat and bmi_heat
#
add_library(heatf SHARED heat.f90)
add_library(bmiheatf SHARED bmi_heat.f90 heat.f90)

add_executable(run_heatf main.f90)
target_link_libraries(run_heatf heatf)

install(
  TARGETS run_heatf
  RUNTIME DESTINATION bin
)
install(
  TARGETS heatf bmiheatf
  LIBRARY DESTINATION lib
)
install(
  FILES
    ${CMAKE_BINARY_DIR}/heat/heat.mod
    ${CMAKE_BINARY_DIR}/heat/bmi_heat.mod
  DESTINATION include
)
