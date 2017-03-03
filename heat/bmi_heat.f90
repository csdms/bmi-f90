module bmi_heat

  use bmif
  use heat
  implicit none

  character (len=BMI_MAXCOMPNAMESTR), target :: &
       component_name = "The 2D Heat Equation"

end module bmi_heat
