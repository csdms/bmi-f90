module bmi_heat

  use bmi_params
  use heat
  implicit none

  character (len=BMI_MAXCOMPNAMESTR), target :: &
       component_name = "The 2D Heat Equation"

contains

  function get_component_name(self, name) result(status)
    type (heat_model), intent (in) :: self
    character (len=*), pointer, intent (out) :: name
    integer :: status

    name => component_name
    status = BMI_SUCCESS
  end function get_component_name

end module bmi_heat
