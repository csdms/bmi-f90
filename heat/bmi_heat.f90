module bmi_heat

  use bmi_params
  use heat
  implicit none

  character (len=BMI_MAXCOMPNAMESTR), target :: &
       component_name = "The 2D Heat Equation"

contains

  ! Perform startup tasks for the model.
  function initialize(self, config_file) result(status)
    type (heat_model), intent (out) :: self
    character (len=*), intent (in) :: config_file
    integer :: status

    if (len (config_file) > 0) then
       call initialize_from_file(self, config_file)
    else
       call initialize_from_defaults(self)
    end if

    status = BMI_SUCCESS
  end function initialize

  ! Perform tear-down tasks for the model.
  function finalize(self) result(status)
    type (heat_model), intent (inout) :: self
    integer :: status

    call cleanup(self)

    status = BMI_SUCCESS
  end function finalize

  ! Get the name of the model.
  function get_component_name(self, name) result(status)
    type (heat_model), intent (in) :: self
    character (len=*), pointer, intent (out) :: name
    integer :: status

    name => component_name
    status = BMI_SUCCESS
  end function get_component_name

end module bmi_heat
