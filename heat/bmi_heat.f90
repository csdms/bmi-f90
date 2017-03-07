module bmi_heat

  use bmi_params
  use heat
  implicit none

  character (len=BMI_MAXCOMPNAMESTR), target :: &
       component_name = "The 2D Heat Equation"

  ! Exchange items
  integer, parameter :: input_item_count = 1
  integer, parameter :: output_item_count = 1
  character (len=BMI_MAXVARNAMESTR), target, &
       dimension (input_item_count) :: &
       input_items = (/'plate_surface__temperature'/)
  character (len=BMI_MAXVARNAMESTR), target, &
       dimension (output_item_count) :: &
       output_items = (/'plate_surface__temperature'/)

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

  ! List a model's input variables.
  function get_input_var_names(self, names) result(status)
    type (heat_model), intent (in) :: self
    character (*), pointer, intent (out) :: names(:)
    integer :: status

    names => input_items
    status = BMI_SUCCESS
  end function get_input_var_names

  ! List a model's output variables.
  function get_output_var_names(self, names) result(status)
    type (heat_model), intent (in) :: self
    character (*), pointer, intent (out) :: names(:)
    integer :: status

    names => output_items
  end function get_output_var_names

  ! Start time of the model.
  function get_start_time(self, time) result(status)
    type (heat_model), intent (inout) :: self
    real, intent (out) :: time
    integer :: status

    time = 0.0

    status = BMI_SUCCESS
  end function get_start_time

  ! End time of the model.
  function get_end_time(self, time) result(status)
    type (heat_model), intent (inout) :: self
    real, intent (out) :: time
    integer :: status

    time = self%t_end

    status = BMI_SUCCESS
  end function get_end_time

  ! Current time of the model.
  function get_current_time(self, time) result(status)
    type (heat_model), intent (inout) :: self
    real, intent (out) :: time
    integer :: status

    time = self%t

    status = BMI_SUCCESS
  end function get_current_time

  ! Time step of the model.
  function get_time_step(self, time_step) result(status)
    type (heat_model), intent (inout) :: self
    real, intent (out) :: time_step
    integer :: status

    time_step = self%dt

    status = BMI_SUCCESS
  end function get_time_step

  ! Time unit of the model.
  function get_time_units(self, time_units) result(status)
    type (heat_model), intent (inout) :: self
    character (len=*), intent (out) :: time_units
    integer :: status

    time_units = "-"

    status = BMI_SUCCESS
  end function get_time_units

  ! Advance the model one time step.
  function update(self) result(status)
    type (heat_model), intent (inout) :: self
    integer :: status

    call advance_in_time(self)

    status = BMI_SUCCESS
  end function update

  ! Advance the model by a fraction of a time step.
  function update_frac(self, time_frac) result(status)
    type (heat_model), intent (inout) :: self
    real, intent (in) :: time_frac
    real :: time_step
    integer :: status

    if (time_frac > 0.0) then
       time_step = self%dt
       self%dt = time_step*time_frac
       call advance_in_time(self)
       self%dt = time_step
    end if

    status = BMI_SUCCESS
  end function update_frac

  ! Advance the model until the given time.
  function update_until(self, time) result(status)
    type (heat_model), intent (inout) :: self
    real, intent (in) :: time
    integer :: status, n_steps, i, s
    real :: n_steps_real

    if (time > self%t) then
       n_steps_real = (time - self%t) / self%dt
       n_steps = floor(n_steps_real)
       do i = 1, n_steps
          s = update(self)
       end do
       s = update_frac(self, n_steps_real - real(n_steps))
    end if

    status = BMI_SUCCESS
  end function update_until

end module bmi_heat
