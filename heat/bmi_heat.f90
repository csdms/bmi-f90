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

  private allocate_flattened_array

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

  ! Get the grid identifier for the given variable.
  function get_var_grid(self, var_name, grid_id) result(status)
    type (heat_model), intent (in) :: self
    character (len=*), pointer, intent (in) :: var_name
    integer, intent (out) :: grid_id
    integer :: status

    if (var_name == output_items(1)) then
       grid_id = 0
       status = BMI_SUCCESS
    else
       grid_id = -1
       status = BMI_FAILURE
    end if
  end function get_var_grid

  ! Get memory use per array element, in bytes.
  function get_var_itemsize(self, var_name, size) result(status)
    type (heat_model), intent (in) :: self
    character (len=*), pointer, intent (in) :: var_name
    integer, intent (out) :: size
    integer :: status

    if (var_name == output_items(1)) then
       size = BMI_DOUBLE
       status = BMI_SUCCESS
    else
       size = 0
       status = BMI_FAILURE
    end if
  end function get_var_itemsize

  ! Get size, in bytes, of the given variable.
  function get_var_nbytes(self, var_name, nsize) result(status)
    type (heat_model), intent (in) :: self
    character (len=*), pointer, intent (in) :: var_name
    integer, intent (out) :: nsize
    integer :: s1, s2, s3, grid_id, grid_size, item_size, status

    s1 = get_var_grid(self, var_name, grid_id)
    s2 = get_grid_size(self, grid_id, grid_size)
    s3 = get_var_itemsize(self, var_name, item_size)

    if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
       nsize = item_size * grid_size
       status = BMI_SUCCESS
    else
       nsize = -1
       status = BMI_FAILURE
    end if
  end function get_var_nbytes

  ! Get the data type of the given variable as a string.
  function get_var_type(self, var_name, type_name) result(status)
    type (heat_model), intent (in) :: self
    character (len=*), pointer, intent (in) :: var_name
    character (len=BMI_MAXVARNAMESTR), intent (out) :: type_name
    integer :: status

    if (var_name == output_items(1)) then
       type_name = "double"
       status = BMI_SUCCESS
    else
       type_name = "-"
       status = BMI_FAILURE
    end if
  end function get_var_type

  ! Get the units of the given variable.
  function get_var_units(self, var_name, units) result(status)
    type (heat_model), intent (in) :: self
    character (len=*), pointer, intent (in) :: var_name
    character (len=BMI_MAXVARNAMESTR), intent (out) :: units
    integer :: status

    if (var_name == output_items(1)) then
       units = "K"
       status = BMI_SUCCESS
    else
       units = "-"
       status = BMI_FAILURE
    end if
  end function get_var_units

  ! Get the grid type as a string.
  function get_grid_type(self, grid_id, grid_type) result(status)
    type (heat_model), intent (in) :: self
    integer, intent (in) :: grid_id
    character (len=BMI_MAXVARNAMESTR), intent (out) :: grid_type
    integer :: status

    if (grid_id == 0) then
       grid_type = "uniform_rectilinear"
       status = BMI_SUCCESS
    else
       grid_type = "-"
       status = BMI_FAILURE
    end if
  end function get_grid_type

  ! Get coordinates for the origin of the computational grid.
  function get_grid_origin(self, grid_id, origin) result(status)
    type (heat_model), intent (in) :: self
    integer, intent (in) :: grid_id
    real, dimension(:), intent (out) :: origin
    integer :: status

    if (grid_id == 0) then
       origin = (/0.0, 0.0/)
    end if

    status = BMI_SUCCESS
  end function get_grid_origin

  ! Get number of dimensions of the computational grid.
  function get_grid_rank(self, grid_id, rank) result(status)
    type (heat_model), intent (in) :: self
    integer, intent (in) :: grid_id
    integer, intent (out) :: rank
    integer :: status

    if (grid_id == 0) then
       rank = 2
       status = BMI_SUCCESS
    else
       rank = -1
       status = BMI_FAILURE
    end if
  end function get_grid_rank

  ! Get the dimensions of the computational grid.
  function get_grid_shape(self, grid_id, shape) result(status)
    type (heat_model), intent (in) :: self
    integer, intent (in) :: grid_id
    integer, dimension(:), intent (out) :: shape
    integer :: status

    if (grid_id == 0) then
       shape = (/self%n_y, self%n_x/)
    end if

    status = BMI_SUCCESS
  end function get_grid_shape

  ! Get the total number of elements in the computational grid.
  function get_grid_size(self, grid_id, nsize) result(status)
    type (heat_model), intent (in) :: self
    integer, intent (in) :: grid_id
    integer, intent (out) :: nsize
    integer :: status

    if (grid_id == 0) then
       nsize = self%n_y * self%n_x
       status = BMI_SUCCESS
    else
       nsize = -1
       status = BMI_FAILURE
    end if
  end function get_grid_size

  ! Get distance between nodes of the computational grid.
  function get_grid_spacing(self, grid_id, spacing) result(status)
    type (heat_model), intent (in) :: self
    integer, intent (in) :: grid_id
    real, dimension(:), intent (out) :: spacing
    integer :: status

    if (grid_id == 0) then
       spacing = (/self%dy, self%dx/)
    end if

    status = BMI_SUCCESS
  end function get_grid_spacing

  ! Get a copy of values (flattened!) of the given variable.
  function get_value(self, var_name, dest) result(status)
    type (heat_model), intent (in) :: self
    character (len=*), intent (in) :: var_name
    real, pointer, intent (inout) :: dest(:)
    integer :: n_elements, status

    select case (var_name)
    case ('plate_surface__temperature')
       n_elements = self%n_y * self%n_x
       call allocate_flattened_array(dest, n_elements)
       dest = reshape(self%temperature, (/n_elements/))
       status = BMI_SUCCESS
    case default
       n_elements = 1
       call allocate_flattened_array(dest, n_elements)
       dest = -1.0
       status = BMI_FAILURE
    end select
  end function get_value

  ! Get a reference to the values (flattened!) of the given variable.
  function get_value_ref(self, var_name, dest) result(status)
    type (heat_model), intent (in) :: self
    character (len=*), pointer, intent (in) :: var_name
    real, pointer, intent (inout) :: dest(:)
    integer :: status

    ! Todo. I don't think I can do this in F9X. I need the
    ! iso_c_bindings module from F03.

    status = BMI_FAILURE
  end function get_value_ref

  ! Get values at particular (one-dimensional) indices.
  function get_value_at_indices(self, var_name, dest, indices) result(status)
    type (heat_model), intent (in) :: self
    character (len=*), intent (in) :: var_name
    real, pointer, intent (inout) :: dest(:)
    integer, intent (in) :: indices(:)
    integer :: i, j, k, status

    select case (var_name)
    case ('plate_surface__temperature')
       call allocate_flattened_array(dest, size(indices))
       do k = 1, size(indices)
          j = mod((indices(k)-1), int(self%n_y)) + 1
          i = (indices(k)-1)/int(self%n_y) + 1
          dest(k) = self%temperature(j,i)
       end do
       status = BMI_SUCCESS
    case default
       call allocate_flattened_array(dest, 1)
       dest = -1.0
       status = BMI_FAILURE
    end select

  end function get_value_at_indices

  ! A helper routine to allocate a flattened array.
  subroutine allocate_flattened_array(array, n)
    real, pointer, intent (inout) :: array(:)
    integer, intent (in) :: n

    if (.not.associated(array)) then
       allocate(array(n))
    end if
  end subroutine allocate_flattened_array

end module bmi_heat
