! Run the heat model through its BMI.
program bmi_main

  use bmi_heat
  use, intrinsic :: iso_fortran_env, only : file_unit=>input_unit
  implicit none

  character (len=*), parameter :: output_file = "bmiheatf90.out"
  character (len=*), parameter :: var_name = "plate_surface__temperature"
  integer, parameter :: ndims = 2

  type (heat_model) :: model
  integer :: arg_count = 0
  character (len=80) :: arg
  integer :: i, j, s, grid_id, grid_size, grid_shape(ndims)
  real :: current_time, end_time
  real, pointer :: temperature(:)

  do while (arg_count <= 1)
    call get_command_argument(arg_count, arg)
    arg_count = arg_count + 1
  end do

  if (len_trim(arg) == 0) then
     write(*,"(a)") "Usage: run_bmiheatf90 CONFIGURATION_FILE"
     write(*,"(a)")
     write(*,"(a)") "Run the heatf90 model through its BMI with a configuration file."
     write(*,"(a)") "Output is written to the file `bmiheatf90.out`."
     stop
  end if

  open(file_unit,file=output_file)

  write(file_unit,"(a)") "Initialize model."
  s = initialize(model, arg)

  s = get_current_time(model, current_time)
  s = get_end_time(model, end_time)
  s = get_var_grid(model, var_name, grid_id)
  s = get_grid_size(model, grid_id, grid_size)
  s = get_grid_shape(model, grid_id, grid_shape)

  do while (current_time <= end_time)
     write(file_unit,"(a, f6.1)") "Model values at time = ", current_time
     s = get_value(model, var_name, temperature)
     do j = 1, grid_shape(1)
        do i = 1, grid_shape(2)
           write (file_unit,"(f6.1)", advance="no") temperature(j + grid_shape(1)*(i-1))
        end do
        write (file_unit,*)
     end do
     s = update(model)
     s = get_current_time(model, current_time)
  end do

  s = finalize(model)
  write(file_unit,"(a)") "Finalize model."

  close(file_unit)

end program bmi_main
