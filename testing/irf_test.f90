program irf_test

  use bmi_heat
  implicit none

  type (heat_model) :: m
  integer :: s
  real :: time, time0, time1
  integer :: i
  character (len=BMI_MAXUNITSSTR) :: time_units
  character (len=BMI_MAXVARNAMESTR), pointer :: names(:)
  character (len=BMI_MAXCOMPNAMESTR), pointer :: name

  write (*,"(a)",advance="no") "Initializing..."
  s = initialize(m, "")
  write (*,*) "Done."

  write (*,"(a)") "Component info"

  s = get_component_name(m, name)
  write (*,"(a30, a30)") "Component name: ", name

  s = get_start_time(m, time0)
  write (*,"(a30, f8.2)") "Start time: ", time0
  s = get_end_time(m, time1)
  write (*,"(a30, f8.2)") "End time: ", time1
  s = get_current_time(m, time)
  write (*,"(a30, f8.2)") "Current time: ", time
  s = get_time_step(m, time)
  write (*,"(a30, f8.2)") "Time step: ", time
  s = get_time_units(m, time_units)
  write (*,"(a30, a)") "Time units: ", time_units

  s = get_input_var_names(m, names)
  write (*,"(a30, a)") "Input variables: ", names
  s = get_output_var_names(m, names)
  write (*,"(a30, a)") "Output variables: ", names

  write (*,"(a)") "Update one time step"
  s = get_current_time(m, time)
  write (*,"(a30, f8.2)") "Start time: ", time
  s = update(m)
  s = get_current_time(m, time)
  write (*,"(a30, f8.2)") "Stop time: ", time

  write (*,"(a)") "Running..."
  do i = 1, 10
     write (*,"(a30, i3)") "Time step:", i
     s = update(m)
  end do

  write (*,"(a)") "Update a fraction of a time step"
  s = get_current_time(m, time0)
  write (*,"(a30, f8.2)") "Start time: ", time0
  s = update_frac(m, 0.5)
  s = get_current_time(m, time1)
  write (*,"(a30, f8.2)") "Stop time: ", time1

  s = get_end_time(m, time)
  write (*,"(a, f8.2)") "Running until... ", time
  s = get_current_time(m, time0)
  write (*,"(a30, f8.2)") "Start time: ", time0
  s = update_until(m, time)
  s = get_current_time(m, time1)
  write (*,"(a30, f8.2)") "Stop time: ", time1

  write (*,"(a)", advance="no") "Finalizing..."
  s = finalize(m)
  write (*,*) "Done"

end program irf_test
