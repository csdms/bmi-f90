! Test the BMI get_var_* and get_grid_* functions.
program vargrid_test

  use bmi_heat
  implicit none

  type (heat_model) :: m
  integer :: s, i
  character (len=BMI_MAXVARNAMESTR), pointer :: names(:)
  character (len=BMI_MAXVARNAMESTR) :: astring
  integer :: grid_id, asize

  write (*,"(a)",advance="no") "Initializing..."
  s = initialize(m, "")
  write (*,*) "Done."

  s = get_output_var_names(m, names)
  write (*,"(a30)") "Output variables:"
  do i = 1, size(names)
     write (*,"(a30, 1x, a)") "-", names(i)
  end do

  s = get_var_grid(m, names(1), grid_id)
  write (*,"(a30, i3)") "Grid id:", grid_id
  s = get_var_itemsize(m, names(1), asize)
  write (*,"(a30, i3, 1x, a)") "Item size:", asize, "bytes"
  ! s = get_var_nbytes(m, names(1), asize)
  ! write (*,"(a30, i3, 1x, a)") "Variable size:", asize, "bytes"
  s = get_var_type(m, names(1), astring)
  write (*,"(a30, 1x, a)") "Variable type:", astring
  s = get_var_units(m, names(1), astring)
  write (*,"(a30, 1x, a)") "Variable units:", astring

  write (*,"(a)", advance="no") "Finalizing..."
  s = finalize(m)
  write (*,*) "Done"

end program vargrid_test
