! Test the get_value, get_value_ref, and get_value_at_indices functions.
program get_value_test

  use bmi_heat
  implicit none

  type (heat_model) :: m
  integer :: s, i, j, grid_id
  character (len=BMI_MAXVARNAMESTR), pointer :: names(:)
  integer :: dims(2), locations(3)
  real, pointer :: z(:), y(:)
  character(len=30) :: rowfmt

  write (*,"(a)",advance="no") "Initializing..."
  s = initialize(m, "")
  write (*,*) "Done."

  s = get_output_var_names(m, names)
  write (*,"(a, a)") "Output variables: ", names

  s = get_var_grid(m, names(1), grid_id)
  s = get_grid_shape(m, grid_id, dims)
  write(rowfmt,'(a,i4,a)') '(', dims(2), '(1x,f6.1))'

  write (*, "(a)") "Initial values:"
  s = get_value(m, "plate_surface__temperature", z)
  call print_array(z, dims)
  write (*, "(a, i5)") "Shape of returned values:", shape(z)

  write (*,"(a)") "Running..."
  do j = 1, 4
     s = update(m)
     s = get_value(m, "plate_surface__temperature", z)
     write (*,*) "Values at time:", j
     call print_array(z, dims)
  end do
  write (*,"(a)") "Done."

  write (*, "(a)") "Values at three locations:"
  locations = (/21, 41, 62/)
  write (*,*) "Locations: ", locations
  s = get_value_at_indices(m, "plate_surface__temperature", y, locations)
  write (*,*) "Values: ", y

  write (*,"(a)", advance="no") "Finalizing..."
  s = finalize(m)
  write (*,*) "Done"

end program get_value_test

! A helper to print the retrived array to the console.
subroutine print_array(array, dims)
  integer :: dims(2)
  real, dimension(product(dims)) :: array
  integer :: i, j

  do j = 1, dims(1)
     do i = 1, dims(2)
        write (*,"(f6.1)", advance="no") array(j + dims(1)*(i-1))
     end do
     write (*,*)
  end do
end subroutine print_array
