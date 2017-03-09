! Do two instances of heat_model conflict?
program conflicting_instances_test

  use bmi_heat
  implicit none

  type (heat_model) :: m1
  type (heat_model) :: m2
  character (len=BMI_MAXVARNAMESTR) :: &
       cfg_file1 = "test1.cfg", cfg_file2 = "test2.cfg"
  integer :: s
  integer :: grid_id1, grid_id2
  character (len=BMI_MAXVARNAMESTR), pointer :: names1(:), names2(:)
  integer :: dims1(2), dims2(2)
  real, pointer :: z1(:), z2(:)
  character(len=30) :: rowfmt1, rowfmt2

  write(*, "(a, a10, a10)") "Configuration files: ", cfg_file1, cfg_file2

  write (*,"(a)",advance="no") "Initializing..."
  s = initialize(m1, cfg_file1)
  s = initialize(m2, cfg_file2)
  write (*,*) "Done."

  s = get_output_var_names(m1, names1)
  s = get_var_grid(m1, names1(1), grid_id1)
  s = get_grid_shape(m1, grid_id1, dims1)
  write(rowfmt1,'(a,i4,a)') '(', dims1(2), '(1x,f6.1))'

  s = get_output_var_names(m2, names2)
  s = get_var_grid(m2, names2(1), grid_id2)
  s = get_grid_shape(m2, grid_id2, dims2)
  write(rowfmt2,'(a,i4,a)') '(', dims2(2), '(1x,f6.1))'

  write (*, "(a)") "Initial values, model 1:"
  s = get_value(m1, "plate_surface__temperature", z1)
  call print_array(z1, dims1)

  write (*, "(a)") "Initial values, model 2:"
  s = get_value(m2, "plate_surface__temperature", z2)
  call print_array(z2, dims2)

  write (*, "(a)") "Set new value in model 2; does it affect model 1?"
  s = set_value_at_indices(m2, "plate_surface__temperature", &
       (/20/), (/42.0/))
  write (*, "(a)") "New values, model 2:"
  s = get_value(m2, "plate_surface__temperature", z2)
  call print_array(z2, dims2)
  write (*, "(a)") "New values, model 1:"
  s = get_value(m1, "plate_surface__temperature", z1)
  call print_array(z1, dims1)

  write (*, "(a)") "Update both models by one time step..."
  s = update(m1)
  s = update(m2)
  write (*, "(a)") "Updated values, model 1:"
  s = get_value(m1, "plate_surface__temperature", z1)
  call print_array(z1, dims1)
  write (*, "(a)") "Updated values, model 2:"
  s = get_value(m2, "plate_surface__temperature", z2)
  call print_array(z2, dims2)

  write (*,"(a)", advance="no") "Finalizing..."
  s = finalize(m1)
  s = finalize(m2)
  write (*,*) "Done"

end program conflicting_instances_test

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
