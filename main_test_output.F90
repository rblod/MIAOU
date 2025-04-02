!===============================================================================
! main_test_output.F90 : Minimal test program for file_manager and variables_registry
!===============================================================================
program main_test_output
   use file_manager
   use variables_registry
   use ocean_var

   implicit none

   integer, parameter :: nx = 10, ny = 8, nz = 5, nt = 10
   integer :: t
   real :: current_time
   character(len=128) :: time_units, calendar

   ! Allocation of test fields
   allocate (zeta(nx, ny))
   allocate (temp(nx, ny, nz))
   allocate (u(nx, ny))
   allocate (v(nx, ny))

   ! Initialization for ocean variables
   zeta = 0.; temp = 0.
   u = 0.; v = 0.

   ! Initialization of model variables
   call init_variables(nx, ny, nz)

   ! Calendar
   time_units = "seconds since 2023-01-01 00:00:00"
   calendar = "gregorian"

   ! Output initialization
   call initialize_output_files(time_units, calendar)

   ! Time loop
   do t = 1, nt
      ! Time in seconds
      current_time = (t - 1)*dt

      ! Fill values for test
      zeta = zeta + 0.1*t
      temp(:, :, 1) = temp(:, :, 1) + 0.2*t
      u = t
      v = t*0.5

      ! Check if this is the last time step
      if (t == nt) then
         ! Write all possible outputs at final step
         call write_all_outputs(current_time, .true.)
      else
         ! Write all possible outputs during normal steps
         call write_all_outputs(current_time, .false.)
      end if
   end do

   ! Finalize outputs
   call close_all_output_files()

   ! Deallocate memory
   deallocate (zeta, temp, u, v)

   print *, "Success!"

end program main_test_output
