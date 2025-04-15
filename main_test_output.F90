!===============================================================================
!> @file main_test_output.F90
!>
!> Test program for multi-dimensional output system
!>
!> This program demonstrates the capabilities of the enhanced output system
!> by creating and writing variables of different dimensions (0D to 3D).
!> It simulates a time loop with increasing values and writes outputs at
!> specified frequencies.
!>
!> @author Rachid Benshila
!> @date 2025-04-11
!===============================================================================
program main_test_output
   use var_registry
   use io_manager
   use ocean_var
   use grid_module

   implicit none

   !> Domain size and time steps
   integer, parameter :: nx = 10, ny = 8, nz = 5, nt = 10
   integer :: t, status
   real :: current_time
   character(len=128) :: time_units, calendar

   ! Allocation of test fields
   allocate(zeta(nx, ny))
   allocate(temp(nx, ny, nz))
   allocate(u(nx, ny))
   allocate(v(nx, ny))
   allocate(temp_profile(nz))

   ! Initialization for ocean variables
   zeta = 0.0
   temp = 0.0
   u = 0.0
   v = 0.0
   temp_profile = 0.0
   wind_speed = 0.0

   ! Initialize model variables and register them for output
  ! call init_variables(nx, ny, nz)

   ! Calendar settings
   time_units = "seconds since 2023-01-01 00:00:00"
   calendar = "gregorian"

   ! Initialize I/O system with configuration
   status = initialize_io("output_config.nml", time_units, calendar)
   if (status /= 0) then
      print *, "Warning: I/O initialization failed with status", status
   end if

   call init_variables(nx, ny, nz)


   ! Time loop
   do t = 1, nt
      ! Time in seconds
      current_time = (t)*dt

      ! Fill values for test - all dimensions
      zeta = zeta + 0.1*t                     ! 2D field: free surface
      temp = temp + 0.2*t                     ! 3D field: temperature
      temp_profile = temp_profile + 0.5*t     ! 1D profile: vertical temperature
      wind_speed = 5.0 + 0.1*t                ! 0D scalar: wind speed
      u = t                                   ! 2D field: x-velocity
      v = t*0.5                               ! 2D field: y-velocity


        ! Debug pour le fichier spécifique
   if (abs(current_time - 18000.0) < 10.0) then
      print *, "At time ", current_time, " v(1,1) = ", v(1,1)
   end if

      ! Check if this is the last time step
      if (t == nt) then
         ! Write all outputs at final step (including restart files)
         status = write_all_data(current_time, .true.)
      else
         ! Write outputs during normal steps
         status = write_all_data(current_time)
      end if
      
      if (status /= 0) then
         print *, "Warning: Write operation failed at time", current_time
      end if
   end do

   ! Finalize I/O system - close files and clean up
   status = finalize_io()
   if (status /= 0) then
      print *, "Warning: I/O finalization failed with status", status
   end if

   ! Deallocate memory for model variables
   deallocate(zeta, temp, u, v, temp_profile)

   print *, "Test with multi-dimensional variables completed successfully!"

end program main_test_output