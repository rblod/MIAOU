!===============================================================================
! main_test_output_enhanced.F90 : Test program for multi-dimensional output
!===============================================================================
program main_test_output_enhanced
   use file_manager
   use variables_registry
   use ocean_var
   use grid_module
   
   implicit none
   
   integer, parameter :: nx = 10, ny = 8, nz = 5, nt = 10
   integer :: t
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
   
   ! Initialize model variables
   call init_variables(nx, ny, nz)
   
   ! Calendar settings
   time_units = "seconds since 2023-01-01 00:00:00"
   calendar = "gregorian"
   
   ! Output initialization
   call initialize_output_files(time_units, calendar)
   
   ! Time loop
   do t = 1, nt
      ! Time in seconds
      current_time = (t)*dt
      
      ! Fill values for test - all dimensions
      zeta = zeta + 0.1*t                     ! 2D field
      temp = temp + 0.2*t                     ! Full 3D field
      temp_profile = temp_profile + 0.5*t     ! 1D profile
      wind_speed = 5.0 + 0.1*t                ! 0D scalar
      u = t                                   ! 2D field
      v = t*0.5                               ! 2D field
      
      ! Check if this is the last time step
      if (t == nt) then
         ! Write all outputs at final step
         call write_all_outputs(current_time, .true.)
      else
         ! Write outputs during normal steps
         call write_all_outputs(current_time, .false.)
      end if
   end do
   print *, 'nt final ', nt, current_time
   
   ! Finalize outputs
   call close_all_output_files()
   
   ! Deallocate memory
   deallocate(zeta, temp, u, v, temp_profile)
   
   print *, "Test with multi-dimensional variables completed successfully!"
   
end program main_test_output_enhanced