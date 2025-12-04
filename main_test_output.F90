!===============================================================================
!> @file main_test_output.F90
!>
!> @brief Test program for the file-centric I/O system
!>
!> This program demonstrates the new file-centric output system where:
!> - Output files are defined in configuration (not per-variable)
!> - Same variable can go to multiple files with different frequencies
!> - Operations (instant, average, min, max) are per-file
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
program main_test_output
   use var_registry
   use var_definitions, only: send_all_outputs
   use io_manager
   use ocean_var
   use grid_module

   implicit none

   !> Domain size and time parameters
   integer, parameter :: nx = 10, ny = 8, nz = 5
   integer, parameter :: nt = 30                    ! 30 hours of simulation
   real, parameter :: dt_sim = 3600.0               ! 1 hour timestep
   
   integer :: t, status, i
   real :: current_time

   print *, "========================================"
   print *, "MIAOU File-Centric I/O Test"
   print *, "========================================"

   ! Allocate model fields
   allocate(zeta(nx, ny))
   allocate(temp(nx, ny, nz))
   allocate(u(nx, ny))
   allocate(v(nx, ny))
   allocate(temp_profile(nz))

   ! Initialize fields
   zeta = 0.0
   temp = 15.0          ! Initial temperature 15°C
   u = 0.0
   v = 0.0
   temp_profile = 15.0
   wind_speed = 5.0     ! Initial wind 5 m/s

   ! Initialize I/O system (reads output_config.nml)
   print *, ""
   print *, "Initializing I/O system..."
   status = initialize_io("output_config.nml")
   if (status /= 0) then
      print *, "Warning: Config file issue, using defaults"
   end if

   ! Initialize and register variables
   print *, ""
   print *, "Registering variables..."
   call init_variables(nx, ny, nz)

   ! Time loop
   print *, ""
   print *, "Starting time loop..."
   print *, "  Simulation: ", nt, " steps of ", dt_sim, " seconds"
   print *, ""

   ! Create files before the loop (needed for averaging)
   call ensure_files_created()

   do t = 1, nt
      current_time = t * dt_sim

      ! Update model fields (simple test patterns)
      zeta = sin(current_time / 3600.0) * 0.5      ! Tidal-like oscillation
      u = 0.1 * sin(current_time / 7200.0)         ! Slow current variation
      v = 0.05 * cos(current_time / 7200.0)
      temp = 15.0 + 2.0 * sin(current_time / 43200.0)  ! Daily temperature cycle
      temp_profile = 15.0 - 0.5 * [(i, i=1,nz)]   ! Decreasing with depth
      wind_speed = 5.0 + 3.0 * sin(current_time / 21600.0)  ! 6-hour wind cycle

      ! Send all outputs (copy data to internal buffers)
      call send_all_outputs(current_time)

      ! Write outputs (io_manager handles timing for each file)
      if (t == nt) then
         status = write_output(current_time, is_final=.true.)
      else
         status = write_output(current_time)
      end if

      ! Progress indicator
      if (mod(t, 6) == 0) then
         print *, "  Time step ", t, "/", nt, " (", current_time/3600.0, " hours)"
      end if
   end do

   ! Finalize
   print *, ""
   print *, "Finalizing I/O system..."
   status = finalize_io()

   ! Cleanup
   deallocate(zeta, temp, u, v, temp_profile)

   print *, ""
   print *, "========================================"
   print *, "Test completed successfully!"
   print *, ""
   print *, "Output files created:"
   print *, "  - ocean_hourly_3600s.nc     (instantaneous, hourly)"
   print *, "  - ocean_6hourly_21600s.nc   (instantaneous, 6-hourly)"
   print *, "  - ocean_daily_avg_86400s.nc (daily averages)"
   print *, "========================================"


end program main_test_output
