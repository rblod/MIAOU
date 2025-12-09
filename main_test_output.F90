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
!> Compile with -DMPI for parallel execution:
!>   mpirun -np 4 ./test_output.exe
!>
!> In MPI mode, each process writes its own file:
!>   ocean_hourly_3600s.0000.nc, ocean_hourly_3600s.0001.nc, ...
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
program main_test_output
   use var_registry, only: init_variables
   use var_definitions, only: send_all_outputs
   use io_manager
   use ocean_var
   use grid_module
#ifdef MPI
   use mpi_param, only: mynode, is_master, Lm, Mm, N, ii, jj, NP_XI
   use mpi_setup, only: mpi_init_decomposition, mpi_finalize_all, mpi_barrier_all
#endif

   implicit none

   !> Domain size and time parameters
#ifndef MPI
   integer, parameter :: nx = 40, ny = 40, nz = 5
#endif
   integer, parameter :: nt = 30                    ! 30 hours of simulation
   real, parameter :: dt_sim = 3600.0               ! 1 hour timestep
   
   integer :: t, status, i, k
   real :: current_time
#ifdef MPI
   integer :: ierr
   real :: local_phase   ! Phase shift based on MPI position
#endif

#ifdef MPI
   !---------------------------------------------------------------------------
   ! MPI Initialization
   !---------------------------------------------------------------------------
   call mpi_init_decomposition(ierr)
   if (ierr /= 0) then
      print *, "MPI initialization failed!"
      stop 1
   end if
#endif

   !---------------------------------------------------------------------------
   ! Print header
   !---------------------------------------------------------------------------
#ifdef MPI
   if (is_master) then
#endif
      print *, "========================================"
      print *, "MIAOU File-Centric I/O Test"
#ifdef MPI
      print *, "  (MPI parallel mode)"
#else
      print *, "  (Serial mode)"
#endif
      print *, "========================================"
#ifdef MPI
   end if
   call mpi_barrier_all()
#endif

   !---------------------------------------------------------------------------
   ! Allocate model fields
   !---------------------------------------------------------------------------
#ifdef MPI
   ! In MPI mode, dimensions come from mpi_param (set by mpi_init_decomposition)
   call allocate_ocean_vars()
#else
   ! In serial mode, use local parameters
   call allocate_ocean_vars(nx, ny, nz)
#endif

   ! Initialize fields
   zeta = 0.0
   temp = 15.0          ! Initial temperature 15°C
   u = 0.0
   v = 0.0
   wind_speed = 5.0     ! Initial wind 5 m/s

   !---------------------------------------------------------------------------
   ! Initialize I/O system (reads output_config.nml)
   !---------------------------------------------------------------------------
#ifdef MPI
   if (is_master) then
#endif
      print *, ""
      print *, "Initializing I/O system..."
#ifdef MPI
   end if
#endif

   status = initialize_io("output_config.nml")
   if (status /= 0) then
#ifdef MPI
      if (is_master) then
#endif
         print *, "Warning: Config file issue, using defaults"
#ifdef MPI
      end if
#endif
   end if

   !---------------------------------------------------------------------------
   ! Initialize and register variables
   !---------------------------------------------------------------------------
#ifdef MPI
   if (is_master) then
#endif
      print *, ""
      print *, "Registering variables..."
#ifdef MPI
   end if
#endif

#ifdef MPI
   call init_variables(Lm, Mm, N)
#else
   call init_variables(nx, ny, nz)
#endif

   !---------------------------------------------------------------------------
   ! Time loop
   !---------------------------------------------------------------------------
#ifdef MPI
   if (is_master) then
#endif
      print *, ""
      print *, "Starting time loop..."
      print *, "  Simulation: ", nt, " steps of ", dt_sim, " seconds"
      print *, ""
#ifdef MPI
   end if
#endif

   ! Create files before the loop (needed for averaging)
   call ensure_files_created()

   do t = 1, nt
      current_time = t * dt_sim

      ! Update model fields (simple test patterns)
#ifdef MPI
   call mpi_barrier_all()
#ifdef NC4PAR
   ! In NC4PAR mode, no phase shift - same values everywhere (like serial)
   local_phase = 0.0
#else
   ! In MPI separate files mode, phase shift creates spatial variation
   local_phase = real(ii + jj * NP_XI) * 0.5
#endif
#endif

   do t = 1, nt
      current_time = t * dt_sim

      !------------------------------------------------------------------------
      ! Update model fields (simple test patterns)
      !------------------------------------------------------------------------
#ifdef MPI
      zeta = sin(current_time / 3600.0 + local_phase) * 0.5
      u = 0.1 * sin(current_time / 7200.0 + local_phase)
      v = 0.05 * cos(current_time / 7200.0 + local_phase)
      temp = 15.0 + 2.0 * sin(current_time / 43200.0 + local_phase)
      do k = 1, N
         temp_profile(k) = 15.0 - 0.5 * k
      end do
#else
      zeta = sin(current_time / 3600.0) * 0.5      ! Tidal-like oscillation
      u = 0.1 * sin(current_time / 7200.0)         ! Slow current variation
      v = 0.05 * cos(current_time / 7200.0)
      temp = 15.0 + 2.0 * sin(current_time / 43200.0)  ! Daily temperature cycle
      do k = 1, size(temp_profile)
         temp_profile(k) = 15.0 - 0.5 * k          ! Decreasing with depth
      end do
#endif
      wind_speed = 5.0 + 3.0 * sin(current_time / 21600.0)  ! 6-hour wind cycle

      ! Send all outputs (copy data to internal buffers)
      call send_all_outputs(current_time)

      ! Write outputs (io_manager handles timing for each file)
      if (t == nt) then
         status = write_output(current_time, is_final=.true.)
      else
         status = write_output(current_time)
      end if

#ifdef MPI
      call mpi_barrier_all()
#endif

      ! Progress indicator
#ifdef MPI
      if (is_master .and. mod(t, 6) == 0) then
#else
      if (mod(t, 6) == 0) then
#endif
         print *, "  Time step ", t, "/", nt, " (", current_time/3600.0, " hours)"
      end if
   end do

   !---------------------------------------------------------------------------
   ! Finalize
   !---------------------------------------------------------------------------
#ifdef MPI
   if (is_master) then
#endif
      print *, ""
      print *, "Finalizing I/O system..."
#ifdef MPI
   end if
#endif

   status = finalize_io()

   ! Cleanup
   call deallocate_ocean_vars()

   !---------------------------------------------------------------------------
   ! Summary
   !---------------------------------------------------------------------------
#ifdef MPI
   call mpi_barrier_all()
   if (is_master) then
#endif
      print *, ""
      print *, "========================================"
      print *, "Test completed successfully!"
      print *, ""
#ifdef MPI
      print *, "Output files created (one per process):"
      print *, "  - ocean_hourly_3600s_NNNN.nc     (instantaneous, hourly)"
      print *, "  - ocean_6hourly_21600s_NNNN.nc   (instantaneous, 6-hourly)"
      print *, "  - ocean_daily_avg_86400s_NNNN.nc (daily averages)"
      print *, "  - ocean_restart_3600s_NNNN.nc    (restart)"
      print *, ""
      print *, "Use ncjoin to assemble: ncjoin ocean_hourly_3600s_????.nc"
#else
      print *, "Output files created:"
      print *, "  - ocean_hourly_3600s.nc     (instantaneous, hourly)"
      print *, "  - ocean_6hourly_21600s.nc   (instantaneous, 6-hourly)"
      print *, "  - ocean_daily_avg_86400s.nc (daily averages)"
      print *, "  - ocean_restart_3600s.nc    (restart)"
#endif
      print *, "========================================"
#ifdef MPI
   end if

   !---------------------------------------------------------------------------
   ! MPI Finalization
   !---------------------------------------------------------------------------
   call mpi_finalize_all()
#endif

end program main_test_output
