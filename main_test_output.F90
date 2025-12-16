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
   use io_netcdf, only: nc_open_file_readonly, nc_close_file, nc_get_num_times, &
                        nc_read_2d, nc_has_variable, nc_get_var_shape
#ifdef MPI
   use mpi_param, only: is_master, Lm, Mm, N
   use mpi_setup, only: mpi_init_decomposition, mpi_finalize_all, mpi_barrier_all
#endif
#ifdef PARALLEL_FILES
   use mpi_param, only: NP_XI, ii, jj
#endif
#if defined(MPI) && !defined(PARALLEL_FILES) && !defined(NC4PAR)
   use io_mpi_sync, only: io_wait_turn, io_pass_turn
#endif

   implicit none

   !> Domain size and time parameters
#ifndef MPI
   integer, parameter :: nx = 40, ny = 40, nz = 5
#endif
   integer, parameter :: nt = 30                    ! 30 hours of simulation
   real, parameter :: dt_sim = 3600.0               ! 1 hour timestep
   
   integer :: t, status, k
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
   ! Validate configuration (v5.4.0)
   !---------------------------------------------------------------------------
   status = validate_config()
   if (status > 0) then
#ifdef MPI
      if (is_master) then
#endif
         print *, "WARNING: Configuration has ", status, " error(s)"
         print *, "         Check variable names in output_config.nml"
#ifdef MPI
      end if
#endif
   end if

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

#ifdef MPI
   call mpi_barrier_all()
#ifdef PARALLEL_FILES
   ! In PARALLEL_FILES mode, phase shift creates spatial variation per file
   local_phase = real(ii + jj * NP_XI) * 0.5
#else
   ! In shared file modes (sequential or NC4PAR), no phase shift
   ! All processes write same values - identical to serial output
   local_phase = 0.0
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

#if defined(MPI) && !defined(PARALLEL_FILES) && !defined(NC4PAR)
      ! Check restart rotation BEFORE token passing (master recreates file if needed)
      call check_restart_rotation(current_time)
      
      ! Sequential I/O: all writing must be inside token-protected section
      call io_wait_turn()
      call open_all_files_seq()
#endif

      ! Send all outputs (write data to files)
      call send_all_outputs(current_time)

      ! Write outputs (io_manager handles timing for each file)
      if (t == nt) then
         status = write_output(current_time, is_final=.true.)
      else
         status = write_output(current_time)
      end if

#if defined(MPI) && !defined(PARALLEL_FILES) && !defined(NC4PAR)
      ! Sequential I/O: close files and pass turn
      call close_all_files_seq()
      call io_pass_turn()
      ! Synchronize restart time indices after all processes have written
      call sync_restart_time_indices()
#endif

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

   !---------------------------------------------------------------------------
   ! Roundtrip read test (v6.1.0)
   !---------------------------------------------------------------------------
#ifndef PARALLEL_FILES
#ifdef MPI
   if (is_master) then
#endif
      call test_roundtrip_read()
#ifdef MPI
   end if
#endif
#endif

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
#ifdef PARALLEL_FILES
      print *, "Output files created (one per process):"
      print *, "  - ocean_hourly_3600s_NNNN.nc     (instantaneous, hourly)"
      print *, "  - ocean_6hourly_21600s_NNNN.nc   (instantaneous, 6-hourly)"
      print *, "  - ocean_daily_avg_86400s_NNNN.nc (daily averages)"
      print *, "  - ocean_restart_3600s_NNNN.nc    (restart)"
      print *, ""
      print *, "Use ncjoin to assemble: ncjoin ocean_hourly_3600s_????.nc"
#elif defined(NC4PAR)
      print *, "Output files created (parallel NetCDF-4):"
      print *, "  - ocean_hourly_3600s.nc     (instantaneous, hourly)"
      print *, "  - ocean_6hourly_21600s.nc   (instantaneous, 6-hourly)"
      print *, "  - ocean_daily_avg_86400s.nc (daily averages)"
      print *, "  - ocean_restart_3600s.nc    (restart)"
#else
      print *, "Output files created (sequential I/O):"
      print *, "  - ocean_hourly_3600s.nc     (instantaneous, hourly)"
      print *, "  - ocean_6hourly_21600s.nc   (instantaneous, 6-hourly)"
      print *, "  - ocean_daily_avg_86400s.nc (daily averages)"
      print *, "  - ocean_restart_3600s.nc    (restart)"
#endif
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

contains

   !---------------------------------------------------------------------------
   !> @brief Test roundtrip: read restart file and verify values (v6.1.0)
   !---------------------------------------------------------------------------
   subroutine test_roundtrip_read()
      integer :: ncid, rd_status, ntimes, ndims
      integer :: var_shape(4)
      real, allocatable :: zeta_read(:,:)
      real :: max_val, min_val
      character(len=256) :: restart_file
#ifndef MPI
      real :: max_diff
#endif
      
      logical :: file_exists
      
      restart_file = "ocean_restart_3600s.nc"
      
      print *, ""
      print *, "--- Roundtrip read test (v6.1.0) ---"
      
      ! Check if file exists first (avoid error message)
      inquire(file=restart_file, exist=file_exists)
      if (.not. file_exists) then
         print *, "  (Skipping: no restart file)"
         return
      end if
      
      ! Open restart file
      rd_status = nc_open_file_readonly(restart_file, ncid)
      if (rd_status /= 0) then
         print *, "  ✗ Cannot open restart file"
         return
      end if
      
      ! Get number of time records
      ntimes = nc_get_num_times(ncid)
      print *, "  Time records in file:", ntimes
      
      ! Check zeta exists
      if (.not. nc_has_variable(ncid, "zeta")) then
         print *, "  ✗ Variable 'zeta' not found"
         rd_status = nc_close_file(ncid)
         return
      end if
      
      ! Get shape and allocate with FILE dimensions (not memory dimensions)
      rd_status = nc_get_var_shape(ncid, "zeta", ndims, var_shape)
      print *, "  File zeta shape:", var_shape(1), "x", var_shape(2)
      allocate(zeta_read(var_shape(1), var_shape(2)))
      
      ! Read last time level
      rd_status = nc_read_2d(ncid, "zeta", ntimes, zeta_read)
      if (rd_status /= 0) then
         print *, "  ✗ Failed to read zeta"
         deallocate(zeta_read)
         rd_status = nc_close_file(ncid)
         return
      end if
      
      ! Verification
      min_val = minval(zeta_read)
      max_val = maxval(zeta_read)
      print *, "  Read zeta range:", min_val, "to", max_val

#ifdef MPI
      ! In MPI mode: zeta in memory is LOCAL, file has GLOBAL domain
      ! Just verify values are in reasonable range (zeta ~= 0.5*sin*cos ~ [-0.5, 0.5])
      if (abs(min_val) < 1.0 .and. abs(max_val) < 1.0) then
         print *, "  ✓ Roundtrip test PASSED (values in valid range)"
      else
         print *, "  ✗ Roundtrip test FAILED (values out of range)"
      end if
#else
      ! In serial mode: direct comparison possible
      max_diff = maxval(abs(zeta_read - zeta))
      print *, "  Max difference (read vs memory):", max_diff
      if (max_diff < 1.0e-6) then
         print *, "  ✓ Roundtrip test PASSED"
      else
         print *, "  ✗ Roundtrip test FAILED"
      end if
#endif
      
      deallocate(zeta_read)
      rd_status = nc_close_file(ncid)
      
   end subroutine test_roundtrip_read

end program main_test_output
