!===============================================================================
!> @file io_manager.F90
!>
!> @brief High-level I/O management module
!>
!> This module provides a unified interface for I/O operations. It coordinates
!> variable registration, file management, and data output using a file-centric
!> approach where output files define which variables they contain.
!>
!> ## Main workflow
!>
!> 1. `initialize_io()` — Read config, setup backend
!> 2. `register_variable()` — Register model variables
!> 3. `write_output(time)` — Called each timestep, handles all files
!> 4. `finalize_io()` — Close files, cleanup
!>
!> @author Rachid Benshila
!> @date 2025-04
!===============================================================================
module io_manager
   use netcdf, only: nf90_sync
   use io_constants, only: IO_TIME_TOLERANCE, IO_PATH_LEN, IO_PREFIX_LEN, &
                           io_flush_freq, io_verbose, IO_QUIET, IO_NORMAL, IO_DEBUG
   use io_definitions, only: io_variable, io_var_registry
   use io_config, only: read_io_config, get_output_prefix, get_time_units, &
                        get_calendar, &
                        io_validate_vars, io_warn_empty_files, &  ! v5.4.0
                        io_nc4par_required  ! v5.5.0
   use io_state, only: file_registry, var_registry, &
                       is_initialized => io_initialized, &
                       files_created   ! v5.4.0: State in io_state
   use io_file_registry, only: output_file_def, output_file_registry, avg_state, &
                               OP_INSTANT, OP_AVERAGE, OP_MIN, OP_MAX, OP_ACCUMULATE
   use io_naming, only: generate_filename, add_mpi_suffix
   use io_error
   use io_netcdf, only: nc_initialize, nc_finalize, nc_create_file, nc_open_file, &
                        nc_write_variable_data, nc_write_time, &
                        nc_close_file, nc_define_variable_in_file, &
                        nc_end_definition, nc_write_avg_data, &
                        nc_write_direct_0d, nc_write_direct_1d, &
                        nc_write_direct_2d, nc_write_direct_3d, &
                        nc_has_parallel_support, nc_check_parallel_runtime
#ifdef MPI
   use mpi
   use mpi_param, only: mynode, is_master, NNODES
#endif
#if defined(MPI) && !defined(PARALLEL_FILES) && !defined(NC4PAR)
   use io_mpi_sync, only: io_wait_turn, io_pass_turn
#endif
#ifdef NC4PAR
   use io_netcdf, only: nc_set_parallel_access, nc_set_all_parallel_access
#endif

   implicit none
   private

   ! Public interface
   public :: initialize_io, finalize_io
   public :: register_variable
   public :: write_output
   public :: get_variable_ptr
   public :: process_var_instant, process_var_average
   public :: ensure_files_created
   public :: validate_config   ! v5.4.0: Validate configuration
   public :: var_registry      ! Re-exported from io_state for compatibility
   public :: get_io_mode_string  ! v5.5.0: Get I/O mode description
#if defined(MPI) && !defined(PARALLEL_FILES) && !defined(NC4PAR)
   public :: open_all_files_seq, close_all_files_seq
   public :: sync_restart_time_indices, check_restart_rotation
#endif

   ! v5.4.0: var_registry, is_initialized, files_created now in io_state module

contains

   !---------------------------------------------------------------------------
   !> @brief Get a human-readable string describing the I/O mode
   !>
   !> Returns one of:
   !> - "Serial" — Single process, direct writes
   !> - "MPI Sequential" — Multiple processes, one writes at a time
   !> - "MPI Parallel Files" — Each process writes its own file
   !> - "NC4PAR" — Parallel NetCDF-4, all processes write to one file
   !>
   !> @return String describing the I/O mode
   !---------------------------------------------------------------------------
   function get_io_mode_string() result(mode)
      character(len=64) :: mode
      
#ifdef NC4PAR
      mode = "NC4PAR (Parallel NetCDF-4, single shared file)"
#elif defined(PARALLEL_FILES)
      mode = "MPI Parallel Files (one file per process)"
#elif defined(MPI)
      mode = "MPI Sequential (processes write in turn)"
#else
      mode = "Serial (single process)"
#endif
   end function get_io_mode_string

   !---------------------------------------------------------------------------
   !> @brief Initialize the I/O system
   !>
   !> @param[in] config_file  Optional path to configuration file
   !> @return    Status (0 = success)
   !---------------------------------------------------------------------------
   function initialize_io(config_file) result(status)
      character(len=*), intent(in), optional :: config_file
      integer :: status
      logical :: nc4par_ok
      character(len=256) :: nc4par_msg
      character(len=64) :: io_mode
      integer :: ierr

      status = 0

      ! Initialize NetCDF backend
      call nc_initialize()

      ! Read configuration FIRST (needed for io_nc4par_required)
      if (present(config_file)) then
         status = read_io_config(config_file)
      else
         status = read_io_config("output_config.nml")
      end if

      ! Determine and display I/O mode
      io_mode = get_io_mode_string()
      
#ifdef MPI
      if (is_master) then
         print *, ""
         print *, "I/O Mode: ", trim(io_mode)
      end if
      
      ! Check parallel NetCDF support if compiled with NC4PAR
      if (nc_has_parallel_support()) then
         call nc_check_parallel_runtime(nc4par_ok, nc4par_msg)
         if (is_master) then
            if (nc4par_ok) then
               print *, "NC4PAR runtime check: OK"
               print *, "  ", trim(nc4par_msg)
            else
               print *, ""
               print *, "=============================================="
               print *, "ERROR: NC4PAR RUNTIME CHECK FAILED"
               print *, "=============================================="
               print *, trim(nc4par_msg)
               print *, "=============================================="
               print *, ""
               if (io_nc4par_required) then
                  print *, "FATAL: NC4PAR is required (nml_nc4par_required=.true.)"
                  print *, "       Set nml_nc4par_required=.false. to continue with degraded I/O"
                  print *, "       or recompile without -DNC4PAR for sequential MPI mode."
               else
                  print *, "WARNING: Continuing with NC4PAR disabled (nml_nc4par_required=.false.)"
                  print *, "         File creation may fail or produce incomplete output."
               end if
            end if
         end if
         ! If NC4PAR required and it failed, stop all processes
         if (.not. nc4par_ok .and. io_nc4par_required) then
            call MPI_Barrier(MPI_COMM_WORLD, ierr)
            call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
         end if
      end if
#else
      print *, ""
      print *, "I/O Mode: ", trim(io_mode)
#endif

      is_initialized = .true.
      files_created = .false.

      print *, "I/O system initialized"
   end function initialize_io

   !---------------------------------------------------------------------------
   !> @brief Finalize the I/O system
   !>
   !> @return Status (0 = success)
   !---------------------------------------------------------------------------
   function finalize_io() result(status)
      integer :: status
      integer :: i
      type(output_file_def), pointer :: file_ptr

      status = 0

      ! Close all open files
      do i = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(i)
         if (associated(file_ptr)) then
            if (file_ptr%is_open()) then
               status = nc_close_file(file_ptr%ncid)
               file_ptr%ncid = -1
            end if
         end if
      end do

      call nc_finalize()
      is_initialized = .false.

      print *, "I/O system finalized"
   end function finalize_io

   !---------------------------------------------------------------------------
   !> @brief Register a variable for output
   !>
   !> @param[in] var  Variable to register
   !---------------------------------------------------------------------------
   subroutine register_variable(var)
      type(io_variable), intent(in) :: var

      call var_registry%add(var)
      print *, "Registered variable: ", trim(var%meta%name)
   end subroutine register_variable

   !---------------------------------------------------------------------------
   !> @brief Validate configuration after variables are registered (v5.4.0)
   !>
   !> Checks that:
   !> 1. All variables referenced in output files exist in var_registry
   !> 2. No output file is empty (has no variables)
   !> 3. Frequency values are valid (>0 for non-restart, any for restart)
   !>
   !> @return Number of validation errors found
   !---------------------------------------------------------------------------
   function validate_config() result(num_errors)
      integer :: num_errors
      
      integer :: i, j, var_idx
      type(output_file_def), pointer :: file_ptr
      character(len=256) :: msg
      integer :: num_warnings
      
      num_errors = 0
      num_warnings = 0
      
      if (io_verbose >= IO_NORMAL) then
         print *, ""
         print *, "=== Validating I/O configuration ==="
      end if
      
      ! Check each file definition
      do i = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(i)
         if (.not. associated(file_ptr)) cycle
         
         ! Check for empty files
         if (io_warn_empty_files) then
            if (file_ptr%num_variables == 0) then
               write(msg, '(A,A,A)') "Output file '", trim(file_ptr%name), &
                  "' has no variables defined"
               call io_report_warning(trim(msg), "validate_config")
               num_warnings = num_warnings + 1
            end if
         end if
         
         ! Check each variable reference
         if (io_validate_vars) then
            do j = 1, file_ptr%num_variables
               var_idx = var_registry%find(file_ptr%variables(j))
               
               if (var_idx < 0) then
                  write(msg, '(A,A,A,A,A)') "Variable '", &
                     trim(file_ptr%variables(j)), &
                     "' in file '", trim(file_ptr%name), &
                     "' is not registered"
                  call io_report_error(IO_ERR_VAR_NOTFOUND, trim(msg), "validate_config")
                  num_errors = num_errors + 1
               end if
            end do
         end if
         
         ! Check frequency validity (non-restart files need freq > 0)
         if (.not. file_ptr%is_restart .and. file_ptr%frequency <= 0.0) then
            write(msg, '(A,A,A,G12.4)') "File '", trim(file_ptr%name), &
               "' has invalid frequency: ", file_ptr%frequency
            call io_report_error(IO_ERR_CONFIG, trim(msg), "validate_config")
            num_errors = num_errors + 1
         end if
      end do
      
      ! Summary
      if (io_verbose >= IO_NORMAL) then
         if (num_errors == 0 .and. num_warnings == 0) then
            print *, "Configuration valid: ", file_registry%size(), " files, ", &
               var_registry%size(), " variables registered"
         else
            print *, "Validation complete: ", num_errors, " errors, ", &
               num_warnings, " warnings"
         end if
         print *, "======================================"
         print *, ""
      end if
      
   end function validate_config

   !---------------------------------------------------------------------------
   !> @brief Get pointer to a registered variable by name
   !>
   !> @param[in] var_name  Variable name
   !> @return    Pointer to variable, or null if not found
   !---------------------------------------------------------------------------
   function get_variable_ptr(var_name) result(var_ptr)
      character(len=*), intent(in) :: var_name
      type(io_variable), pointer :: var_ptr
      integer :: idx

      nullify(var_ptr)
      idx = var_registry%find(var_name)
      if (idx > 0) then
         var_ptr => var_registry%get_ptr(idx)
      end if
   end function get_variable_ptr

   !---------------------------------------------------------------------------
   !> @brief Write output for all files at current time
   !>
   !> This is the main entry point called each timestep. It:
   !> 1. Creates files if not yet done
   !> 2. Accumulates data for averaging files
   !> 3. Writes data to files when their output time is reached
   !>
   !> @param[in] current_time  Current simulation time (seconds)
   !> @param[in] is_final      Optional: true if this is the last timestep
   !> @return    Status (0 = success)
   !---------------------------------------------------------------------------
   function write_output(current_time, is_final) result(status)
      real, intent(in) :: current_time
      logical, intent(in), optional :: is_final
      integer :: status

      integer :: file_idx
      type(output_file_def), pointer :: file_ptr
      logical :: final_step

      status = 0
      final_step = .false.
      if (present(is_final)) final_step = is_final

      ! Create files on first call
      if (.not. files_created) then
         call create_all_files()
         files_created = .true.
      end if

      ! Note: In sequential I/O mode, token passing and file open/close
      ! must be done by the CALLER around both send_all_outputs() and write_output()

      ! Process each file
      do file_idx = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle

         call process_file(file_ptr, current_time, final_step)
      end do

   end function write_output

#if defined(MPI) && !defined(PARALLEL_FILES) && !defined(NC4PAR)
   !---------------------------------------------------------------------------
   !> @brief Open all files for sequential I/O
   !>
   !> Called by non-master processes to open files created by master.
   !> Also called by master after passing token back around.
   !---------------------------------------------------------------------------
   subroutine open_all_files_seq()
      integer :: file_idx, status
      type(output_file_def), pointer :: file_ptr

      do file_idx = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle
         
         ! Open file if not already open
         if (file_ptr%ncid < 0) then
            status = nc_open_file(file_ptr%filename, file_ptr%ncid, &
                                  file_ptr%time_dimid, file_ptr%time_varid)
            if (status /= 0) then
               call io_report_error(IO_ERR_FILE_OPEN, &
                  "Cannot open file for sequential I/O: " // trim(file_ptr%filename), &
                  "open_all_files_seq")
            end if
         end if
      end do
   end subroutine open_all_files_seq

   !---------------------------------------------------------------------------
   !> @brief Close all files for sequential I/O
   !>
   !> Must close files after writing so next process can open them.
   !---------------------------------------------------------------------------
   subroutine close_all_files_seq()
      integer :: file_idx, status
      type(output_file_def), pointer :: file_ptr

      do file_idx = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle
         
         ! Close file if open
         if (file_ptr%ncid > 0) then
            status = nc_close_file(file_ptr%ncid)
            file_ptr%ncid = -1  ! Mark as closed
         end if
      end do
   end subroutine close_all_files_seq

   !---------------------------------------------------------------------------
   !> @brief Check and handle restart rotation before token passing
   !>
   !> In MPI sequential mode, rotation must happen BEFORE token passing starts.
   !> Master checks if rotation needed, performs it, then broadcasts to all.
   !> All processes update their time_index before writing.
   !---------------------------------------------------------------------------
   subroutine check_restart_rotation(current_time)
      use mpi
      real, intent(in) :: current_time
      
      integer :: file_idx, ierr, status
      integer :: needs_rotation, new_time_index
      type(output_file_def), pointer :: file_ptr
      logical :: is_checkpoint_time

      do file_idx = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle
         
         ! Only process restart files
         if (file_ptr%operation /= OP_INSTANT) cycle
         if (file_ptr%restart_nlevels <= 0) cycle
         
         ! Check if this is a checkpoint time
         if (file_ptr%frequency < 0.0) then
            is_checkpoint_time = .false.  ! Final only - handled separately
         else
            is_checkpoint_time = is_output_time(current_time, file_ptr%frequency)
         end if
         
         if (.not. is_checkpoint_time) cycle
         
         ! Master checks if rotation needed and performs it
         needs_rotation = 0
         if (is_master) then
            if (file_ptr%time_index > file_ptr%restart_nlevels) then
               needs_rotation = 1
               ! Close and recreate file
               if (file_ptr%ncid > 0) then
                  status = nc_close_file(file_ptr%ncid)
                  file_ptr%ncid = -1
               end if
               call recreate_restart_file(file_ptr)
               ! time_index is now 1 after recreate
            end if
            new_time_index = file_ptr%time_index
         end if
         
         ! Broadcast rotation status and new time_index
         call MPI_Bcast(needs_rotation, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
         call MPI_Bcast(new_time_index, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
         
         ! All non-master processes update their time_index
         if (.not. is_master) then
            file_ptr%time_index = new_time_index
         end if
         
         ! Barrier to ensure file is ready before others open it
         call MPI_Barrier(MPI_COMM_WORLD, ierr)
      end do
   end subroutine check_restart_rotation

   !---------------------------------------------------------------------------
   !> @brief Synchronize restart file time indices after writing
   !>
   !> After all processes have written, sync time_index for next iteration.
   !---------------------------------------------------------------------------
   subroutine sync_restart_time_indices()
      use mpi
      integer :: file_idx, ierr
      type(output_file_def), pointer :: file_ptr

      do file_idx = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle
         
         ! Only sync restart files
         if (file_ptr%operation /= OP_INSTANT) cycle
         if (file_ptr%restart_nlevels <= 0) cycle
         
         ! Master broadcasts its time_index to all others
         call MPI_Bcast(file_ptr%time_index, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      end do
   end subroutine sync_restart_time_indices
#endif

   !---------------------------------------------------------------------------
   !> @brief Ensure all output files are created
   !>
   !> Call this before send_all_outputs to ensure files exist.
   !> In sequential I/O mode, waits for master to finish creating files.
   !---------------------------------------------------------------------------
   subroutine ensure_files_created()
#if defined(MPI) && !defined(PARALLEL_FILES) && !defined(NC4PAR)
      use mpi
      integer :: ierr
#endif

      if (.not. files_created) then
         call create_all_files()
         files_created = .true.
      end if

#if defined(MPI) && !defined(PARALLEL_FILES) && !defined(NC4PAR)
      ! Wait for master to finish creating files before continuing
      call MPI_Barrier(MPI_COMM_WORLD, ierr)
#endif
   end subroutine ensure_files_created

   !---------------------------------------------------------------------------
   !> @brief Create all output files
   !>
   !> MPI I/O modes:
   !> - Sequential (default): master creates single file, others open later
   !> - PARALLEL_FILES: each process creates its own file with _NNNN suffix
   !> - NC4PAR: parallel NetCDF-4, single shared file
   !---------------------------------------------------------------------------
   subroutine create_all_files()
      integer :: file_idx, status
      type(output_file_def), pointer :: file_ptr
      character(len=IO_PATH_LEN) :: filename
      logical :: do_create

      ! Note: validation is now done by calling validate_config() explicitly
      ! after variables are registered (v5.4.0)

      do file_idx = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle

         ! Generate filename (all processes do this)
         filename = generate_filename(file_ptr%prefix, file_ptr%name, file_ptr%frequency)
         
#if defined(MPI) && defined(PARALLEL_FILES)
         ! Separate files mode: add MPI rank suffix
         filename = add_mpi_suffix(filename, mynode)
#endif
         
         ! Store filename on all processes
         file_ptr%filename = filename

#if defined(MPI) && !defined(PARALLEL_FILES) && !defined(NC4PAR)
         ! Sequential I/O: only master creates files
         do_create = is_master
#else
         ! Other modes: all processes create/participate
         do_create = .true.
#endif

         if (do_create) then
            ! Create the file
            status = nc_create_file(filename, file_ptr%name, file_ptr%frequency, &
                                    get_time_units(), get_calendar(), &
                                    file_ptr%ncid, file_ptr%time_dimid, &
                                    file_ptr%time_varid)

            if (status /= 0) then
               call io_report_error(IO_ERR_FILE_CREATE, &
                  "Failed to create file: " // trim(filename), &
                  "ensure_files_created")
               cycle
            end if

            ! Define variables in file
            call define_variables_in_file(file_ptr)

            ! End definition mode
            status = nc_end_definition(file_ptr%ncid)

#ifdef NC4PAR
            ! Set all variables to collective parallel access mode
            call nc_set_all_parallel_access(file_ptr%ncid)
#endif

#if defined(MPI) && !defined(PARALLEL_FILES) && !defined(NC4PAR)
            ! Sequential I/O: master closes file after creation
            status = nc_close_file(file_ptr%ncid)
            file_ptr%ncid = -1  ! Mark as closed
#endif
         else
            ! Non-master in sequential mode: mark file as not yet opened
            file_ptr%ncid = -1
         end if

         if (io_verbose >= IO_NORMAL) then
#ifdef MPI
            if (is_master) then
#ifdef NC4PAR
               print *, "Created parallel file: ", trim(filename)
#elif defined(PARALLEL_FILES)
               print *, "Created files: ", trim(file_ptr%prefix)//"_"//trim(file_ptr%name)// &
                        "_NNNN.nc (one per process)"
#else
               print *, "Created file (sequential I/O): ", trim(filename)
#endif
            end if
#else
            print *, "Created file: ", trim(filename)
#endif
         end if
      end do
   end subroutine create_all_files

   !> @brief Define variables for a file
   !---------------------------------------------------------------------------
   subroutine define_variables_in_file(file_ptr)
      type(output_file_def), pointer, intent(inout) :: file_ptr

      integer :: i, var_idx, status
      type(io_variable), pointer :: var_ptr
      character(len=32) :: var_name

      do i = 1, file_ptr%num_variables
         var_name = file_ptr%variables(i)

         ! Find variable in registry
         var_idx = var_registry%find(var_name)
         if (var_idx < 0) then
            call io_report_warning( &
               "Variable " // trim(var_name) // " not registered, skipping", &
               "define_variables_in_file")
            cycle
         end if

         var_ptr => var_registry%get_ptr(var_idx)
         if (.not. associated(var_ptr)) cycle

         ! Define in file
         status = nc_define_variable_in_file(file_ptr%ncid, file_ptr%time_dimid, var_ptr)
         if (status /= 0) then
            call io_report_warning( &
               "Failed to define variable " // trim(var_name), &
               "define_variables_in_file")
         end if
      end do
      
      ! Initialize avg_states array (but not the buffers - done later with actual data)
      if (file_ptr%needs_averaging()) then
         call file_ptr%init_avg_states()
      end if
   end subroutine define_variables_in_file

   !---------------------------------------------------------------------------
   !> @brief Process a single file at current time
   !---------------------------------------------------------------------------
   subroutine process_file(file_ptr, current_time, is_final)
      type(output_file_def), pointer, intent(inout) :: file_ptr
      real, intent(in) :: current_time
      logical, intent(in) :: is_final

      logical :: is_write_time
      integer :: status

      if (.not. file_ptr%is_open()) return
      
      ! Skip t=0 for restart files
      if (file_ptr%is_restart .and. abs(current_time) < IO_TIME_TOLERANCE) return

      ! Handle restart files specially
      if (file_ptr%is_restart) then
         call process_restart_file(file_ptr, current_time, is_final)
         return
      end if

      ! Check if it's time to write
      is_write_time = is_output_time(current_time, file_ptr%frequency)

      ! For averaging files: write when time (accumulation done by send_var)
      if (file_ptr%needs_averaging()) then
         if (is_write_time) then
            call write_file_averaged(file_ptr, current_time)
            call reset_file_averaging(file_ptr)
         end if
      else
         ! Instantaneous: data written by send_var, just write time here
         if (file_ptr%data_written_this_step) then
            ! Write time value
            status = nc_write_time(file_ptr%ncid, file_ptr%time_varid, &
                                   file_ptr%time_index, current_time)
            call file_ptr%increment_time()
            call maybe_flush(file_ptr)
            
            if (io_verbose >= IO_DEBUG) then
               print *, "  Written instant: ", trim(file_ptr%name), " t=", current_time
            end if
            
            ! Reset flag for next step
            file_ptr%data_written_this_step = .false.
         end if
      end if

   end subroutine process_file

   !---------------------------------------------------------------------------
   !> @brief Process a restart file
   !>
   !> Restart files write incrementally like instant files, but with rotation:
   !> - Writes data at each checkpoint time
   !> - When N levels reached, rewrites file from beginning
   !> - freq < 0: write only at final time
   !> - freq > 0: write at freq intervals AND at final time
   !---------------------------------------------------------------------------
   subroutine process_restart_file(file_ptr, current_time, is_final)
      type(output_file_def), pointer, intent(inout) :: file_ptr
      real, intent(in) :: current_time
      logical, intent(in) :: is_final

      logical :: is_checkpoint_time
#if !defined(MPI) || defined(PARALLEL_FILES) || defined(NC4PAR)
      integer :: status
#endif
      
      ! Determine if we should write
      if (file_ptr%frequency < 0.0) then
         ! Final only mode
         is_checkpoint_time = is_final
      else
         ! Periodic checkpoints + final
         is_checkpoint_time = is_output_time(current_time, file_ptr%frequency) .or. is_final
      end if
      
      if (is_checkpoint_time) then
         ! Check if we need to rotate (reset to beginning)
         ! In MPI sequential mode, rotation is handled by check_restart_rotation()
         ! before token passing starts, so we skip it here
#if !defined(MPI) || defined(PARALLEL_FILES) || defined(NC4PAR)
         if (file_ptr%time_index > file_ptr%restart_nlevels) then
            ! Reopen file in overwrite mode
            if (file_ptr%ncid > 0) then
               status = nc_close_file(file_ptr%ncid)
            end if
            call recreate_restart_file(file_ptr)
         end if
#endif
         
         ! Write current data directly (like instant)
         call write_restart_record(file_ptr, current_time)
      end if

   end subroutine process_restart_file

   !---------------------------------------------------------------------------
   !> @brief Recreate restart file (for rotation)
   !---------------------------------------------------------------------------
   subroutine recreate_restart_file(file_ptr)
      type(output_file_def), pointer, intent(inout) :: file_ptr

      integer :: i, var_idx, status
      type(io_variable), pointer :: var_ptr

      status = nc_create_file(file_ptr%filename, file_ptr%name, file_ptr%frequency, &
                              get_time_units(), get_calendar(), &
                              file_ptr%ncid, file_ptr%time_dimid, &
                              file_ptr%time_varid)

      if (status /= 0) then
         call io_report_error(IO_ERR_FILE_CREATE, &
            "Failed to recreate restart file: " // trim(file_ptr%filename), &
            "recreate_restart_file")
         return
      end if

      ! Define variables
      do i = 1, file_ptr%num_variables
         var_idx = var_registry%find(file_ptr%variables(i))
         if (var_idx < 0) cycle

         var_ptr => var_registry%get_ptr(var_idx)
         if (.not. associated(var_ptr)) cycle

         status = nc_define_variable_in_file(file_ptr%ncid, file_ptr%time_dimid, var_ptr)
      end do

      status = nc_end_definition(file_ptr%ncid)
      
#ifdef NC4PAR
      ! Set all variables to collective parallel access mode
      call nc_set_all_parallel_access(file_ptr%ncid)
#endif

      file_ptr%time_index = 1

   end subroutine recreate_restart_file

   !---------------------------------------------------------------------------
   !> @brief Write one restart record (current data)
   !---------------------------------------------------------------------------
   subroutine write_restart_record(file_ptr, current_time)
      type(output_file_def), pointer, intent(inout) :: file_ptr
      real, intent(in) :: current_time

      integer :: i, var_idx, status
      type(io_variable), pointer :: var_ptr

      ! Write all variables at current time_index
      do i = 1, file_ptr%num_variables
         var_idx = var_registry%find(file_ptr%variables(i))
         if (var_idx < 0) cycle

         var_ptr => var_registry%get_ptr(var_idx)
         if (.not. associated(var_ptr)) cycle

         status = nc_write_variable_data(file_ptr%ncid, file_ptr%time_index, var_ptr)
      end do

      ! Write time
      status = nc_write_time(file_ptr%ncid, file_ptr%time_varid, &
                             file_ptr%time_index, current_time)

      if (io_verbose >= IO_NORMAL) then
         print *, "Written restart record ", file_ptr%time_index, "/", &
                  file_ptr%restart_nlevels, " at t=", current_time
      end if

      file_ptr%time_index = file_ptr%time_index + 1

   end subroutine write_restart_record

   !---------------------------------------------------------------------------
   !> @brief Check if it's time to write
   !---------------------------------------------------------------------------
   function is_output_time(current_time, freq) result(is_time)
      real, intent(in) :: current_time, freq
      logical :: is_time

      if (freq <= 0.0) then
         is_time = .false.
      else if (abs(current_time) < IO_TIME_TOLERANCE) then
         is_time = .true.  ! Always write at t=0
      else
         is_time = (abs(mod(current_time, freq)) < IO_TIME_TOLERANCE)
      end if
   end function is_output_time

   !---------------------------------------------------------------------------
   !> @brief Write averaged data for all variables in a file
   !---------------------------------------------------------------------------
   subroutine write_file_averaged(file_ptr, current_time)
      type(output_file_def), pointer, intent(inout) :: file_ptr
      real, intent(in) :: current_time

      integer :: i, var_idx, status
      type(io_variable), pointer :: var_ptr
      type(avg_state), pointer :: state_ptr
      logical :: any_written

      any_written = .false.

      do i = 1, file_ptr%num_variables
         var_idx = var_registry%find(file_ptr%variables(i))
         if (var_idx < 0) cycle

         var_ptr => var_registry%get_ptr(var_idx)
         if (.not. associated(var_ptr)) cycle

         state_ptr => file_ptr%get_avg_state(var_ptr%meta%name)
         if (.not. associated(state_ptr)) cycle
         if (.not. state_ptr%is_ready()) cycle

         status = nc_write_avg_data(file_ptr%ncid, file_ptr%time_index, &
                                    var_ptr, state_ptr)
         if (status == 0) any_written = .true.
      end do

      if (any_written) then
         status = nc_write_time(file_ptr%ncid, file_ptr%time_varid, &
                                file_ptr%time_index, current_time)
         call file_ptr%increment_time()
         
         ! Periodic flush
         call maybe_flush(file_ptr)
         
         if (io_verbose >= IO_DEBUG) then
            print *, "  Written average: ", trim(file_ptr%name), " t=", current_time
         end if
      end if
   end subroutine write_file_averaged

   !---------------------------------------------------------------------------
   !> @brief Reset averaging states for all variables in a file
   !---------------------------------------------------------------------------
   subroutine reset_file_averaging(file_ptr)
      type(output_file_def), pointer, intent(inout) :: file_ptr

      integer :: i
      type(avg_state), pointer :: state_ptr

      do i = 1, file_ptr%num_variables
         state_ptr => file_ptr%get_avg_state(file_ptr%variables(i))
         if (associated(state_ptr)) then
            call state_ptr%reset()
         end if
      end do
   end subroutine reset_file_averaging

   !---------------------------------------------------------------------------
   !> @brief Flush file to disk if flush frequency is reached
   !>
   !> This ensures data is written to disk periodically, protecting against
   !> data loss in case of crash during long simulations.
   !---------------------------------------------------------------------------
   subroutine maybe_flush(file_ptr)
      type(output_file_def), pointer, intent(in) :: file_ptr
      
      integer :: status
      
      if (io_flush_freq <= 0) return
      if (file_ptr%ncid <= 0) return
      
      if (mod(file_ptr%time_index - 1, io_flush_freq) == 0) then
         status = nf90_sync(file_ptr%ncid)
         if (io_verbose >= IO_DEBUG) then
            print *, "  Flushed: ", trim(file_ptr%name)
         end if
      end if
   end subroutine maybe_flush

   !---------------------------------------------------------------------------
   !> @brief Process instant variable - write directly to files (no buffer)
   !>
   !> For each INSTANT file containing this variable, if it's time to write,
   !> write the data directly. NO MEMORY DUPLICATION for instant outputs!
   !---------------------------------------------------------------------------
   subroutine process_var_instant(var_name, current_time, scalar, d1, d2, d3)
      character(len=*), intent(in) :: var_name
      real, intent(in) :: current_time
      real, intent(in), optional :: scalar
      real, intent(in), optional :: d1(:)
      real, intent(in), optional :: d2(:,:)
      real, intent(in), optional :: d3(:,:,:)

      integer :: file_idx, j, status
      type(output_file_def), pointer :: file_ptr
      logical :: var_in_file

      ! Files must be created before calling this (via ensure_files_created)
      if (.not. files_created) return

      ! Process each file
      do file_idx = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle
         
         ! Skip non-instant files
         if (file_ptr%needs_averaging()) cycle
         if (file_ptr%is_restart) cycle
         
         ! Check if variable is in this file
         var_in_file = .false.
         do j = 1, file_ptr%num_variables
            if (trim(file_ptr%variables(j)) == trim(var_name)) then
               var_in_file = .true.
               exit
            end if
         end do
         if (.not. var_in_file) cycle
         
         ! Check if it's time to write
         if (.not. is_output_time(current_time, file_ptr%frequency)) cycle
         
         ! Write directly (no buffer!)
         if (present(scalar)) then
            status = nc_write_direct_0d(file_ptr%ncid, var_name, file_ptr%time_index, scalar)
         else if (present(d1)) then
            status = nc_write_direct_1d(file_ptr%ncid, var_name, file_ptr%time_index, d1)
         else if (present(d2)) then
            status = nc_write_direct_2d(file_ptr%ncid, var_name, file_ptr%time_index, d2)
         else if (present(d3)) then
            status = nc_write_direct_3d(file_ptr%ncid, var_name, file_ptr%time_index, d3)
         end if
         
         ! Mark that we wrote data to this file
         file_ptr%data_written_this_step = .true.
         
      end do

   end subroutine process_var_instant

   !---------------------------------------------------------------------------
   !> @brief Process average variable - accumulate in file buffers
   !>
   !> For each AVERAGE file containing this variable, accumulate the data.
   !---------------------------------------------------------------------------
   subroutine process_var_average(var_name, scalar, d1, d2, d3)
      character(len=*), intent(in) :: var_name
      real, intent(in), optional :: scalar
      real, intent(in), optional :: d1(:)
      real, intent(in), optional :: d2(:,:)
      real, intent(in), optional :: d3(:,:,:)

      integer :: file_idx, j, ndims
      type(output_file_def), pointer :: file_ptr
      type(avg_state), pointer :: state_ptr
      logical :: var_in_file
      integer :: shp(3)

      ! Determine dimensions from provided data
      ndims = 0
      shp = 0
      if (present(scalar)) then
         ndims = 0
      else if (present(d1)) then
         ndims = 1
         shp(1) = size(d1)
      else if (present(d2)) then
         ndims = 2
         shp(1) = size(d2, 1)
         shp(2) = size(d2, 2)
      else if (present(d3)) then
         ndims = 3
         shp(1) = size(d3, 1)
         shp(2) = size(d3, 2)
         shp(3) = size(d3, 3)
      end if

      ! Process each file
      do file_idx = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle
         
         ! Only process averaging files
         if (.not. file_ptr%needs_averaging()) cycle
         
         ! Check if variable is in this file
         var_in_file = .false.
         do j = 1, file_ptr%num_variables
            if (trim(file_ptr%variables(j)) == trim(var_name)) then
               var_in_file = .true.
               exit
            end if
         end do
         if (.not. var_in_file) cycle
         
         ! Get or create averaging state
         state_ptr => file_ptr%get_avg_state(var_name)
         if (.not. associated(state_ptr)) cycle
         
         ! Initialize if needed with shape from actual data
         if (.not. state_ptr%initialized) then
            call state_ptr%init(var_name, file_ptr%operation, ndims, shp)
         end if
         
         ! Accumulate
         if (present(scalar)) then
            call state_ptr%accumulate_scalar(scalar)
         else if (present(d1)) then
            call state_ptr%accumulate_1d(d1)
         else if (present(d2)) then
            call state_ptr%accumulate_2d(d2)
         else if (present(d3)) then
            call state_ptr%accumulate_3d(d3)
         end if
         
      end do

   end subroutine process_var_average

end module io_manager
