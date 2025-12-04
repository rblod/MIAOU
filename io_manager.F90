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
                        get_calendar, file_registry
   use io_file_registry, only: output_file_def, output_file_registry, avg_state, &
                               OP_INSTANT, OP_AVERAGE, OP_MIN, OP_MAX, OP_ACCUMULATE
   use io_naming, only: generate_filename
   use io_netcdf, only: nc_initialize, nc_finalize, nc_create_file, &
                        nc_write_variable_data, nc_write_time, &
                        nc_close_file, nc_define_variable_in_file, &
                        nc_end_definition, nc_write_avg_data, &
                        nc_write_direct_0d, nc_write_direct_1d, &
                        nc_write_direct_2d, nc_write_direct_3d

   implicit none
   private

   ! Public interface
   public :: initialize_io, finalize_io
   public :: register_variable
   public :: write_output
   public :: get_variable_ptr
   public :: process_var_instant, process_var_average
   public :: ensure_files_created

   ! Variable registry
   type(io_var_registry), public, target :: var_registry

   ! Module state
   logical, private :: is_initialized = .false.
   logical, private :: files_created = .false.

contains

   !---------------------------------------------------------------------------
   !> @brief Initialize the I/O system
   !>
   !> @param[in] config_file  Optional path to configuration file
   !> @return    Status (0 = success)
   !---------------------------------------------------------------------------
   function initialize_io(config_file) result(status)
      character(len=*), intent(in), optional :: config_file
      integer :: status

      status = 0

      ! Initialize NetCDF backend
      call nc_initialize()

      ! Read configuration
      if (present(config_file)) then
         status = read_io_config(config_file)
      else
         status = read_io_config("output_config.nml")
      end if

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
               status = nc_close_file(file_ptr%backend_id)
               file_ptr%backend_id = -1
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

      ! Process each file
      do file_idx = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle

         call process_file(file_ptr, current_time, final_step)
      end do

   end function write_output

   !---------------------------------------------------------------------------
   !> @brief Ensure all output files are created
   !>
   !> Call this before send_all_outputs to ensure files exist.
   !---------------------------------------------------------------------------
   subroutine ensure_files_created()
      if (.not. files_created) then
         call create_all_files()
         files_created = .true.
      end if
   end subroutine ensure_files_created

   !---------------------------------------------------------------------------
   !> @brief Check if a variable is in any restart file
   !>
   !> Returns true if the variable is referenced by at least one restart file.
   !> Used to determine if we need to store the variable in var_registry buffer.
   !---------------------------------------------------------------------------
   function is_restart_variable(var_name) result(is_restart)
      character(len=*), intent(in) :: var_name
      logical :: is_restart

      integer :: file_idx, j
      type(output_file_def), pointer :: file_ptr

      is_restart = .false.

      do file_idx = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle
         if (.not. file_ptr%is_restart) cycle

         ! Check if variable is in this restart file
         do j = 1, file_ptr%num_variables
            if (trim(file_ptr%variables(j)) == trim(var_name)) then
               is_restart = .true.
               return
            end if
         end do
      end do

   end function is_restart_variable

   !---------------------------------------------------------------------------
   !> @brief Create all output files
   !---------------------------------------------------------------------------
   subroutine create_all_files()
      integer :: file_idx, status
      type(output_file_def), pointer :: file_ptr
      character(len=IO_PATH_LEN) :: filename

      ! First validate configuration
      call validate_config()

      do file_idx = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle

         ! Generate filename
         filename = generate_filename(file_ptr%prefix, file_ptr%name, file_ptr%frequency)
         file_ptr%filename = filename

         ! Create the file
         status = nc_create_file(filename, file_ptr%name, file_ptr%frequency, &
                                 get_time_units(), get_calendar(), &
                                 file_ptr%backend_id, file_ptr%time_dimid, &
                                 file_ptr%time_varid)

         if (status /= 0) then
            print *, "ERROR: Failed to create file: ", trim(filename)
            cycle
         end if

         ! Define variables in file
         call define_variables_in_file(file_ptr)

         ! End definition mode
         status = nc_end_definition(file_ptr%backend_id)

         if (io_verbose >= IO_NORMAL) then
            print *, "Created file: ", trim(filename)
         end if
      end do
   end subroutine create_all_files

   !---------------------------------------------------------------------------
   !> @brief Validate configuration: check that all referenced variables exist
   !>
   !> Prints warnings for variables referenced in file definitions but not
   !> registered in the variable registry.
   !---------------------------------------------------------------------------
   subroutine validate_config()
      integer :: file_idx, var_idx, j
      type(output_file_def), pointer :: file_ptr
      character(len=32) :: var_name
      integer :: missing_count, total_refs

      missing_count = 0
      total_refs = 0

      if (io_verbose >= IO_NORMAL) then
         print *, ""
         print *, "Validating I/O configuration..."
      end if

      do file_idx = 1, file_registry%size()
         file_ptr => file_registry%get_ptr(file_idx)
         if (.not. associated(file_ptr)) cycle

         do j = 1, file_ptr%num_variables
            var_name = file_ptr%variables(j)
            total_refs = total_refs + 1

            var_idx = var_registry%find(var_name)
            if (var_idx < 0) then
               print *, "  WARNING: Variable '", trim(var_name), &
                        "' in file '", trim(file_ptr%name), "' is not registered"
               missing_count = missing_count + 1
            end if
         end do
      end do

      if (io_verbose >= IO_NORMAL) then
         if (missing_count == 0) then
            print *, "  All ", total_refs, " variable references are valid"
         else
            print *, "  Found ", missing_count, " missing variables out of ", total_refs, " references"
            print *, "  (Missing variables will be skipped during output)"
         end if
         print *, ""
      end if

   end subroutine validate_config

   !---------------------------------------------------------------------------
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
            print *, "Warning: Variable ", trim(var_name), " not registered, skipping"
            cycle
         end if

         var_ptr => var_registry%get_ptr(var_idx)
         if (.not. associated(var_ptr)) cycle

         ! Define in file
         status = nc_define_variable_in_file(file_ptr%backend_id, file_ptr%time_dimid, var_ptr)
         if (status /= 0) then
            print *, "Warning: Failed to define variable ", trim(var_name)
         end if
      end do
      
      ! Initialize avg_states array (but not the buffers - done later with actual data)
      if (file_ptr%needs_averaging()) then
         call file_ptr%init_avg_states()
      end if
   end subroutine define_variables_in_file

   !---------------------------------------------------------------------------
   !> @brief Initialize averaging state for a variable
   !---------------------------------------------------------------------------
   subroutine init_avg_state_for_var(file_ptr, var_ptr)
      type(output_file_def), pointer, intent(inout) :: file_ptr
      type(io_variable), pointer, intent(in) :: var_ptr

      type(avg_state), pointer :: state_ptr
      integer :: shp(3)

      state_ptr => file_ptr%get_avg_state(var_ptr%meta%name)
      if (.not. associated(state_ptr)) return
      if (state_ptr%initialized) return

      ! Get data shape and initialize
      call var_ptr%data%get_shape(var_ptr%meta%ndims, shp)
      call state_ptr%init(var_ptr%meta%name, file_ptr%operation, var_ptr%meta%ndims, shp)
   end subroutine init_avg_state_for_var

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
            status = nc_write_time(file_ptr%backend_id, file_ptr%time_varid, &
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
      integer :: status
      
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
         if (file_ptr%time_index > file_ptr%restart_nlevels) then
            ! Reopen file in overwrite mode
            if (file_ptr%backend_id > 0) then
               status = nc_close_file(file_ptr%backend_id)
            end if
            call recreate_restart_file(file_ptr)
         end if
         
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
                              file_ptr%backend_id, file_ptr%time_dimid, &
                              file_ptr%time_varid)

      if (status /= 0) then
         print *, "ERROR: Failed to recreate restart file"
         return
      end if

      ! Define variables
      do i = 1, file_ptr%num_variables
         var_idx = var_registry%find(file_ptr%variables(i))
         if (var_idx < 0) cycle

         var_ptr => var_registry%get_ptr(var_idx)
         if (.not. associated(var_ptr)) cycle

         status = nc_define_variable_in_file(file_ptr%backend_id, file_ptr%time_dimid, var_ptr)
      end do

      status = nc_end_definition(file_ptr%backend_id)
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

         status = nc_write_variable_data(file_ptr%backend_id, file_ptr%time_index, var_ptr)
      end do

      ! Write time
      status = nc_write_time(file_ptr%backend_id, file_ptr%time_varid, &
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
   !> @brief Accumulate data for all variables in an averaging file
   !---------------------------------------------------------------------------
   subroutine accumulate_file_data(file_ptr)
      type(output_file_def), pointer, intent(inout) :: file_ptr

      integer :: i, var_idx
      type(io_variable), pointer :: var_ptr
      type(avg_state), pointer :: state_ptr

      do i = 1, file_ptr%num_variables
         var_idx = var_registry%find(file_ptr%variables(i))
         if (var_idx < 0) cycle

         var_ptr => var_registry%get_ptr(var_idx)
         if (.not. associated(var_ptr)) cycle

         state_ptr => file_ptr%get_avg_state(var_ptr%meta%name)
         if (.not. associated(state_ptr)) cycle

         ! Initialize if not done
         if (.not. state_ptr%initialized) then
            call init_avg_state_for_var(file_ptr, var_ptr)
         end if

         ! Accumulate based on dimensionality
         ! Uses buffers if use_buffer is true, otherwise pointers
         select case (var_ptr%meta%ndims)
         case (0)
            if (var_ptr%data%is_valid(0)) then
               if (var_ptr%data%use_buffer) then
                  call state_ptr%accumulate_scalar(var_ptr%data%buf_scalar)
               else
                  call state_ptr%accumulate_scalar(var_ptr%data%scalar)
               end if
            end if
         case (1)
            if (var_ptr%data%is_valid(1)) then
               if (var_ptr%data%use_buffer) then
                  call state_ptr%accumulate_1d(var_ptr%data%buf_1d)
               else
                  call state_ptr%accumulate_1d(var_ptr%data%d1)
               end if
            end if
         case (2)
            if (var_ptr%data%is_valid(2)) then
               if (var_ptr%data%use_buffer) then
                  call state_ptr%accumulate_2d(var_ptr%data%buf_2d)
               else
                  call state_ptr%accumulate_2d(var_ptr%data%d2)
               end if
            end if
         case (3)
            if (var_ptr%data%is_valid(3)) then
               if (var_ptr%data%use_buffer) then
                  call state_ptr%accumulate_3d(var_ptr%data%buf_3d)
               else
                  call state_ptr%accumulate_3d(var_ptr%data%d3)
               end if
            end if
         end select
      end do
   end subroutine accumulate_file_data

   !---------------------------------------------------------------------------
   !> @brief Write instantaneous data for all variables in a file
   !---------------------------------------------------------------------------
   subroutine write_file_instant(file_ptr, current_time)
      type(output_file_def), pointer, intent(inout) :: file_ptr
      real, intent(in) :: current_time

      integer :: i, var_idx, status
      type(io_variable), pointer :: var_ptr
      logical :: any_written

      any_written = .false.

      do i = 1, file_ptr%num_variables
         var_idx = var_registry%find(file_ptr%variables(i))
         if (var_idx < 0) cycle

         var_ptr => var_registry%get_ptr(var_idx)
         if (.not. associated(var_ptr)) cycle

         status = nc_write_variable_data(file_ptr%backend_id, file_ptr%time_index, var_ptr)
         if (status == 0) any_written = .true.
      end do

      if (any_written) then
         status = nc_write_time(file_ptr%backend_id, file_ptr%time_varid, &
                                file_ptr%time_index, current_time)
         call file_ptr%increment_time()
         
         ! Periodic flush
         call maybe_flush(file_ptr)
         
         if (io_verbose >= IO_DEBUG) then
            print *, "  Written instant: ", trim(file_ptr%name), " t=", current_time
         end if
      end if
   end subroutine write_file_instant

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

         status = nc_write_avg_data(file_ptr%backend_id, file_ptr%time_index, &
                                    var_ptr, state_ptr)
         if (status == 0) any_written = .true.
      end do

      if (any_written) then
         status = nc_write_time(file_ptr%backend_id, file_ptr%time_varid, &
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
   !> @brief Write restart file with N time levels
   !>
   !> This creates/overwrites the restart file with the last N time levels.
   !> The data is read from the model variables at the current time - we 
   !> assume the model stores previous time levels internally.
   !>
   !> Note: For a proper multi-level restart, the model must provide access
   !> to previous time level data. This implementation writes the current
   !> state N times with the stored time values.
   !---------------------------------------------------------------------------
   subroutine write_restart_file(file_ptr, current_time)
      type(output_file_def), pointer, intent(inout) :: file_ptr
      real, intent(in) :: current_time

      integer :: i, j, var_idx, status, level
      integer :: start_idx, nlevels_to_write
      type(io_variable), pointer :: var_ptr
      real :: time_val

      ! Close existing file if open (we overwrite)
      if (file_ptr%backend_id > 0) then
         status = nc_close_file(file_ptr%backend_id)
         file_ptr%backend_id = -1
      end if

      ! Recreate the file
      status = nc_create_file(file_ptr%filename, file_ptr%name, file_ptr%frequency, &
                              get_time_units(), get_calendar(), &
                              file_ptr%backend_id, file_ptr%time_dimid, &
                              file_ptr%time_varid)

      if (status /= 0) then
         print *, "ERROR: Failed to create restart file: ", trim(file_ptr%filename)
         return
      end if

      ! Define variables
      do i = 1, file_ptr%num_variables
         var_idx = var_registry%find(file_ptr%variables(i))
         if (var_idx < 0) cycle

         var_ptr => var_registry%get_ptr(var_idx)
         if (.not. associated(var_ptr)) cycle

         status = nc_define_variable_in_file(file_ptr%backend_id, file_ptr%time_dimid, var_ptr)
      end do

      status = nc_end_definition(file_ptr%backend_id)

      ! Determine how many levels to write
      nlevels_to_write = min(file_ptr%restart_buffer_count, file_ptr%restart_nlevels)
      
      if (nlevels_to_write == 0) then
         print *, "WARNING: No time levels stored for restart"
         return
      end if

      ! Write each time level
      ! Buffer is circular: find starting index for oldest data to write
      if (file_ptr%restart_buffer_count >= file_ptr%restart_nlevels) then
         ! Buffer is full, start from oldest
         start_idx = mod(file_ptr%restart_buffer_idx, file_ptr%restart_nlevels) + 1
      else
         ! Buffer not full, start from 1
         start_idx = 1
      end if

      file_ptr%time_index = 1
      
      do level = 1, nlevels_to_write
         ! Get time value from circular buffer
         j = mod(start_idx + level - 2, file_ptr%restart_nlevels) + 1
         time_val = file_ptr%restart_times(j)

         ! Write all variables at this time index
         ! Note: This writes current model state - for true multi-level restart,
         ! the model should provide access to previous time level arrays
         do i = 1, file_ptr%num_variables
            var_idx = var_registry%find(file_ptr%variables(i))
            if (var_idx < 0) cycle

            var_ptr => var_registry%get_ptr(var_idx)
            if (.not. associated(var_ptr)) cycle

            status = nc_write_variable_data(file_ptr%backend_id, file_ptr%time_index, var_ptr)
         end do

         ! Write time
         status = nc_write_time(file_ptr%backend_id, file_ptr%time_varid, &
                                file_ptr%time_index, time_val)
         file_ptr%time_index = file_ptr%time_index + 1
      end do

      if (io_verbose >= IO_NORMAL) then
         print *, "Written restart: ", trim(file_ptr%filename), " (", nlevels_to_write, " levels)"
      end if

   end subroutine write_restart_file

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
      if (file_ptr%backend_id <= 0) return
      
      if (mod(file_ptr%time_index - 1, io_flush_freq) == 0) then
         status = nf90_sync(file_ptr%backend_id)
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
            status = nc_write_direct_0d(file_ptr%backend_id, var_name, file_ptr%time_index, scalar)
         else if (present(d1)) then
            status = nc_write_direct_1d(file_ptr%backend_id, var_name, file_ptr%time_index, d1)
         else if (present(d2)) then
            status = nc_write_direct_2d(file_ptr%backend_id, var_name, file_ptr%time_index, d2)
         else if (present(d3)) then
            status = nc_write_direct_3d(file_ptr%backend_id, var_name, file_ptr%time_index, d3)
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
